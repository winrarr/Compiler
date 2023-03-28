(**************************************************************************)
(* AU compilation.                                                        *)
(* Skeleton file -- expected to be modified as part of the assignment     *)
(**************************************************************************)

(* ll ir compilation -------------------------------------------------------- *)

open Tigercommon
module S = Symbol
open X86
open Ll
open Asm

exception BackendFatal (* use this for impossible cases *)

type os = Linux | Darwin

let os =
  let ic = Unix.open_process_in "uname" in
  let uname = input_line ic in
  let () = close_in ic in
  match uname with
  | "Linux" -> Linux
  | "Darwin" -> Darwin
  | _ -> raise BackendFatal

let mangle s =
  match os with Linux -> Symbol.name s | Darwin -> "_" ^ Symbol.name s

let compile_cnd (c : Ll.cnd) : X86.cnd =
  match c with
  | Ll.Eq -> X86.Eq
  | Ll.Ne -> X86.Neq
  | Ll.Slt -> X86.Lt
  | Ll.Sle -> X86.Le
  | Ll.Sgt -> X86.Gt
  | Ll.Sge -> X86.Ge

type layout = (Ll.uid * X86.operand) list

type ctxt = {tdecls: (Ll.tid * Ll.ty) list; layout: layout}

let lookup m x = List.assoc x m

let compile_operand ({layout; _} : ctxt) (operand_x86 : X86.operand)
    (operand_ll : Ll.operand) : ins =
  match operand_ll with
  | Null -> (Movq, [~$0; operand_x86])
  | Const i -> (Movq, [~$i; operand_x86])
  | Gid gid -> (Leaq, [Ind3 (Lbl (mangle gid), Rip); operand_x86])
  | Id uid -> (Movq, [lookup layout uid; operand_x86])

let arg_registers = [~%Rdi; ~%Rsi; ~%Rdx; ~%Rcx; ~%R08; ~%R09]

let combine_args_regs args =
  let rec trav args regs offset =
    match (args, regs) with
    | uid :: xs, reg :: ys -> (uid, reg) :: trav xs ys offset
    | uid :: xs, [] ->
        (uid, Ind3 (Lit offset, Rbp)) :: trav xs [] (offset + 8)
    | [], _ -> []
  in
  trav args arg_registers 16

let compile_call ctxt operand (lst : (ty * Ll.operand) list) : ins list =
  let operand_lst = snd (List.split lst) in
  let args_regs = combine_args_regs operand_lst in
  let move_arg (llop, operand) =
    match operand with
    | Reg _ -> [compile_operand ctxt operand llop]
    | _ -> [(Pushq, [~%R11]); compile_operand ctxt ~%R11 llop]
  in
  let args_insns = List.concat_map move_arg args_regs in
  let pop_insns =
    [(Addq, [~$(max 0 (List.length operand_lst - 6) * 8); ~%Rsp])]
  in
  List.rev args_insns
  @ [compile_operand ctxt ~%R12 operand; (Callq, [~%R12])]
  @ pop_insns

let size_of_ty (named_tys : (uid * ty) list) (ty : Ll.ty) : quad =
  let rec size_one_ty (ty : Ll.ty) : quad =
    match ty with
    | Void | I8 | Fun _ -> 0
    | I1 | I64 | Ptr _ -> 8
    | Struct ty_lst ->
        List.fold_left (fun acc ty -> acc + size_one_ty ty) 0 ty_lst
    | Array (size, array_ty) -> size * size_one_ty array_ty
    | Namedt tid -> size_one_ty (lookup named_tys tid)
  in
  size_one_ty ty

let compile_gep ({tdecls; _} as ctxt : ctxt) ((ty, base) : ty * Ll.operand)
    (indices : Ll.operand list) : ins list =
  let base_ptr_in_r12 = [compile_operand ctxt ~%R12 base] in
  let size = size_of_ty tdecls ty in
  let offset_in_r11 =
    [compile_operand ctxt ~%R11 (List.hd indices); (Imulq, [~$size; ~%R11])]
  in
  let compile_index index =
    [ compile_operand ctxt ~%R13 index
    ; (Imulq, [~$8; ~%R13])
    ; (Addq, [~%R13; ~%R11]) ]
  in
  let compiled_indices = List.concat_map compile_index (List.tl indices) in
  offset_in_r11 @ compiled_indices @ base_ptr_in_r12
  @ [(Addq, [~%R12; ~%R11])]

let compile_insn ({tdecls; layout} as ctxt : ctxt)
    ((uid, insn) : uid option * insn) : ins list =
  let insns =
    match uid with
    | Some uid -> (
        let res_operand = lookup layout uid in
        match insn with
        | Alloca ty ->
            let size = size_of_ty tdecls ty in
            [(Subq, [~$size; ~%Rsp]); (Movq, [~%Rsp; res_operand])]
        | Binop (bop, _, left, right) -> (
          match bop with
          | Add | Sub | Mul | Xor | Or | And ->
              let opcode =
                match bop with
                | Add -> Addq
                | Sub -> Subq
                | Mul -> Imulq
                | Xor -> Xorq
                | Or -> Orq
                | And -> Andq
                | _ -> raise BackendFatal
              in
              [ compile_operand ctxt ~%R11 left
              ; compile_operand ctxt ~%R12 right
              ; (opcode, [~%R12; ~%R11])
              ; (Movq, [~%R11; res_operand]) ]
          | SDiv ->
              [ compile_operand ctxt ~%Rax left
              ; compile_operand ctxt ~%Rbx right
              ; (Cqto, [])
              ; (Idivq, [~%Rbx])
              ; (Movq, [~%Rax; res_operand]) ]
          | Shl | Lshr | Ashr ->
              let opcode =
                match bop with
                | Shl -> Shlq
                | Lshr -> Shrq
                | Ashr -> Sarq
                | _ -> raise BackendFatal
              in
              [ compile_operand ctxt ~%R11 left
              ; compile_operand ctxt ~%Rcx right
              ; (opcode, [~%Rcx; ~%R11])
              ; (Movq, [~%R11; res_operand]) ] )
        | Load (_, operand) ->
            [ compile_operand ctxt ~%R11 operand
            ; (Movq, [Ind2 R11; ~%R12])
            ; (Movq, [~%R12; res_operand]) ]
        | Icmp (cnd, _, left, right) ->
            [ compile_operand ctxt ~%R11 left
            ; compile_operand ctxt ~%R12 right
            ; (Cmpq, [~%R12; ~%R11])
            ; (Movq, [~$0; res_operand])
            ; (Set (compile_cnd cnd), [res_operand]) ]
        | Call (_, operand, lst) ->
            compile_call ctxt operand lst @ [(Movq, [~%Rax; res_operand])]
        | Gep (ty, left, right) ->
            compile_gep ctxt (ty, left) right @ [(Movq, [~%R11; res_operand])]
        | Bitcast (_, operand, _)
         |Zext (_, operand, _)
         |Ptrtoint (_, operand, _) ->
            [compile_operand ctxt ~%R11 operand; (Movq, [~%R11; res_operand])]
        | Comment _ -> []
        | Store _ -> raise BackendFatal )
    | None -> (
      match insn with
      | Store (_, left, right) ->
          [ compile_operand ctxt ~%R11 left
          ; compile_operand ctxt ~%R12 right
          ; (Movq, [~%R11; Ind2 R12]) ]
      | Call (_, operand, lst) -> compile_call ctxt operand lst
      | Comment _ -> []
      | _ -> raise BackendFatal )
  in
  insns

let compile_terminator ({layout; _} as ctxt : ctxt) (terminator : terminator)
    : ins list =
  match terminator with
  | Ret (ty, operand_opt) -> (
    match (operand_opt, ty) with
    | Some operand, _ ->
        [ compile_operand ctxt ~%Rax operand
        ; (Movq, [~%Rbp; ~%Rsp])
        ; (Popq, [~%Rbp])
        ; (Retq, []) ]
    | None, Void ->
        [ (Xorq, [~%Rax; ~%Rax])
        ; (Movq, [~%Rbp; ~%Rsp])
        ; (Popq, [~%Rbp])
        ; (Retq, []) ]
    | None, _ -> [(Movq, [~%Rbp; ~%Rsp]); (Popq, [~%Rbp]); (Retq, [])] )
  | Br lbl ->
      let operand = lookup layout lbl in
      [(Jmp, [operand])]
  | Cbr (operand, lbl1, lbl2) ->
      [ compile_operand ctxt ~%R11 operand
      ; (Cmpq, [~$1; ~%R11])
      ; (J Eq, [lookup layout lbl1])
      ; (Jmp, [lookup layout lbl2]) ]

let compile_block ctxt {insns; terminator} =
  let insns_without_terminator = List.concat_map (compile_insn ctxt) insns in
  insns_without_terminator @ compile_terminator ctxt terminator

let compile_lbl_block lbl ctxt block =
  {lbl= mangle lbl; global= false; asm= Text (compile_block ctxt block)}

let make_prologue (layout : layout) (params : uid list) stack_offset :
    ins list =
  let args_regs = combine_args_regs params in
  let move_arg (uid, operand) =
    match operand with
    | Reg _ -> [(Movq, [operand; lookup layout uid])]
    | _ -> [(Movq, [operand; ~%R11]); (Movq, [~%R11; lookup layout uid])]
  in
  let args_insns = List.concat_map move_arg args_regs in
  [(Pushq, [~%Rbp]); (Movq, [~%Rsp; ~%Rbp])]
  @ [(Subq, [~$(abs stack_offset - 8); ~%Rsp])]
  @ args_insns

let add_block_to_layout curr {insns; _} =
  List.fold_left
    (fun (layout, offset) insn ->
      match fst insn with
      | Some uid -> ((uid, Ind3 (Lit offset, Rbp)) :: layout, offset - 8)
      | _ -> (layout, offset) )
    curr insns

let make_layout params (entry_block, labeled_blocks_lst) =
  let args =
    List.fold_left
      (fun (layout, offset) (param : uid) ->
        ((param, Ind3 (Lit offset, Rbp)) :: layout, offset - 8) )
      ([], -8) params
  in
  let args_entry = add_block_to_layout args entry_block in
  let labels_lst, blocks_lst = List.split labeled_blocks_lst in
  let args_entry_labels =
    ( List.map (fun lbl -> (lbl, ~$$(S.name lbl))) labels_lst @ fst args_entry
    , snd args_entry )
  in
  let final =
    List.fold_left add_block_to_layout args_entry_labels blocks_lst
  in
  final

let compile_fdecl tdecls (name : uid)
    ({param; cfg= entry_block, labeled_blocks_lst; _} : fdecl) : elem list =
  let layout, stack_offset =
    make_layout param (entry_block, labeled_blocks_lst)
  in
  let ctxt = {tdecls; layout} in
  let entry_block_insns = compile_block ctxt entry_block in
  let labeled_elem_lst =
    List.map
      (fun (lbl, block) -> compile_lbl_block lbl ctxt block)
      labeled_blocks_lst
  in
  [ { lbl= mangle name
    ; global= true
    ; asm= Text (make_prologue layout param stack_offset @ entry_block_insns)
    } ]
  @ labeled_elem_lst

let rec compile_ginit = function
  | GNull -> [Quad (Lit 0)]
  | GGid gid -> [Quad (Lbl (mangle gid))]
  | GInt c -> [Quad (Lit c)]
  | GString s -> [Asciz s]
  | GArray gs -> List.concat (List.map compile_gdecl gs)
  | GStruct gs -> List.concat (List.map compile_gdecl gs)

and compile_gdecl (_, g) = compile_ginit g

let compile_prog ({tdecls; gdecls; fdecls} : Ll.prog) : X86.prog =
  let g (lbl, gdecl) = Asm.data (mangle lbl) (compile_gdecl gdecl) in
  let f (name, fdecl) = compile_fdecl tdecls name fdecl in
  List.map g gdecls @ List.concat (List.map f fdecls)