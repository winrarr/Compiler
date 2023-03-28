(**************************************************************************)
(* AU compilation.                                                        *)
(* Skeleton file -- expected to be modified as part of the assignment     *)
(**************************************************************************)

open Tigercommon
open Tigerhoist
module H = Habsyn
module Ty = Types
module S = Symbol
module B = Cfgbuilder

module SymbolMap = Map.Make (struct
  type t = S.symbol

  let compare = compare
end)

module UniqueMap = Map.Make (struct
  type t = Ty.unique

  let compare = compare
end)

type unique_env = Ll.tid UniqueMap.t

type fdecl_summary =
  { parent_opt: Ll.gid option
  ; locals_uid: Ll.uid
  ; locals_tid: Ll.tid
  ; offset_of_symbol: S.symbol -> int }

type summary_env = fdecl_summary SymbolMap.t

exception CodeGenerationBug

let rec actual_type = function
  | Ty.NAME (_, result) -> (
    match !result with
    | Some ty -> actual_type ty
    | None -> raise CodeGenerationBug )
  | ty -> ty

let ptr_i8 = Ll.Ptr Ll.I8

let ty_to_llty ty =
  match actual_type ty with
  | Ty.INT -> Ll.I64
  | Ty.STRING -> ptr_i8
  | Ty.VOID -> Ll.Void
  | Ty.NIL -> ptr_i8
  | Ty.RECORD _ -> ptr_i8
  | Ty.ERROR -> raise CodeGenerationBug
  | Ty.NAME _ -> raise CodeGenerationBug
  | Ty.ARRAY _ -> ptr_i8

type context =
  { break_lbl: Ll.lbl option
  ; summary: fdecl_summary
  ; uenv: unique_env
  ; senv: summary_env
  ; gdecls: (Ll.gid * Ll.gdecl) list ref }

(* Obs: this is a rather tricky piece of code; 2019-10-12 *)
let cg_tydecl (tenv : unique_env ref) (H.Tdecl {name; ty; _}) =
  match actual_type ty with
  | Ty.RECORD (lst, unique) -> (
    match UniqueMap.find_opt unique !tenv with
    | None ->
        let ll_ty_list =
          List.map (fun ((_, ty) : _ * Ty.ty) -> ty_to_llty ty) lst
        in
        tenv := UniqueMap.add unique name !tenv ;
        Some (name, Ll.Struct ll_ty_list)
    | Some _ -> None )
  | _ -> None

let fresh =
  let open Freshsymbols in
  let env = empty in
  gensym env

let ( <$> ) f g x = f (g x)

let id = Fun.id

let ( $> ) b1 b2 = b2 <$> b1 (* buildlet composition *)

let ( @> ) (b, op) k = b $> k op

let ty_of_exp (H.Exp {ty; _}) = ty

let ty_of_var (H.Var {ty; _}) = ty

let ty_of_arg (H.Arg {ty; _}) = ty

let name_of_arg (H.Arg {name; _}) = name

let default_sl_name = S.symbol "$sl"

let locals_type_name name = S.symbol @@ "$locals_" ^ S.name name

let ty_dec_name name = S.symbol @@ "t_$" ^ S.name name

(* add instruction with fresh symbol *)
let aiwf s i =
  let t = fresh s in
  (B.add_insn (Some t, i), Ll.Id t)

(* --- monadic interface;) ----------------------------- *)

(* Notes on the monadic interface:

   1) Monadic interface is not necessary at all, and one could just
      program with buildlets as before; it's just a little bit more
      concise, but one _really_ needs to know what they are doing.

   2) Some useful info on the OCmal let* notation here
      http://jobjo.github.io/2019/04/24/ocaml-has-some-new-shiny-syntax.html

   3) Observe that the new OCaml notation conflicts with JaneStreet
      pre-processors, so we don't have any pre-processing in this library.
*)

type 'a m = B.buildlet * 'a

let make_comment s =
  let comment_insn = Ll.Comment s in
  let b_comment_insn, _ = aiwf "comment" comment_insn in
  b_comment_insn

let bind ((builder, op) : 'a m) (f : 'a -> 'b m) : 'b m =
  let builder', op' = f op in
  (builder $> builder', op')

let return x = (B.id_buildlet, x)

let ( let* ) = bind
(* --- end of monadic interface ------------------------ *)

let unit b = (b, ())

let build_store t o1 o2 = B.add_insn (None, Ll.Store (t, o1, o2))

let gep_0 ty op i = Ll.Gep (ty, op, [Const 0; Const i])

let chase_sl (level_diff : int) (name : Ll.uid) (ctxt : context) =
  let rec chase_the_fucker op buildlets level_diff
      ({summary; senv; _} as ctxt : context) =
    if level_diff > 0 then
      match summary.parent_opt with
      | Some name ->
          let current_name = summary.locals_tid in
          let gep_insn = gep_0 (Namedt current_name) op 0 in
          let b_gep_insn, gep_op = aiwf "gep" gep_insn in
          let new_summary = SymbolMap.find name senv in
          let load_insn =
            Ll.Load (Ptr (Namedt new_summary.locals_tid), gep_op)
          in
          let b_load_insn, load_op = aiwf "load" load_insn in
          chase_the_fucker load_op
            (buildlets @ [b_gep_insn; b_load_insn])
            (level_diff - 1)
            {ctxt with summary= new_summary}
      | None -> chase_the_fucker op buildlets (level_diff - 1) ctxt
    else
      match SymbolMap.find_opt name senv with
      | Some _ -> (buildlets, op, Ll.Ptr (Namedt summary.locals_tid))
      | _ -> (
        match S.name name with
        | "print" | "flush" | "getChar" | "ord" | "chr" | "size"
         |"substring" | "concat" | "not" | "tigerexit" ->
            let gep_insn = gep_0 (Namedt summary.locals_tid) op 0 in
            let b_gep_insn, gep_op = aiwf "gep" gep_insn in
            let load_insn = Ll.Load (ptr_i8, gep_op) in
            let b_load_insn, load_op = aiwf "load" load_insn in
            (buildlets @ [b_gep_insn; b_load_insn], load_op, ptr_i8)
        | _ ->
            let gep_insn =
              gep_0 (Namedt summary.locals_tid) op
                (summary.offset_of_symbol name)
            in
            let b_gep_insn, gep_op = aiwf "gep" gep_insn in
            (buildlets @ [b_gep_insn], gep_op, Ll.Void) )
  in
  chase_the_fucker (Id ctxt.summary.locals_uid) [] level_diff ctxt

let rec find_index x lst =
  match lst with
  | [] -> raise CodeGenerationBug
  | h :: t -> if x = h then 0 else 1 + find_index x t

let rec cgExp (ctxt : context) (Exp {exp_base; ty; _} : H.exp) :
    B.buildlet * Ll.operand (* Alternatively: Ll.operand m *) =
  let cgE_ = cgExp ctxt in
  let open Ll in
  match exp_base with
  | VarExp var ->
      let var_b, var_op = cgVar ctxt var in
      let load_insn = Ll.Load (ty_to_llty ty, var_op) in
      let b_load_insn, load_op = aiwf "load" load_insn in
      (var_b $> b_load_insn, load_op)
  | NilExp -> (id, Null)
  | IntExp i -> (id, Const i)
  | StringExp s ->
      let s_len = String.length s in
      let typ = Struct [I64; Array (s_len, I8)] in
      let gdecl_list = [(I64, GInt s_len); (Array (s_len, I8), GString s)] in
      let global_init = GStruct gdecl_list in
      let gdecl = (typ, global_init) in
      let global_id = fresh "str" in
      let bitcast_insn = Bitcast (Ptr typ, Gid global_id, ptr_i8) in
      let b_bitcast_insn, bitcast_op = aiwf "bitcast" bitcast_insn in
      let new_gdecl = (global_id, gdecl) in
      let old_gdecls = !(ctxt.gdecls) in
      let new_gdecls = ref (new_gdecl :: old_gdecls) in
      ctxt.gdecls := !new_gdecls ;
      (b_bitcast_insn, bitcast_op)
  | CallExp {func; lvl_diff; args} -> (
      let some_tuple_list = List.map (get_ty_and_tuple ctxt) args in
      let tuple_args = List.map (fun (a, b) -> (a, snd b)) some_tuple_list in
      let args_buildlets = List.map (fun (_, b) -> fst b) some_tuple_list in
      let arg_buildlet = B.seq_buildlets args_buildlets in
      let b_chase_insns, chase_op, chase_ty = chase_sl lvl_diff func ctxt in
      let call_insn =
        Call (ty_to_llty ty, Gid func, (chase_ty, chase_op) :: tuple_args)
      in
      match ty with
      | Ty.VOID ->
          let b_call_insn = B.add_insn (None, call_insn) in
          ( arg_buildlet $> B.seq_buildlets b_chase_insns $> b_call_insn
          , Const 0 )
      | _ ->
          let b_call_insn, call_op = aiwf "call" call_insn in
          ( arg_buildlet $> B.seq_buildlets b_chase_insns $> b_call_insn
          , call_op ) )
  | OpExp {left; oper; right} -> (
      let build_right, op_right = cgE_ right in
      let build_left, op_left = cgE_ left in
      match oper with
      | PlusOp | MinusOp | TimesOp ->
          let bop =
            match oper with
            | PlusOp -> Add
            | MinusOp -> Sub
            | TimesOp -> Mul
            | _ -> raise CodeGenerationBug
          in
          let binop_insn = Binop (bop, I64, op_left, op_right) in
          let b_binop_insn, binop_op = aiwf "binop" binop_insn in
          (build_left $> build_right $> b_binop_insn, binop_op)
      | EqOp | NeqOp -> (
          let ty_left = ty_to_llty (ty_of_exp left) in
          let cnd =
            match oper with
            | EqOp -> Eq
            | NeqOp -> Ne
            | _ -> raise CodeGenerationBug
          in
          match ty_left with
          | I64 ->
              let icmp_insn = Icmp (cnd, ty_left, op_left, op_right) in
              let b_icmp_insn, icmp_op = aiwf "icmp_i64" icmp_insn in
              let zext_insn = Zext (I1, icmp_op, I64) in
              let b_zext_insn, zext_op = aiwf "zext" zext_insn in
              ( build_left $> build_right $> b_icmp_insn $> b_zext_insn
              , zext_op )
          | Ptr I8 -> (
            match ty_of_exp left with
            | STRING ->
                let func_name =
                  match cnd with
                  | Eq -> "stringEqual"
                  | Ne -> "stringNotEq"
                  | _ -> raise CodeGenerationBug
                in
                let call_insn =
                  Call
                    ( I64
                    , Gid (S.symbol func_name)
                    , [(ptr_i8, op_left); (ptr_i8, op_right)] )
                in
                let b_call_insn, call_op = aiwf "call" call_insn in
                (build_left $> build_right $> b_call_insn, call_op)
            | _ ->
                let gep_insn_left = Gep (I8, op_left, [Const 0]) in
                let b_gep_insn_left, gep_op_left =
                  aiwf "gep_left" gep_insn_left
                in
                let gep_insn_right = Gep (I8, op_right, [Const 0]) in
                let b_gep_insn_right, gep_op_right =
                  aiwf "gep_right" gep_insn_right
                in
                let icmp_insn =
                  Icmp (cnd, ty_left, gep_op_left, gep_op_right)
                in
                let b_icmp_insn, icmp_op = aiwf "icmp_ptr" icmp_insn in
                let zext_insn = Zext (I1, icmp_op, I64) in
                let b_zext_insn, zext_op = aiwf "zext" zext_insn in
                ( build_left $> build_right $> b_gep_insn_left
                  $> b_gep_insn_right $> b_icmp_insn $> b_zext_insn
                , zext_op ) )
          | _ -> raise CodeGenerationBug )
      | LtOp | LeOp | GtOp | GeOp -> (
          let cnd, func_name =
            match oper with
            | LtOp -> (Slt, "stringLess")
            | LeOp -> (Sle, "stringLessEq")
            | GtOp -> (Sgt, "stringGreater")
            | GeOp -> (Sge, "stringGreaterEq")
            | _ -> raise CodeGenerationBug
          in
          let left_right_ty = ty_of_exp left in
          match left_right_ty with
          | Ty.STRING ->
              let call_insn =
                Call
                  ( I64
                  , Gid (S.symbol func_name)
                  , [(ptr_i8, op_left); (ptr_i8, op_right)] )
              in
              let b_call_insn, call_op = aiwf "call" call_insn in
              (build_left $> build_right $> b_call_insn, call_op)
          | _ ->
              let icmp_insn =
                Icmp (cnd, ty_to_llty left_right_ty, op_left, op_right)
              in
              let b_icmp_insn, icmp_op = aiwf "icmp" icmp_insn in
              let zext_insn = Zext (I1, icmp_op, I64) in
              let b_zext_insn, zext_op = aiwf "zext" zext_insn in
              ( build_left $> build_right $> b_icmp_insn $> b_zext_insn
              , zext_op ) )
      | ExponentOp ->
          let build_right, op_right = cgE_ right in
          let build_left, op_left = cgE_ left in
          let call_insn =
            Call
              ( I64
              , Gid (S.symbol "exponent")
              , [(I64, op_left); (I64, op_right)] )
          in
          let b_call_insn, call_op = aiwf "exponent" call_insn in
          (build_left $> build_right $> b_call_insn, call_op)
      | DivideOp -> (
          let lbl_div_zero, lbl_div_ok =
            (fresh "divisionByZero", fresh "divisionOk")
          in
          let build_right, op_right = cgE_ right in
          let build_left, op_left = cgE_ left in
          match op_right with
          | Const 0 ->
              let call_insn =
                Call (I64, Gid (S.symbol "divisionByZero"), [])
              in
              let b_call_insn = B.add_insn (None, call_insn) in
              (b_call_insn, Const 0)
          | Const _ ->
              let binop_insn = Binop (SDiv, I64, op_left, op_right) in
              let b_binop_insn, binop_op = aiwf "binop" binop_insn in
              (build_left $> build_right $> b_binop_insn, binop_op)
          | _ ->
              let icmp_insn = Icmp (Eq, I64, op_right, Const 0) in
              let b_icmp_insn, icmp_op = aiwf "icmp" icmp_insn in
              let terminate_entry =
                B.term_block (Cbr (icmp_op, lbl_div_zero, lbl_div_ok))
              in
              let start_zero = B.start_block lbl_div_zero in
              let call_insn =
                Call (I64, Gid (S.symbol "divisionByZero"), [])
              in
              let b_call_insn = B.add_insn (None, call_insn) in
              let terminate_zero = B.term_block (Br lbl_div_zero) in
              let start_ok = B.start_block lbl_div_ok in
              let binop_insn = Binop (SDiv, I64, op_left, op_right) in
              let b_binop_insn, binop_op = aiwf "binop" binop_insn in
              ( build_left $> build_right $> b_icmp_insn $> terminate_entry
                $> start_zero $> b_call_insn $> terminate_zero $> start_ok
                $> b_binop_insn
              , binop_op ) ) )
  | RecordExp {fields} ->
      let tuple_list, record_unique =
        match actual_type ty with
        | RECORD (lst, unique) -> (lst, unique)
        | _ -> raise CodeGenerationBug
      in
      let field_names = List.map (fun (x, _) -> x) tuple_list in
      let ty_top_level_exp = ty_to_llty ty in
      let name = UniqueMap.find record_unique ctxt.uenv in
      let gep_insn = Gep (Namedt name, Null, [Const 1]) in
      let b_gep_insn, gep_op = aiwf "gep" gep_insn in
      let ptrtoint_insn = Ptrtoint (Namedt name, gep_op, I64) in
      let b_ptrtoint_insn, ptrtoint_op = aiwf "ptrtoint" ptrtoint_insn in
      let call_insn =
        Call
          ( ty_top_level_exp
          , Gid (S.symbol "allocRecord")
          , [(I64, ptrtoint_op)] )
      in
      let b_call_insn, call_op = aiwf "call" call_insn in
      let ty_fields =
        List.map (fun (_, exp) -> ty_to_llty (ty_of_exp exp)) fields
      in
      let bitcast_insn =
        Bitcast (ty_top_level_exp, call_op, Ptr (Struct ty_fields))
      in
      let b_bitcast_insn, bitcast_op = aiwf "bitcast" bitcast_insn in
      let store_fields lst (field_name, exp) =
        let field_number = find_index field_name field_names in
        let ty_exp, tuple_exp = get_ty_and_tuple ctxt exp in
        let gep_insn = gep_0 (Struct ty_fields) bitcast_op field_number in
        let b_gep_insn, gep_op = aiwf "gep" gep_insn in
        let store_insn = Store (ty_exp, snd tuple_exp, gep_op) in
        let b_store_insn = B.add_insn (None, store_insn) in
        lst @ [fst tuple_exp; b_gep_insn; b_store_insn]
      in
      let b_store_fields_list = List.fold_left store_fields [] fields in
      let b_store_fields_list_rev = B.seq_buildlets b_store_fields_list in
      ( b_gep_insn $> b_ptrtoint_insn $> b_call_insn $> b_bitcast_insn
        $> b_store_fields_list_rev
      , call_op )
  | SeqExp exps -> (
    match List.length exps with
    | 0 -> (B.id_buildlet, Const 0)
    | _ ->
        let tuple_list = List.map cgE_ exps in
        let last_tuple = List.nth tuple_list (List.length tuple_list - 1) in
        let buildlets = B.seq_buildlets (List.map fst tuple_list) in
        (buildlets, snd last_tuple) )
  | AssignExp {var; exp} ->
      let b_exp, exp_op = cgE_ exp in
      let b_var, var_op = cgVar ctxt var in
      let store_insn = Store (ty_to_llty (ty_of_exp exp), exp_op, var_op) in
      let b_store = B.add_insn (None, store_insn) in
      (b_exp $> b_var $> b_store, Const 0)
  | IfExp {test; thn; els} -> (
    match els with
    | Some e -> (
        let b_test, test_op = cgE_ test in
        match test_op with
        | _ -> (
            let b_then, then_op = cgE_ thn in
            let (H.Exp {exp_base= eb1; _}) = thn in
            let (H.Exp {exp_base= eb2; _}) = e in
            if eb1 = eb2 then (b_then, then_op)
            else
              let lbl_then, lbl_else, lbl_merge, lbl_alloca =
                (fresh "then", fresh "else", fresh "merge", fresh "alloca")
              in
              let icmp_insn = Icmp (Eq, I64, test_op, Const 0) in
              let b_icmp_insn, icmp_op = aiwf "icmp" icmp_insn in
              let terminate_entry =
                B.term_block (Cbr (icmp_op, lbl_else, lbl_then))
              in
              let start_then = B.start_block lbl_then in
              let terminate_then = B.term_block (Br lbl_merge) in
              let start_else = B.start_block lbl_else in
              let b_else, else_op = cgE_ e in
              let terminate_else = B.term_block (Br lbl_merge) in
              let start_merge = B.start_block lbl_merge in
              match ty_of_exp thn with
              | Ty.VOID ->
                  ( b_test $> b_icmp_insn $> terminate_entry $> start_then
                    $> b_then $> terminate_then $> start_else $> b_else
                    $> terminate_else $> start_merge
                  , Const 0 )
              | _ ->
                  let b_alloca = B.add_alloca (lbl_alloca, ty_to_llty ty) in
                  let store_insn_then =
                    Store (ty_to_llty ty, then_op, Id lbl_alloca)
                  in
                  let store_insn_else =
                    Store (ty_to_llty ty, else_op, Id lbl_alloca)
                  in
                  let b_store_then = B.add_insn (None, store_insn_then) in
                  let b_store_else = B.add_insn (None, store_insn_else) in
                  let load_insn = Load (ty_to_llty ty, Id lbl_alloca) in
                  let b_load_insn, load_op = aiwf "load" load_insn in
                  ( b_test $> b_icmp_insn $> terminate_entry $> start_then
                    $> b_then $> b_store_then $> terminate_then $> start_else
                    $> b_else $> b_store_else $> terminate_else
                    $> start_merge $> b_alloca $> b_load_insn
                  , load_op ) ) )
    | None -> (
        let lbl_then, lbl_merge = (fresh "then", fresh "merge") in
        let b_test, test_op = cgE_ test in
        let icmp_insn = Icmp (Eq, I64, test_op, Const 0) in
        let b_icmp_insn, icmp_op = aiwf "icmp" icmp_insn in
        let terminate_entry =
          B.term_block (Cbr (icmp_op, lbl_merge, lbl_then))
        in
        let start_then = B.start_block lbl_then in
        let b_then, _ = cgE_ thn in
        let terminate_then = B.term_block (Br lbl_merge) in
        let start_merge = B.start_block lbl_merge in
        match ty_of_exp thn with
        | Ty.VOID ->
            ( b_test $> b_icmp_insn $> terminate_entry $> start_then
              $> b_then $> terminate_then $> start_merge
            , Const 0 )
        | _ -> raise CodeGenerationBug ) )
  | WhileExp {test; body} -> (
      let b_test, test_op = cgE_ test in
      match test_op with
      | Const 0 -> (b_test, Const 0)
      | Const _ ->
          let lbl_body, lbl_merge = (fresh "body", fresh "merge") in
          let terminate_entry = B.term_block (Br lbl_body) in
          let start_body = B.start_block lbl_body in
          let b_body, _ = cgExp {ctxt with break_lbl= Some lbl_merge} body in
          let terminate_body = B.term_block (Br lbl_body) in
          let start_merge = B.start_block lbl_merge in
          ( b_test $> terminate_entry $> start_body $> b_body
            $> terminate_body $> start_merge
          , Const 0 )
      | _ ->
          let lbl_test, lbl_body, lbl_merge =
            (fresh "test", fresh "body", fresh "merge")
          in
          let terminate_entry = B.term_block (Br lbl_test) in
          let start_test = B.start_block lbl_test in
          let icmp_insn = Icmp (Eq, I64, test_op, Const 0) in
          let b_icmp, icmp_value = aiwf "icmp" icmp_insn in
          let terminate_test =
            B.term_block (Cbr (icmp_value, lbl_merge, lbl_body))
          in
          let start_body = B.start_block lbl_body in
          let b_body, _ = cgExp {ctxt with break_lbl= Some lbl_merge} body in
          let terminate_body = B.term_block (Br lbl_test) in
          let start_merge = B.start_block lbl_merge in
          ( terminate_entry $> start_test $> b_test $> b_icmp
            $> terminate_test $> start_body $> b_body $> terminate_body
            $> start_merge
          , Const 0 ) )
  | BreakExp -> (
    match ctxt.break_lbl with
    | Some lbl ->
        let terminate_body = B.term_block (Br lbl) in
        let breaked_lbl = fresh "breaked" in
        let ce_mai_faci = B.start_block breaked_lbl in
        (terminate_body $> ce_mai_faci, Const 0)
    | None -> raise CodeGenerationBug )
  | LetExp {vardecl= VarDec {name; typ; init; _}; body; _} ->
      let b_init, init_op = cgE_ init in
      let gep_insn =
        gep_0 (Namedt ctxt.summary.locals_tid) (Id ctxt.summary.locals_uid)
          (ctxt.summary.offset_of_symbol name)
      in
      let b_gep_insn, gep_op = aiwf "gep" gep_insn in
      let store_insn = Store (ty_to_llty typ, init_op, gep_op) in
      let b_store_insn = B.add_insn (None, store_insn) in
      let b_body, body_op = cgE_ body in
      (b_gep_insn $> b_init $> b_store_insn $> b_body, body_op)
  | ArrayExp {size; init} ->
      let ty_size, tuple_size = get_ty_and_tuple ctxt size in
      let ty_init, tuple_init = get_ty_and_tuple ctxt init in
      let lbl_init = fresh "alloca_array_init" in
      let b_alloca_insn = B.add_alloca (lbl_init, ty_init) in
      let store_insn = Store (ty_init, snd tuple_init, Id lbl_init) in
      let b_store_insn, _ = (B.add_insn (None, store_insn), None) in
      let bitcast_insn = Bitcast (Ptr ty_init, Id lbl_init, ptr_i8) in
      let b_bitcast_insn, bitcast_op = aiwf "bitcast" bitcast_insn in
      let gep_insn = Gep (ty_init, Null, [Const 1]) in
      let b_gep_insn, gep_op = aiwf "gep" gep_insn in
      let ptrtoint_insn = Ptrtoint (ty_init, gep_op, I64) in
      let b_ptrtoint_insn, ptrtoint_op = aiwf "ptrtoint" ptrtoint_insn in
      let call_insn =
        Call
          ( ty_to_llty ty
          , Gid (S.symbol "initArray")
          , [ (ty_size, snd tuple_size)
            ; (I64, ptrtoint_op)
            ; (ptr_i8, bitcast_op) ] )
      in
      let b_call_insn, call_op = aiwf "call" call_insn in
      ( fst tuple_size $> fst tuple_init $> b_alloca_insn $> b_store_insn
        $> b_bitcast_insn $> b_gep_insn $> b_ptrtoint_insn $> b_call_insn
      , call_op )

and get_ty_and_tuple (ctxt : context) (Exp {exp_base; ty; pos} : H.exp) =
  let tuple = cgExp ctxt (H.Exp {exp_base; ty; pos}) in
  (ty_to_llty ty, tuple)

and cgVar (ctxt : context) (H.Var {var_base; _}) =
  let open Ll in
  match var_base with
  | AccessVar (i, varname) ->
      let chase_buildlets, load_op, _ = chase_sl i varname ctxt in
      let buildlets = B.seq_buildlets chase_buildlets in
      (buildlets, load_op)
  | FieldVar (var, symbol) ->
      let b_var, var_op = cgVar ctxt var in
      (* check nil *)
      let lbl_bad, lbl_good = (fresh "bad", fresh "good") in
      let ty = ty_of_var var in
      let llty = ty_to_llty ty in
      let fucking_load = Load (llty, var_op) in
      let b_fucking_load, fucking_load_op = aiwf "load" fucking_load in
      let icmp_insn = Icmp (Eq, llty, fucking_load_op, Null) in
      let b_icmp_insn, icmp_op = aiwf "icmp" icmp_insn in
      let terminate_entry =
        B.term_block (Cbr (icmp_op, lbl_bad, lbl_good))
      in
      let start_bad = B.start_block lbl_bad in
      let call_insn = Call (I64, Gid (S.symbol "recFieldError"), []) in
      let b_call_insn, _ = aiwf "call" call_insn in
      let terminate_bad = B.term_block (Br lbl_bad) in
      let start_good = B.start_block lbl_good in
      (* not nil :) *)
      let tuple_list =
        match actual_type ty with
        | RECORD (lst, _) -> lst
        | _ -> raise CodeGenerationBug
      in
      let field_names = List.map (fun (s, _) -> s) tuple_list in
      let ty_fields = List.map (fun (_, t) -> ty_to_llty t) tuple_list in
      let load_insn = Load (ptr_i8, var_op) in
      let b_load_insn, load_op = aiwf "load" load_insn in
      let bitcast_insn = Bitcast (ptr_i8, load_op, Ptr (Struct ty_fields)) in
      let b_bitcast_insn, bitcast_op = aiwf "bitcast" bitcast_insn in
      let gep_insn =
        gep_0 (Struct ty_fields) bitcast_op (find_index symbol field_names)
      in
      let b_gep_insn, gep_op = aiwf "gep" gep_insn in
      ( b_var $> b_fucking_load $> b_icmp_insn $> terminate_entry
        $> start_bad $> b_call_insn $> terminate_bad $> start_good
        $> b_load_insn $> b_bitcast_insn $> b_gep_insn
      , gep_op )
  | SubscriptVar (var, exp) ->
      let ty =
        match actual_type (ty_of_var var) with
        | ARRAY (ty, _) -> ty_to_llty ty
        | _ -> raise CodeGenerationBug
      in
      let b_exp, op_exp = cgExp ctxt exp in
      let b_var, op_var = cgVar ctxt var in
      let lbl_merge, lbl_comp_size, lbl_bad =
        (fresh "merge", fresh "comparison_size", fresh "bad")
      in
      let icmp_negative_insn = Icmp (Slt, I64, op_exp, Const 0) in
      let b_icmp_negative_insn, icmp_negative_op =
        aiwf "icmp" icmp_negative_insn
      in
      let terminate_entry =
        B.term_block (Cbr (icmp_negative_op, lbl_bad, lbl_comp_size))
      in
      (* bad *)
      let start_bad = B.start_block lbl_bad in
      let call_insn =
        Call (I64, Gid (S.symbol "arrInxError"), [(I64, op_exp)])
      in
      let b_call_insn = B.add_insn (None, call_insn) in
      let terminate_bad = B.term_block (Br lbl_bad) in
      (* i >= 0 *)
      let start_good1 = B.start_block lbl_comp_size in
      let load_pointer_insn = Load (ptr_i8, op_var) in
      let b_load_pointer_insn, load_pointer_op =
        aiwf "load" load_pointer_insn
      in
      let bitcast_size_insn = Bitcast (ptr_i8, load_pointer_op, Ptr I64) in
      let b_bitcast_size_insn, bitcast_op =
        aiwf "bitcast" bitcast_size_insn
      in
      let gep_size_insn = Gep (I64, bitcast_op, [Const (-1)]) in
      let b_gep_size_insn, gep_size_op = aiwf "gep" gep_size_insn in
      let load_size_insn = Load (I64, gep_size_op) in
      let b_load_size_insn, load_op = aiwf "load" load_size_insn in
      let icmp_size_insn = Icmp (Sge, I64, op_exp, load_op) in
      let b_icmp_size_insn, icmp_size_op = aiwf "icmp" icmp_size_insn in
      let terminate_good1 =
        B.term_block (Cbr (icmp_size_op, lbl_bad, lbl_merge))
      in
      (* i < len *)
      let start_merge = B.start_block lbl_merge in
      let load_insn = Load (ptr_i8, op_var) in
      let b_load_insn, load_op = aiwf "load" load_insn in
      let bitcast_insn = Bitcast (ptr_i8, load_op, Ptr ty) in
      let b_bitcast_insn, bitcast_op = aiwf "bitcast" bitcast_insn in
      let gep_insn = Gep (ty, bitcast_op, [op_exp]) in
      let b_gep_insn, gep_op = aiwf "gep" gep_insn in
      ( b_var $> b_exp $> b_icmp_negative_insn $> terminate_entry
        $> start_bad $> b_call_insn $> terminate_bad $> start_good1
        $> b_load_pointer_insn $> b_bitcast_size_insn $> b_gep_size_insn
        $> b_load_size_insn $> b_icmp_size_insn $> terminate_good1
        $> start_merge $> b_load_insn $> b_bitcast_insn $> b_gep_insn
      , gep_op )

(* --- From this point on the code requires no changes --- *)

(* Creates summary of a function declaration; relies on the alpha conversion *)
let cg_fdecl_summary senv_ref (H.Fdecl {name; parent_opt; locals; _}) =
  let locals_uid = fresh "locals" in
  let locals_tid = locals_type_name name in
  let offset_of_symbol =
    let locals_map =
      default_sl_name :: List.map fst locals
      |> List.mapi (fun i x -> (x, i))
      |> List.to_seq |> SymbolMap.of_seq
    in
    fun sym -> SymbolMap.find sym locals_map
  in
  senv_ref :=
    SymbolMap.add name
      {parent_opt; locals_uid; locals_tid; offset_of_symbol}
      !senv_ref ;
  let sl_type =
    match parent_opt with
    | Some p -> Ll.Ptr (Ll.Namedt (p |> locals_type_name))
    | None -> Ll.Ptr I8
  in
  let locals_ty =
    Ll.Struct (sl_type :: List.map (fun (_, t) -> ty_to_llty t) locals)
  in
  (locals_tid, locals_ty)

(* --- Code genartion of function bodies --- *)
let cg_fdecl senv uenv gdecls (H.Fdecl {name; args; result; body; _}) =
  (* Function bodies are generated in 5 steps
     1. Creating the translation context
     2. Allocating the locals structure with all the variables
     3. Copying the arguments to the locals
     4. Generating the code for the function body
     5. Terminate the function

     Because we use buildlets, the order in which we take steps 2-4 does not
     matter as long as we compose the resulting buildlets correctly in the
     end.
  *)
  let open Ll in
  (* locally open the Ll module; for convenience *)

  (* Extract the necessary information from the function summary environment *)
  let ({parent_opt; locals_uid; locals_tid; offset_of_symbol; _} as summary)
      =
    SymbolMap.find name senv
  in
  (* Get the name of the static link
     - either from the lookup in the summary, if the function is not main
     - a dummy i8*, for the main function
  *)
  let sl_type =
    match parent_opt with
    | Some p -> Ll.Ptr (Ll.Namedt (SymbolMap.find p senv).locals_tid)
    | None -> Ll.Ptr I8
  in
  (* A tuple that contains all sl-related information  *)
  let sl_info = (default_sl_name, sl_type) in
  (* Step 1 -- this is our context *)
  let ctxt = {summary; break_lbl= None; uenv; senv; gdecls} in
  (* A buildlet for allocating the locals structure *)
  let build_alloca = B.add_alloca (locals_uid, Namedt locals_tid) in
  (* Aux list of arguments with SL added in the beginning *)
  let args' =
    sl_info
    :: List.map (fun (H.Arg {name; ty; _}) -> (name, ty_to_llty ty)) args
  in
  let copy_one_arg (name, ty) =
    (* A buildlet for copying one argument *)
    let build_gep, op_gep =
      aiwf "arg"
        (gep_0 (* Use GEP to find where to store the argument *)
           (Namedt locals_tid) (Id locals_uid) (offset_of_symbol name) )
    in
    (* Observe how we return the composition of two buildlets *)
    build_gep $> build_store ty (Id name) op_gep
  in
  let copy_args =
    (* A buildlet that copies all of the arguments *)
    List.rev args' |> List.map copy_one_arg |> B.seq_buildlets
  in
  let ret_ty, tr =
    match result with
    | Ty.VOID -> (Void, fun _ -> Ret (Void, None))
    | t ->
        let llty = ty_to_llty t in
        (llty, fun op -> Ret (llty, Some op))
  in
  let build_body, op =
    (* Get the builder for the body and the operand with
       the result; observe that we pass the context. *)
    cgExp ctxt body
  in
  let build_function (* Putting it all together *) =
    build_alloca (* Step 2 *) $> copy_args (* Step 3 *)
    $> build_body (* Step 4 *)
    $> B.term_block (tr op)
  in
  (* Step 5 *)
  let cfg_builder =
    (* Apply the buildlet; we get a cfg_builder *)
    build_function B.empty_cfg_builder
  in
  ( name
  , { fty= (sl_type :: List.map (ty_to_llty <$> ty_of_arg) args, ret_ty)
    ; param= default_sl_name :: List.map name_of_arg args
    ; cfg= B.get_cfg cfg_builder } )

let codegen_prog (H.Program {tdecls; fdecls}) : Ll.prog =
  let tenv = ref UniqueMap.empty in
  let senv = ref SymbolMap.empty in
  let gdecls = ref [] in
  let tdecls1 = List.filter_map (cg_tydecl tenv) tdecls in
  let tdecls2 = List.map (cg_fdecl_summary senv) fdecls in
  let fdecls = List.map (cg_fdecl !senv !tenv gdecls) fdecls in
  let tdecls = tdecls1 @ tdecls2 in
  let gdecls = !gdecls in
  let open Ll in
  {tdecls; gdecls; fdecls}

let runtime_fns =
  let fns =
    [ "i8* @allocRecord(i64)" (* runtime functions *)
    ; "i8* @initArray (i64, i64, i8*)"
    ; "i64 @arrInxError (i64)"
    ; "i64 @recFieldError()"
    ; "i64 @divisionByZero()"
    ; "i64 @stringEqual (i8*, i8*)"
    ; "i64 @stringNotEq (i8*, i8*)"
    ; "i64 @stringLess (i8*, i8*)"
    ; "i64 @stringLessEq (i8*, i8*)"
    ; "i64 @stringGreater (i8*, i8*)"
    ; "i64 @stringGreaterEq (i8*, i8*)"
    ; "i64 @exponent(i64, i64)"
      (* user visible functions; note SL argument *)
    ; "void @print      (i8*, i8*)"
    ; "void @flush      (i8*)"
    ; "i8*  @getChar    (i8*)"
    ; "i64  @ord        (i8*, i8*)"
    ; "i8*  @chr        (i8*, i64)"
    ; "i64  @size       (i8*, i8*)"
    ; "i8*  @substring  (i8*, i8*, i64, i64)"
    ; "i8*  @concat     (i8*, i8*, i8*)"
    ; "i64  @not        (i8*, i64)"
    ; "void @tigerexit  (i8*, i64)" ]
  in
  let mkDeclare s = "declare " ^ s ^ "\n" in
  String.concat "" (List.map mkDeclare fns)

let ll_target_triple : string =
  let ic = Unix.open_process_in "uname" in
  let uname = input_line ic in
  let () = close_in ic in
  match uname with
  | "Darwin" -> "target triple = \"x86_64-apple-macosx10.15.0\""
  | "Linux" -> "target triple = \"x86_64-pc-linux-gnu\""
  | _ -> ""