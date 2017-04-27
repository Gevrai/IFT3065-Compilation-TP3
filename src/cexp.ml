(* cexp.ml --- Intermediate representation at a C-like level
 *
 *      Copyright (C) 2017  Free Software Foundation, Inc.
 *
 * Author: Stefan Monnier <monnier@iro.umontreal.ca>
 *
 * This file is part of Typer.
 *
 * Typer is free software; you can redistribute it and/or modify it under the
 * terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * Typer is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *** Commentary:
 *
 * This is meant to follow `elexp` and has the following differences:
 * - No nested functions.
 * - Instead, there is now a global environment.
 * - Functions take several arguments.
 * - `Call` is not curried any more.
 * - `Case` expressions now only do dispatch.
 * - New operations to construct and access record fields.
 *
 * -------------------------------------------------------------------------- *)


open Sexp (* Sexp type *)

module U = Util
module L = Lexp
module EL = Elexp

type vname = U.vname
type vref = U.vref

module SMap = U.SMap

type cexp =
  (* A constant, either string, integer, or float.  *)
  | Imm of sexp

  (* A builtin constant, typically a function implemented in Ocaml.  *)
  | Builtin of vname

  (* A variable reference, using deBruijn indexing.
   * The bool is `true` for a global reference and `false` for a
   * reference to a variable defined within the current expression.  *)
  | Var of bool * vref

  (* Recursive `let` binding.  *)
  | Let of U.location * (vname * cexp) list * cexp

  (* A curried function call.  *)
  | Call of cexp * cexp list

  (* A data constructor, such as `cons` or `nil`.  *)
  | MkRecord of symbol * cexp list

  (* A closure constructor *)
  | Closure of string * (string * int) list

  (* Extract field of a record.  *)
  | Select of cexp * int

  (* Case analysis on an agebraic datatype.
   * I.e. tests the special `symbol` field of a record.  *)
  | Case of U.location * cexp
            * (U.location * cexp) SMap.t
            * cexp option

  (* A Type expression.  There's no useful operation we can apply to it,
   * but they can appear in the code.  *)
  | Type of L.lexp

(* Top-level expression.  *)
type ctexp =
  | Lambda of vname * cexp
  | Cexp of cexp

(* The content of a whole file.  *)
type cfile = (vname * ctexp) list

(* Mutable list and context for simplicity... *)
let hoisted_lambdas = ref []
let lctx = ref Lparse.default_ectx

let capture_free_vars elexp =
  let free_vars = ref [] in
  let rec _capture _el = match _el with
    | EL.Var vref -> free_vars := vref::!free_vars; ()
    | EL.Let (_, els, el)
      -> _capture el; List.iter _capture (List.map (fun (_,elexp) -> elexp) els)
    | EL.Lambda (_, el) -> _capture el; ()
    | EL.Call (el, els) -> _capture el; List.iter _capture els
    | EL.Case (_,el, branches, default)
      -> _capture el;
      ignore(SMap.map _capture (SMap.map (fun (_,_,elexp) -> elexp) branches));
      (match default with Some (_,e) -> _capture e | None -> ()); ()
    | _ -> () in
  _capture elexp;
  !free_vars

(* Simply for map, call the elexp to cexp transformation specifying if its toplevel *)
let rec notGlobal_EtoC (el : EL.elexp) : cexp = _elexp_to_cexp el false
and global_EtoC (el : EL.elexp) : cexp = _elexp_to_cexp el true

(* el => a single elexp
 * els => a list of elexp
 * elss=> a list list of elexp *)
and _elexp_to_cexp (el : EL.elexp) (isGlobal : bool) : cexp = match el with
  | EL.Imm s
    -> Imm s
  | EL.Builtin vname
    -> Builtin vname
  | EL.Var vref
    -> Var (isGlobal, vref)
  | EL.Let (loc, els, el)
    -> Let (loc,
              List.map (fun (_vname,_el) -> (_vname, notGlobal_EtoC _el)) els,
              notGlobal_EtoC el)
  | EL.Lambda (vname, el)
    -> mkClosure vname el
  | EL.Call (el, els)
    -> Call (notGlobal_EtoC el, List.map notGlobal_EtoC els)
  | EL.Cons (s, i)
    -> mkRecord s i
  | EL.Case (loc, el, branches, default)
    -> mkCase loc el branches default
  | EL.Type t
    -> Type t

(* Transforms case branches to cexp *)
and mkCase loc el branches default =
 let _default_branch default = match default with
  | Some (vname, e) -> Some (vname, notGlobal_EtoC e)
  | None -> None in
 let _branches branches =
   SMap.map (fun (loc, vnames, el) -> (loc, vnames, notGlobal_EtoC el)) branches
 in
 Case(loc, notGlobal_EtoC el, _branches branches, _default_branch default)

(* Creates a MkRecord from a cons elexp *)
and mkRecord (s : Sexp.symbol) (i : int) : cexp =
  (* TODO *) MkRecord (s, [])

(* Closure conversion and hoisting *)
and mkClosure vname el =
  let free_vars = capture_free_vars el in
  let body = transform_lambda el free_vars in
  let lamdba_name = "__fun" ^ string_of_int (List.length !hoisted_lambdas) in
  let lambda = ((Util.dummy_location, lamdba_name), Lambda(vname, body)) in
  hoisted_lambdas := lambda::!hoisted_lambdas;
  Closure (lamdba_name, free_vars)

and get_args_list lambda_exp = 
  let rec aux l lis = match l with
    | EL.Lambda (arg, body)
        -> aux body (arg :: lis)
    | _ -> lis
  in aux lambda_exp []

and build_args_list n = 
  let rec aux lst n = match n with
    | 0 -> lst
    | _ -> aux ((Util.dummy_location,"arg" ^ string_of_int n) :: lst) (n-1)
  in aux [] n

let elexpss_to_cfiles (elss: ((vname * EL.elexp) list list)) (lctx : Debruijn.elab_context)
  : (cfile list) =
  (* Global always ? *)
  let _vname_elexps_to_vname_ctexps (vname, el) = (vname, Cexp (global_EtoC el)) in
  (* close all elexps while keeping them separated *)
  let elss = List.map (fun els-> List.map _vname_elexps_to_vname_ctexps els) elss in
  !hoisted_lambdas :: elss

(* This should return a list of (vname * ctexp) AKA a cfile, no idea if the arguments are OK just
 *  playing with stuff. Mainly, I don't know if lctx is useful or not... *)
let compile_decls_toplevel
    (elss : ((vname * Elexp.elexp) list list)) (lctx : Debruijn.elab_context) : cfile =
  let cfiles = elexpss_to_cfiles elss lctx in
  let rec foldcfiles cf = match cf with
    | _cf::_cfs ->  (_cf)  @ (foldcfiles _cfs)
    | [] -> [] in
  (* Returns a cfile containing everything *)
  foldcfiles cfiles


(* ============================================================
   From a cfile, get the corresponding code string
*)
let rec cfile_to_c_code cfile = match cfile with
    | [] -> ""
    | (vname, ctexp) :: others -> typeof_ctexp ctexp ^ ctexp_to_c_code ctexp
                                    ^ cfile_to_c_code others

and typeof_ctexp ctexp =
    (* TODO *)
    "u_type"

and ctexp_to_c_code ctexp = match ctexp with
    (* TODO  add type to arguments *)
    | Lambda (args, body)
      -> "(" ^ print_args args ^ ")" ^ "{" ^ cexp_to_c_code body ^ "};"
    | Cexp cexp -> cexp_to_c_code cexp

and cexp_to_c_code cexp = match cexp with
    | Imm (Integer (_, i)) -> string_of_int i
    | Imm (Float (_, f))   -> string_of_float f
    | Imm (String (_, s))  -> s
    | Builtin (loc, name)
        -> name
    | Var (_, ((_, name), _)) -> name
(*
    | Let (loc, decls, body)
        -> 
*)
    | Call (f, args)
        -> "call (" ^ cexp_to_c_code f ^ "(" ^ print_args args ^ ")"
    | MkRecord (sym, arity)
        -> 
    | Select (record, ind) -> cexp_to_c_code record ^ "[" ^ string_of_int ind ^ "]"
    | _ -> ""

and print_args args = match args with
    | [] -> ""
    | (_, arg_name) :: [] -> arg_name
    | (_, arg_name) :: others -> arg_name ^ "," ^ print_args others
