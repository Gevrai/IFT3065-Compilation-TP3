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

  (* A (non-curried) function call.  *)
  | Call of cexp * cexp list

  (* A data constructor, such as `cons` or `nil`.  *)
  | MkRecord of symbol * cexp list

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
  | Lambda of vname list * cexp
  | Cexp of cexp

(* The content of a whole file.  *)
type cfile = (vname * ctexp) list


let rec elexp_to_ctexp elexp global = match elexp with
    | EL.Imm e -> Imm e
    | EL.Builtin vn -> Builtin vn
    | EL.Var vr -> Var (global, vr)
    | EL.Let (loc, name_exp_list, body)
        -> Let (loc, 
            (fun (name, exp)
                -> (name, elexp_to_cexp exp false))
                name_exp_list,
                elexp_to_cexp body false)
    | EL.Lambda (name, body)
        -> (* problem *)
            
    | EL.Call (f, args_list)
        -> Call (elexp_to_cexp f false
                List.map (fun e ->  elexp_to_cexp e false) args_list)
    | EL.Cons (sym, i)
        -> (* problem *)
    | EL.Case (l, e, branches, default)
        -> Case (l, elexp_to_ctexp e, 
            SMap.Map 
                (fun (loc, _, e) -> (loc, elexp_to_ctexp e false)
                    branches,
                (fun def
                    -> if def = None then None
                       else (match def with
                                | (_, el) -> elexp_to_ctexp el false)) 
                    default)

    | EL.Type lexp
        -> Type lexp

let rec cfile_to_c_code cfile = match cfile with
    | [] -> ""
    | (vname, ctexp) :: others -> typeof_ctexp ctexp ^ ctexp_to_c_code ctexp 
                                    ^ cfile_to_c_code others

and rec typeof_ctexp ctexp = 
    (* TODO *)
    "void"

and rec ctexp_to_c_code ctexp = match ctexp with
    (* TODO  add type to arguments *)
    | Lambda (args, body) 
      -> "(" ^ print_args args ^ ")" ^ "{" ^ cexp_to_c_code body ^ "};"
    | Cexp cexp -> cexp_to_c_code cexp

and cexp_to_c_code cexp = match cexp with
    | Imm (Integer (_, i)) -> string_of_int i
    | Imm (Float (_, f))   -> string_of_float f
    | Imm (String (_, s))  -> s
    (* Builtin TODO *)
    | Var (_, ((_, name), _)) -> name
    | (* Let TODO *)
    | _ -> ""
and print_args args = match args with
    | [] -> ""
    | (_, arg_name) :: [] -> arg_name   
    | (_, arg_name) :: others -> arg_name ^ "," ^ print_args others
