
(*
 *      Typer Compiler
 *
 * ---------------------------------------------------------------------------
 *
 *      Copyright (C) 2011-2017  Free Software Foundation, Inc.
 *
 *   Author: Pierre Delaunay <pierre.delaunay@hec.ca>
 *   Keywords: languages, lisp, dependent types.
 *
 *   This file is part of Typer.
 *
 *   Typer is free software; you can redistribute it and/or modify it under the
 *   terms of the GNU General Public License as published by the Free Software
 *   Foundation, either version 3 of the License, or (at your option) any
 *   later version.
 *
 *   Typer is distributed in the hope that it will be useful, but WITHOUT ANY
 *   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 *   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 *   more details.
 *
 *   You should have received a copy of the GNU General Public License along
 *   with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * ---------------------------------------------------------------------------
 *
 *      Description:
            Curriffied version for elexp
 *
 * --------------------------------------------------------------------------- *)


open Sexp (* Sexp type *)
open Pexp (* Aexplicit *)

module U = Util
module L = Lexp
module E = Elexp

type vname = U.vname
type vref = U.vref
type label = symbol

module SMap = U.SMap

type ulexp =
  (* A constant, either string, integer, or float.  *)
  | Imm of sexp

  (* A builtin constant, typically a function implemented in Ocaml.  *)
  | Builtin of vname

  (* A variable reference, using deBruijn indexing.  *)
  | Var of vref

  (* Recursive `let` binding.  *)
  | Let of U.location * (vname * ulexp) list * ulexp

  (* An anonymous function.  *)
  | Lambda of vname list * ulexp
  (* A (curried) function call.
   * In other words,         Call(f, [e1, e2])
   * is just a shorthand for Call (Call (f, [e1]), [e2]).  *)
  (* a (non-curried) function call *)
  | Call of ulexp * ulexp list

  (* A data constructor, such as `cons` or `nil`.  *)
  | Cons of symbol * int

  (* Case analysis on an agebraic datatype.
   * Case (l, e, branches, default)
   * tests the value of `e`, and either selects the corresponding branch
   * in `branches` or branches to the `default`.  *)
  | Case of U.location * ulexp
            * (U.location * (vname option) list * ulexp) SMap.t
            * (vname option * ulexp) option

  (* A Type expression.  There's no useful operation we can apply to it,
   * but they can appear in the code.  *)
  | Type of L.lexp

let rec elexp_to_ulexp e = match e with
    | E.Imm s -> Imm s
    | E.Builtin name-> Builtin name
    | E.Var ref -> Var ref
    | E.Let (l, decl_list, body)
        -> Let (l, 
                List.map (fun (n, e)
                              -> (n, elexp_to_ulexp e))
                  decl_list,
                elexp_to_ulexp body)
    | E.Lambda (name, body)
        -> uncurried_lambda e
    | E.Call (f, args) 
        -> Call (elexp_to_ulexp f,
                 List.map elexp_to_ulexp args)
    | E.Cons (s, i) -> Cons (s, i)
    | E.Case (l, e, branches, default)
        -> Case (l, elexp_to_ulexp e,
                 SMap.map (fun (lo, li, el) -> (lo, li, elexp_to_ulexp el)) 
                            branches,
                 (match default with
                    | None -> None
                    | Some (n, e) -> Some (n, elexp_to_ulexp e)))
    | E.Type t -> Type t 

and uncurried_lambda lexp = 
    let rec aux lexp li = match lexp with
        | E.Lambda (v, e) -> aux e (li @ v)
        | _ as body-> (li, body) 
    in 
    let (names, body) = aux lexp [] in
    Lambda (names, body)

and uncurried_call lexp = 
    let rec aux lexp li = match lexp with
        | E.Call (f, args) -> 

let rec ulexp_location e =
    match e with
        | Imm s -> sexp_location s
        | Var ((l,_), _) -> l
        | Builtin ((l, _)) -> l
        | Let (l,_,_) -> l
        | Lambda ((l,_),_) -> l
        | Call (f,_) -> ulexp_location f
        | Cons ((l,_), _) -> l
        | Case (l,_,_,_) -> l
        | Type e -> L.lexp_location e


let ulexp_name e =
  match e with
    | Imm  _ -> "Imm"
    | Var  _ -> "Var"
    | Let  _ -> "Let"
    | Call _ -> "Call"
    | Cons _ -> "Cons"
    | Case _ -> "Case"
    | Type _ -> "Type"
    | Lambda    _ -> "Lambda"
    | Builtin   _ -> "Builtin"

let rec ulexp_print lxp = print_string (ulexp_string lxp)
and ulexp_string lxp =
    let maybe_str lxp =
        match lxp with
        | Some (v, lxp)
          -> " | " ^ (match v with None -> "_" | Some (_,name) -> name)
            ^ " => " ^ ulexp_string lxp
        | None -> "" in

    let str_decls d =
        List.fold_left (fun str ((_, s), lxp) ->
            str ^ " " ^ s ^ " = " ^ (ulexp_string lxp)) "" d in

    let str_pat lst =
        List.fold_left (fun str v ->
            str ^ " " ^ (match v with
                | None -> "_"
                | Some (_, s) -> s)) "" lst in

    let str_cases c =
        SMap.fold (fun key (_, lst, lxp) str ->
            str ^ " | " ^ key ^ " " ^ (str_pat lst) ^ " => " ^ (ulexp_string lxp))
                c "" in

    let str_args lst =
        List.fold_left (fun str lxp ->
            str ^ " " ^ (ulexp_string lxp)) "" lst in

    match lxp with
        | Imm(s)          -> sexp_string s
        | Builtin((_, s)) -> s
        | Var((_, s), i)  -> s ^ "[" ^ string_of_int i ^ "]"
        | Cons((_, s), _)    -> "datacons(" ^ s ^")"

        | Lambda((_, s), b)  -> "lambda " ^ s ^ " -> " ^ (ulexp_string b)

        | Let(_, d, b)    ->
            "let" ^ (str_decls d) ^ " in " ^ (ulexp_string b)

        | Call(fct, args) ->
            "(" ^ (ulexp_string fct) ^ (str_args args) ^ ")"

        | Case(_, t, cases, default) ->
            "case " ^ (ulexp_string t) ^ (str_cases cases) ^ (maybe_str default)

        | Type e -> "Type(" ^ L.lexp_string e ^ ") "
