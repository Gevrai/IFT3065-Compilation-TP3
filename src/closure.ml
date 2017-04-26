(* closure.ml --- Transformation from normal elexp to closure converted elexp
 *
 *      Copyright (C) 2017  Free Software Foundation, Inc.
 *
 * Author: Gevrai Jodoin-Tremblay <gevrai@gmail.com>
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
 * -------------------------------------------------------------------------- *)

module C = Cexp
module EL = Elexp
module S = Sexp

type vname = Util.vname
type vref = Util.vref

module SMap = Util.SMap

(* Mutable list for simplicity... *)
let hoisted_lambdas = ref []

(* Simply for map, call the elexp to cexp transformation specifying if its toplevel *)
let rec notGlobal_EtoC (el : EL.elexp) : C.cexp = _elexp_to_cexp el false
and global_EtoC (el : EL.elexp) : C.cexp = _elexp_to_cexp el true

(* el => a single elexp
 * els => a list of elexp
 * elss=> a list list of elexp *)
and _elexp_to_cexp (el : EL.elexp) (isGlobal : bool) : C.cexp = match el with
  | EL.Imm s
    -> C.Imm s
  | EL.Builtin vname
    -> C.Builtin vname
  | EL.Var vref
    -> C.Var (isGlobal, vref)
  | EL.Let (loc, els, el)
    -> C.Let (loc,
              List.map (fun (_vname,_el) -> (_vname, notGlobal_EtoC _el)) els,
              notGlobal_EtoC el)
  | EL.Lambda (vname, el)
    -> mkClosure vname el
  | EL.Call (el, els)
    -> C.Call (notGlobal_EtoC el, List.map notGlobal_EtoC els)
  | EL.Cons (s, i)
    -> mkRecord s i
  | EL.Case (loc, el, branches, default)
    -> mkCase loc el branches default
  | EL.Type t
    -> C.Type t

(* Transforms case branches to cexp *)
and mkCase loc el branches default =
 let _default_branch default = match default with
  | Some (vname, e) -> Some (vname, notGlobal_EtoC e)
  | None -> None in
 let _branches branches =
   SMap.map (fun (loc, vnames, el) -> (loc, vnames, notGlobal_EtoC el)) branches
 in
 C.Case(loc, notGlobal_EtoC el, _branches branches, _default_branch default)

(* Creates a MkRecord from a cons elexp *)
and mkRecord (s : Sexp.symbol) (i : int) : C.cexp =
  (* TODO *) C.MkRecord (s, [])

(* Closure conversion and hoisting *)
and mkClosure vname el =
  let (list_free_var, cexp) = 
  let lambda = Lambda
  C.MkClosure (

let elexpss_to_cfiles (elss: ((vname * EL.elexp) list list)) (lctx :  : (Cexp.cfile list) =
  (* Global always ? *)
  let _vname_elexps_to_vname_cexps (vname, el) = (vname, global_EtoC el) in
  (* close all elexps while keeping them separated *)
  List.map (fun els-> List.map _vname_elexps_to_vname_cexps els) elss
