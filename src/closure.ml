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

let closure_conversion (elexps : ((vname * EL.elexp) list list)) : ((vname * EL.elexp) list list) =
  let rec close_elexp elexp = match elexp with
    | EL.Lambda (vname, elexp)
      -> close_lambda vname elexp
    | EL.Let (loc, elexps, elexp)
        -> EL.Let (loc, List.map (fun (vname,e) -> (vname, close_elexp e)) elexps, close_elexp elexp)
    | EL.Call (elexp, elexps)
        -> EL.Call (close_elexp elexp, List.map close_elexp elexps)
    | EL.Case (loc, elexp, branches, default)
      -> EL.Case(loc, close_elexp elexp, close_branches branches, close_default_branch default)
    | _ -> elexp
  and close_default_branch default = match default with
    | Some (vname, e) -> Some (vname, close_elexp e)
    | None -> None
  and close_branches branches =
    SMap.map (fun (loc, vnames, elexp) -> (loc, vnames, close_elexp elexp)) branches
  and close_lambda vname elexp =
    (* TODO *)
    EL.Lambda (vname, elexp) in
  let close_vname_elexps (vname, elexp) = (vname, close_elexp elexp) in
  (* close all elexps while keeping them separated *)
  List.map (fun _elexps -> List.map close_vname_elexps _elexps) elexps
