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

open Format

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
  | Closure of string * (string) list

  (* Extract variable at index in a context *)
  | Context_Select of int

  (* Extract field of a record.  *)
  | Select of cexp * int

  (* Case analysis on an agebraic datatype.
   * I.e. tests the special `symbol` field of a record.  *)
  | Case of U.location * cexp
            * (U.location * (vname option) list * cexp) SMap.t
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

(* Mutable list for simplicity. Those are simply the hoisted lambdas that should be
   declared before everything else...  Not very pure but oh well!  *)
let hoisted_lambdas = ref []

(* This constructs the lists in reverse, should be reversed before outputting *)
let add_lambda (n : vname) (l : ctexp) = hoisted_lambdas := (n,l)::!hoisted_lambdas

(* Needs to be done at every declaration to keep the db indexes valid *)
let extend_rctx varname rctx = Env.add_rte_variable (Some varname) Env.Vundefined rctx
(* This print function is here to get rid of circular build error... *)
let ctxstring = "ctx"
let ctx_select_string n = sprintf "%s[%d]" ctxstring n

(* Uses the runtime environment to see if a something is a free variable *)
let capture_free_vars elexp rctx dbi : string list =
  let free_vars = ref [] in
  let rec _capture _el rctx curr_i = match _el with
    | EL.Var ((_, name), dbi) ->
      (* Check if it's declared before (dbi > curr_i) and if it's referencing a Builtin *)
      if dbi > curr_i then let value_t = Env.get_rte_variable (Some name) dbi rctx in
      (match value_t with
      | Env.Vbuiltin _ -> ()
      (* Add to free_vars if it's not already there *)
      | _ -> if not (List.exists (fun n -> n = name) !free_vars)
        then free_vars := name::!free_vars else ()
      )
    | EL.Let (_, els, el)
      -> let newrctx = ref rctx in
      List.iteri (fun i vname_elexp ->
          let ((_,n), e) = vname_elexp in
          newrctx := extend_rctx n !newrctx;
          _capture e !newrctx (curr_i + i)
        ) els;
      _capture el !newrctx (List.length els + curr_i)
    | EL.Lambda ((_,name), el)
      -> _capture el (extend_rctx name rctx) (curr_i+1)
    | EL.Call (el, els) ->
      _capture el rctx curr_i;
      List.iter (fun e -> _capture e rctx curr_i) els
    | EL.Case (_,el, branches, default)
      ->_capture el rctx curr_i;
      (* FIXME Should branches and default declarations be added to rctx ? *)
      List.iter (fun (_ ,(_,_,e)) -> _capture e rctx curr_i) (SMap.bindings branches);
      (match default with Some (_, e) -> _capture e rctx curr_i | None -> ())
    | _ -> () in
  _capture elexp rctx dbi;
  !free_vars

(* Change all references to a variable in a cexp with the corresponding index in vars *)
let free_vars_to_select_context vars c : cexp =
  let vars = List.mapi (fun i fv -> (i, fv)) vars in
  let rec _convert vars c = (match c with
    | Var (global, ((_, name), _))
      -> (try let (i,n) = List.find (fun (i,n) -> n = name) vars in
            Context_Select (i)
          with Not_found -> c)
    (* TODO not sure at all about this, when we find a closure should we just change its args
       for the free variables we found, or should we go deeper, or do it at all ??? *)
    | Closure (name, args)
      -> let change_name name =
           (try let (i,n) = List.find (fun (i,n) -> n = name) vars in
              ctx_select_string i with Not_found -> name) in
      Closure (name, List.map change_name args)
    | Let (loc, _cs, _c)
      -> Let (loc,
              (List.map (fun (name, c) -> (name, (_convert vars c))) _cs),
              _convert vars _c)
    | Call (c, cs)
      -> Call(_convert vars c, List.map (_convert vars) cs)
    | _ -> c) in
  _convert vars c

(* el => a single elexp
 * els => a list of elexp
 * elss=> a list list of elexp *)
let rec _elexp_to_cexp (isGlobal : bool) (rctx : Env.runtime_env) (el : EL.elexp) : cexp =
  match el with
  | EL.Imm s -> Imm s
  | EL.Builtin vname -> Builtin vname
  | EL.Var ((loc,name), dbi)
    (* Needs to create a Builtin if the variable refers to one *)
    -> let value_t = Env.get_rte_variable (Some name) dbi rctx in
    (match value_t with
     | Env.Vbuiltin blt_name -> Builtin (loc,blt_name)
     | _ -> Var (isGlobal, ((loc,name),dbi))
    )
  | EL.Let (loc, els, el)
    -> let _aux (rctx, cs) vname_elexp =
         let ((l,name), e) = vname_elexp in
         let rctx = extend_rctx name rctx in
         let c = _elexp_to_cexp false rctx e in
         (rctx, ((l,name),c)::cs) in
    let (rctx, cs) = List.fold_left _aux (rctx, []) els in
    Let (loc, cs, _elexp_to_cexp false rctx el)

  | EL.Lambda ((loc,varname), el) ->
    let rctx = extend_rctx varname rctx in
    let _body = _elexp_to_cexp false rctx el in
    let free_vars = capture_free_vars el rctx 0 in
    let body = free_vars_to_select_context free_vars _body in
    let lamdba_name = "fun" ^ string_of_int (List.length !hoisted_lambdas) in
    (* Closure conversion and hoisting *)
    add_lambda (loc, lamdba_name) (Lambda ((loc,varname), body));
    Closure (lamdba_name, free_vars)
  | EL.Call (el, els)
    -> Call (_elexp_to_cexp false rctx el, List.map (_elexp_to_cexp false rctx) els)
  | EL.Cons (s, i)
    -> (* TODO *) MkRecord (s, [])
  | EL.Case (loc, el, branches, default)
    -> (* Trying to mimick Eval.eval_case *)
    let c_test = _elexp_to_cexp false rctx el in
    let single_branch_transform (loc, pat_args, e) =
      (* Copied and tweaked from Eval.eval_case *)
        let rec fold2 nctx pats = match pats with
            | pat::pats -> let nctx = extend_rctx (match pat with
                  | Some (_, name) -> name
                  | _ -> ""
                ) nctx in
                fold2 nctx pats
            | [] -> nctx in
        let nctx = fold2 rctx pat_args in
        (loc, pat_args, _elexp_to_cexp false nctx e)
    in
    let c_branches = SMap.map single_branch_transform branches in
    let c_default = (match default with
        | Some (vname, e) -> Some (_elexp_to_cexp false (extend_rctx "" rctx) e)
        | None -> None
      )
    in
    Case (loc, c_test, c_branches, c_default)
  | EL.Type t
    -> Type t

(* This should return a list of (vname * ctexp) AKA a cfile *)
let compile_decls_toplevel (elss : ((vname * Elexp.elexp) list list)) lambdas rctx =
  hoisted_lambdas := lambdas;
  (* Construct runtime context without evaluating anything, copied on Eval._eval_decls *)
  let add_to_rctx rctx ((_, name), _) = Env.add_rte_variable (Some name) Env.Vundefined rctx in
  let elexps_to_cfile (cfile, rctx) vname_els =
    let rctx = List.fold_left add_to_rctx rctx vname_els in
    let _cfile =
      let rec _aux els rctx = (match els with
          | (_vname,_e)::_els -> (_vname,Cexp(_elexp_to_cexp true rctx _e)) :: (_aux _els rctx)
          | [] -> [])
      in _aux vname_els rctx
    in
    (cfile @ _cfile, rctx)
  in
  let (cfile, rctx) = List.fold_left elexps_to_cfile ([],rctx) elss in
  (cfile, !hoisted_lambdas, rctx)
