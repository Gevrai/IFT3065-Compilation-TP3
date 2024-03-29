(* codify.ml Writes a cfile to a file.c

   Copyright (C) 2016-2017  Free Software Foundation, Inc.

   Author: Pierre Delaunay <pierre.delaunay@hec.ca>
   Author: Gevrai Jodoin-Tremblay <gevrai@gmail.com>
   Author: Nicolas Lafond <lafondni89@gmail.com>

   This file is part of Typer.

   Typer is free software; you can redistribute it and/or modify it under the
   terms of the GNU General Public License as published by the Free Software
   Foundation, either version 3 of the License, or (at your option) any
   later version.

   Typer is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   You should have received a copy of the GNU General Public License along with
   this program.  If not, see <http://www.gnu.org/licenses/>.  *)

(*
 *      Description:
 *          From a cfile, get a real .C file !
 * -------------------------------------------------------------------------- *)

open Cexp
open Printf

let header_string = ref "#include \"src/runtime_support.c\""
let main_string = ref "int main(){"

let gentype = "prim_type"
let blt_prefix = "builtin_"

let assoc_builtins_to_c_function = [
  ("Int.+",           (2,"int_add"));
  ("Int.-",           (2,"int_sub"));
  ("Int.*",           (2,"int_mul"));
  ("Int./",           (2,"int_div"));
  ("Float.+",         (2,"float_add"));
  ("Float.-",         (2,"float_sub"));
  ("Float.*",         (2,"float_mul"));
  ("Float./",         (2,"float_div"));
  ("Float.to_string", (1,"float_tostring"));
  ("Int.<",           (2,"int_lt"));
  ("Int.>",           (2,"int_gt"));
  ("Int.=",           (2,"int_eq"));
  ("Int.<=",          (2,"int_leq"));
  ("Int.>=",          (2,"int_geq"));
  ("String.=",        (2,"str_eq"));
  (* ARITY UNCHECKED FOR OTHER BUILTINS *)
  ("Sexp.=",          (-1,"sexp_eq"));
  ("Sexp.symbol",     (-1,"sexp_symbol"));
  ("Sexp.string",     (-1,"sexp_string"));
  ("Sexp.node",       (-1,"sexp_node"));
  ("Sexp.integer",    (-1,"sexp_int"));
  ("Sexp.float",      (-1,"sexp_float"));
  ("Sexp.dispatch",   (-1,"sexp_dispatch"));
  ("IO.bind",         (-1,"io_bind"));
  ("IO.return",       (-1,"io_return"));
  ("IO.run",          (-1,"io_run"));
  ("File.open",       (-1,"file_open"));
  ("File.stdout",     (-1,"file_stdout"));
  ("File.write",      (-1,"file_write"));
  ("File.read",       (-1,"file_read"));
  ("Sys.cpu_time",    (-1,"sys_cputime"));
  ("Sys.exit",        (-1,"sys_exit"));
  ("Eq.refl",         (-1,"eq_refl"));
  ("Eq.cast",         (-1,"eq_cast"))
  (* ("Y",               "Ycombinator"); *)
]

let get_builtin typername =
  try
    let (arity, cname) = List.assoc typername assoc_builtins_to_c_function in
    if arity < 0 then (printf "Unsuported builtin: %s\n" typername ; exit 1)
    else
      arity, blt_prefix ^ cname
  with Not_found -> (printf "Unsuported builtin: %s\n" typername ; exit 1)

let prefix_ n = "_"^n

let compile_error loc msg = printf "%s\n%s\n" (Util.loc_string loc) msg; exit 1

let output_cfile output_file_name cfile =
  let outc = open_out output_file_name in

  (* Print declarations only, meaning the file is wholy mutally recursive... *)
  let rec print_globals cfile =
    let rec _print_globals cfile vars_declared = (match cfile with
      | [] -> ()
      | ((_,funname), Lambda(_))::next
        -> fprintf outc "%s %s(%s, %s*);\n"
             gentype funname gentype gentype;
        _print_globals next vars_declared;
        (* Declares only non-declared variables *)
      | ((_,varname), _)::next ->
        if List.exists (fun n -> n=varname) vars_declared
        then _print_globals next vars_declared
        else (fprintf outc "%s %s;\n" gentype (prefix_ varname);
              _print_globals next (varname::vars_declared))
      ) in
    _print_globals cfile []
    (* Print every lambdas one after the other ignoring other ctexps *)
  and print_lambdas cfile = match cfile with
    | ((funloc, funname), Lambda((argloc,argname),body))::next ->
      (* fprintf outc "\n/* %s: %s */\n" funname (Util.loc_string funloc); *)
      fprintf outc "%s %s(%s %s, %s *%s){\n\treturn "
        gentype funname gentype (prefix_ argname) gentype (prefix_ ctxstring);
      print_cexp body;
      fprintf outc ";\n}\n";
      print_lambdas next
    | _ :: next -> print_lambdas next
    | [] -> ()
  (* Print every non-lambdas one after the other *)
  and print_main cfile = match cfile with
    | ((exprloc, exprname), Cexp(cexp))::next ->
      (* fprintf outc "\n/* %s: %s */\n" exprname (Util.loc_string exprloc); *)
      fprintf outc "_%s = " exprname;
      print_cexp cexp;
      fprintf outc ";\n";
       print_main next
    | _ :: next -> print_main next
    | [] -> ()
  and print_cexp c = match c with
    | Imm (s) -> (match s with
      | Sexp.String (_, s)  -> fprintf outc "mkString(%s)" s
      | Sexp.Integer (_, i) -> fprintf outc "mkInt(%d)" i
      | Sexp.Float (_, f)   -> fprintf outc "mkFloat(%f)" f
      | Sexp.Block (loc,_,_)
        -> compile_error loc "Unsuported expression: Sexp.Block"
      | Sexp.Symbol (_)
        -> compile_error Util.dummy_location 
             "Unsuported expression: Sexp.Symbol"
      | Sexp.Node (_)
        -> compile_error Util.dummy_location "Unsuported expression: Sexp.Node"
      )
    | Var (isGlobal, ((_,varname),_)) -> fprintf outc "%s" (prefix_ varname)
    | Builtin (loc, name) ->
      let (_,fname) = get_builtin name in fprintf outc "(&%s)" fname
    | Call (func, args) -> (
        match func with
        (* If there was not enough arguments typer would have given a 
         * lambda instead.
         * Still, checking arity for debugging purposes, should not 
         * happen here *)
        | Builtin (loc,name)
          -> let (arity, cname) = get_builtin name in
          if arity <> (List.length args)
          then compile_error loc
              (sprintf ("Compile error: Builtin  
                %s expected %d arguments but received %d!")
              name arity (List.length args))
          else (
            fprintf outc "%s(" cname;
            List.iteri
              (fun i arg -> print_cexp arg; if arity > i+1 then 
                  fprintf outc ", ") args;
            fprintf outc ") "
          )
        (* Assumes typer only gives us a valid callable and right ammount 
         * of args.
         * Wraps it with as many callclosure as there are arguments *)
        | _ ->
        (* Holy damn this is dirty... Wasn't able to make it recursive :S *)
          List.iter (fun _ -> fprintf outc "callclosure(") args;
          fprintf outc "("; print_cexp func; fprintf outc ")";
          List.iter (fun c -> fprintf outc ",";print_cexp c;fprintf outc ")") 
            args
      )
    | Context_Select i -> fprintf outc "_%s" (ctx_select_string i)
    | Select (record, ind)
      -> print_cexp record; fprintf outc "[%i]" ind
    | Closure (name, args)
      -> fprintf outc "mkClosure( %s, %d,(%s[]){%s}) "
           name (List.length args) gentype 
                (String.concat ", " (List.map prefix_ args))
    | Let (loc, defs, return) ->
      let print_let_first_part ((_,name),c) =
        fprintf outc "({%s %s = " gentype (prefix_ name);
        print_cexp c; fprintf outc ";" in
      let print_let_last_part _ =
        fprintf outc "})" in
      List.iter print_let_first_part defs;
      print_cexp return;
      fprintf outc ";";
      List.iter print_let_last_part defs;

    (* FIXME FIXME FIXME take care of every cases ! *)
    | Type t -> ()
    | Case (_, c, branches, def) -> ()
    | MkRecord _ -> ()

  in
  (* Start the process ! *)
  fprintf outc "%s\n\n" !header_string;
  print_globals cfile;
  fprintf outc "\n\n";
  print_lambdas cfile;
  fprintf outc "\n%s\n" !main_string;
  print_main cfile;
  fprintf outc "\n}\n";
