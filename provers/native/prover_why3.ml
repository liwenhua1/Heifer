open Why3

(* open this second, so it gets precedence for shadowed modules *)
open Hipcore
open Hiptypes

let prover_configs : Whyconf.config_prover SMap.t ref = ref SMap.empty

(* top-level side effects *)
let why3_config = Whyconf.init_config None
let why3_config_main = Whyconf.get_main why3_config

let why3_env : Env.env =
  let extra_paths =
    let rec find_dune_project dir =
      if Sys.file_exists (dir ^ "/dune-project") then Some dir
      else if dir = "/" then None
      else find_dune_project (Filename.dirname dir)
    in
    SSet.of_list
      (List.concat
         [
           [Sys.getcwd ()];
           Option.to_list (find_dune_project (Sys.getcwd ()));
           (try [Filename.dirname Sys.argv.(0)] with _ -> []);
         ])
  in
  let load_path =
    SSet.to_list extra_paths @ Whyconf.loadpath why3_config_main
  in
  Debug.debug ~at:4 ~title:"why load path" "%s"
    (string_of_list Fun.id load_path);
  Env.create_env load_path

let load_prover_config name : Whyconf.config_prover =
  let fp = Whyconf.parse_filter_prover name in
  let provers = Whyconf.filter_provers why3_config fp in
  if Whyconf.Mprover.is_empty provers then begin
    Format.printf "Prover %s not installed or not configured@." name;
    exit 1
  end
  else begin
    (* Format.printf "Versions of %s found:" name;
       Whyconf.(
         Mprover.iter
           (fun k _ -> Format.printf " %s/%s" k.prover_version k.prover_altern)
           provers);
       Format.printf "@."; *)
    (* returning an arbitrary one *)
    snd (Whyconf.Mprover.min_binding provers)
  end

let ensure_prover_loaded name =
  match SMap.find_opt name !prover_configs with
  | None ->
    prover_configs := SMap.add name (load_prover_config name) !prover_configs
  | Some _ -> ()

let get_prover_config name = SMap.find name !prover_configs

let load_prover_driver pc name =
  try Driver.load_driver_for_prover why3_config_main why3_env pc
  with e ->
    Format.printf "Failed to load driver for %s: %a@." name
      Exn_printer.exn_printer e;
    raise e

module Tid = struct
  type t = string list * string

  let compare = compare
end

let string_of_tid = string_of_pair (string_of_list Fun.id) Fun.id

module TMap = Map.Make (Tid)
module TSet = Set.Make (Tid)

(** Mutable data structure for keeping track of which theories have been loaded *)
module Theories = struct
  type t = Theory.theory TMap.t ref

  let create () = ref TMap.empty
  let fold f t init = List.fold_right f (TMap.bindings !t) init

  (* Some common ones we use *)
  let int = (["int"], "Int")
  let bool = (["bool"], "Bool")
  let list = (["list"], "List")
  let extras = (["extras"], "Extras")

  let needed tid t =
    (* Format.printf "needed: %s@." (string_of_tid tid); *)
    match TMap.find_opt tid !t with
    | None ->
      (* Format.printf "read: %s@." (string_of_tid tid); *)
      let path, name = tid in
      t := TMap.add tid (Env.read_theory why3_env path name) !t
    | Some _ -> ()

  let get_symbol th sym t =
    needed th t;
    match TMap.find_opt th !t with
    | None -> failwith (Format.asprintf "theory with symbol %s not loaded" sym)
    | Some t ->
      (* Format.printf "--- get symbol@.";
         Wstdlib.Mstr.bindings t.Theory.th_export.ns_ls
         |> List.iter (fun (k, v) ->
                Format.printf "%s %a@." k Why3.Pretty.print_ls v);
         Format.printf "---@."; *)
      Theory.ns_find_ls t.Theory.th_export [sym]

  let get_type_symbol th sym t =
    needed th t;
    match TMap.find_opt th !t with
    | None ->
      failwith (Format.asprintf "theory with type symbol %s not loaded" sym)
    | Some t -> Theory.ns_find_ts t.Theory.th_export [sym]
end

type env = {
  mutable names : Term.lsymbol SMap.t;
  theories : Theories.t; (* TODO consider making this global for performance *)
  tenv : typ SMap.t;
  quantified : Term.vsymbol SMap.t;
  letbound : Term.vsymbol SMap.t;
}

let type_to_why3 env (t : typ) =
  match t with
  | Unit -> Ty.ty_tuple []
  | List_int ->
    Theories.(needed int env.theories);
    Ty.ty_app Theories.(get_type_symbol list "list" env.theories) [Ty.ty_int]
  | Int ->
    Theories.(needed int env.theories);
    Ty.ty_int
  | Bool ->
    Theories.(needed bool env.theories);
    Ty.ty_bool
  | Lamb -> Ty.ty_int
  | TVar _ ->
    (* failwith "unknown type" *)
    (* default to int *)
    Theories.(needed int env.theories);
    Ty.ty_int

(*
(* this is needed because we use strings for names, while why3 uses refs *)
let build_name_map env =
  (* let don't_load_theories_now = create_env SMap.empty SMap.empty [] in *)
  object
    inherit [_] reduce_spec
    method zero = SMap.empty
    method plus = SMap.merge_disjoint

    method! visit_Var _ v =
      let ty1 = SMap.find_opt v env.tenv |> Option.value ~default:Int in
      (* let ty = type_to_why3 don't_load_theories_now ty1 in *)
      let ty = type_to_why3 en ty1 in
      SMap.singleton v (Term.create_lsymbol (Ident.id_fresh v) [] (Some ty))

    method! visit_TApp _ v args = failwith ""
    (* let ls =
         Term.create_lsymbol
           (Ident.id_fresh pure_fn.pf_name)
           (List.map snd params)
           (Some (type_to_why3 env pure_fn.pf_ret_type))
       in *)
  end
  *)

(* let find_needed_theories =
     object
       inherit [_] reduce_spec
       method zero = TSet.empty
       method plus = TSet.union

       (* method! visit_Num _ _ = TSet.singleton Theories.int *)
       (* method! visit_TLambda _ _ _ _ = TSet.singleton Theories.int *)

       (* method! visit_typ _ t = match t with Int -> TSet.singleton int *)

       method! visit_pi _ p =
         match p with
         | True -> TSet.empty
         | False
         | Atomic (_, _, _)
         | And (_, _)
         | Or (_, _)
         | Imply (_, _)
         | Not _
         | Predicate (_, _) ->
           failwith "pi"

       method! visit_term _ t =
         match t with
         | UNIT -> TSet.empty
         | TTrue | TFalse | Nil | Num _ | Var _
         | Plus (_, _)
         | Minus (_, _)
         | Rel (_, _, _)
         | TAnd (_, _)
         | TPower (_, _)
         | TTimes (_, _)
         | TDiv (_, _)
         | TOr (_, _)
         | TNot _
         | TApp (_, _)
         | TCons (_, _)
         | TLambda (_, _, _)
         | TList _ | TTupple _ ->
           failwith "term"
     end

   let find_needed_theories_typ (t : typ) =
     match t with
     | Unit -> TSet.empty
     | List_int -> TSet.of_list Theories.[list; int]
     | Int -> TSet.singleton Theories.int
     | Bool -> TSet.singleton Theories.bool
     | Lamb ->
       (* lambdas are encoded as ints *)
       TSet.singleton Theories.int
     | TVar _ ->
       (* default to int... *)
       TSet.singleton Theories.int *)

let rec term_to_why3 env (t : term) =
  Format.printf "term %s@." (Pretty.string_of_term t);
  match t with
  | UNIT -> (Term.t_tuple [], Unit)
  | Num i ->
    Theories.(needed int env.theories);
    (Term.t_nat_const i, Int)
  | TLambda (_, _, _) ->
    Theories.(needed int env.theories);
    (Term.t_nat_const (Subst.hash_lambda t), Int)
  | Var v when SMap.mem v env.tenv ->
    let ty1 = SMap.find v env.tenv (* |> Option.value ~default:Int *) in
    let ty = type_to_why3 env ty1 in
    let name =
      match SMap.find_opt v env.quantified with
      | Some v -> Term.t_var v
      | None ->
        (match SMap.find_opt v env.names with
        | None ->
          let name = Ident.id_fresh v in
          let sym = Term.create_lsymbol name [] (Some ty) in
          env.names <- SMap.add v sym env.names;
          Term.t_app sym [] (Some ty)
        | Some vv -> Term.t_app vv [] (Some ty))
    in
    (* Format.printf "var %s : %s@." v
       Pretty.(
         string_of_option string_of_type (SMap.find_opt v env.tenv)); *)
    (name, ty1)
  | Var v -> failwith (Format.asprintf "variable %s has no type" v)
  | Plus (a, b) ->
    let a1, _ = term_to_why3 env a in
    let b1, _ = term_to_why3 env b in
    ( Term.t_app_infer Theories.(get_symbol int "infix +" env.theories) [a1; b1],
      Int )
  | Minus (a, b) ->
    let a1, _ = term_to_why3 env a in
    let b1, _ = term_to_why3 env b in
    ( Term.t_app_infer Theories.(get_symbol int "infix -" env.theories) [a1; b1],
      Int )
  | Rel (EQ, a, b) ->
    let a1, t1 = term_to_why3 env a in
    let b1, t2 = term_to_why3 env b in
    ( (match (t1, t2) with
      | Int, Int ->
        Term.fs_app
          (* Theories.(get_symbol int "infix =" env.theories) *)
          Theories.(get_symbol extras "eqi" env.theories)
          [a1; b1] Ty.ty_bool
      | Bool, Bool ->
        Term.t_app_infer
          Theories.(get_symbol extras "eqb" env.theories)
          [a1; b1]
      | _, _ ->
        failwith
          (Format.asprintf "equality not defined between types %s and %s"
             (Pretty.string_of_type t1) (Pretty.string_of_type t2))),
      Bool )
  | Rel (GT, a, b) ->
    let a1, _ = term_to_why3 env a in
    let b1, _ = term_to_why3 env b in
    ( Term.t_app_infer Theories.(get_symbol int "infix >" env.theories) [a1; b1],
      Bool )
  | Rel (LT, a, b) ->
    let a1, _ = term_to_why3 env a in
    let b1, _ = term_to_why3 env b in
    ( Term.t_app_infer Theories.(get_symbol int "infix <" env.theories) [a1; b1],
      Bool )
  | Rel (GTEQ, a, b) ->
    let a1, _ = term_to_why3 env a in
    let b1, _ = term_to_why3 env b in
    ( Term.t_app_infer Theories.(get_symbol int "infix >=" env.theories) [a1; b1],
      Bool )
  | Rel (LTEQ, a, b) ->
    let a1, _ = term_to_why3 env a in
    let b1, _ = term_to_why3 env b in
    ( Term.t_app_infer Theories.(get_symbol int "infix <=" env.theories) [a1; b1],
      Bool )
  | TTrue ->
    Theories.(needed bool env.theories);
    (Term.t_bool_true, Bool)
  | TFalse ->
    Theories.(needed bool env.theories);
    (Term.t_bool_false, Bool)
  | TAnd (a, b) ->
    let a1, _ = term_to_why3 env a in
    let b1, _ = term_to_why3 env b in
    ( Term.t_app_infer Theories.(get_symbol bool "andb" env.theories) [a1; b1],
      Bool )
    (* Term.fs_app  [] Ty.ty_bool *)
    (* Term.t_and (term_to_why3 env a) (term_to_why3 env b) *)
  | TOr (a, b) ->
    (* Term.t_and (term_to_why3 env a) (term_to_why3 env b) *)
    let a1, _ = term_to_why3 env a in
    let b1, _ = term_to_why3 env b in
    ( Term.t_app_infer Theories.(get_symbol bool "orb" env.theories) [a1; b1],
      Bool )
  | TNot a ->
    (* Term.t_not (term_to_why3 env a) *)
    let a1, _ = term_to_why3 env a in
    (Term.t_app_infer Theories.(get_symbol bool "notb" env.theories) [a1], Bool)
  | TCons (a, b) ->
    let a1, _ = term_to_why3 env a in
    let b1, _ = term_to_why3 env b in
    (* let open Pretty in *)
    (* Format.printf "cons %s %s @." (string_of_term a) (string_of_term b); *)
    ( Term.t_app
        Theories.(get_symbol list "Cons" env.theories)
        [a1; b1]
        (Some (type_to_why3 env List_int)),
      List_int )
    (* inferring types leads to issues reconciling types between systems *)
    (* Term.t_app_infer
       (get_theory_symbol env.list_theory "Cons")
       [term_to_why3 env a; term_to_why3 env b] *)
  | Nil ->
    (* Term.t_app_infer (get_theory_symbol env.list_theory "Nil") [] *)
    ( Term.t_app
        Theories.(get_symbol list "Nil" env.theories)
        []
        (Some (type_to_why3 env List_int)),
      List_int )
  | TApp (s, args) when Globals.is_pure_fn_defined s ->
    let args1 = List.map (term_to_why3 env) args |> List.map fst in
    let defn = Globals.pure_fn s in
    let ret_typ = type_to_why3 env defn.pf_ret_type in
    Format.printf "app %s@." s;
    (Term.t_app (SMap.find s env.names) args1 (Some ret_typ), defn.pf_ret_type)
  | TApp (s, _) -> failwith (Format.asprintf "unknown function term %s" s)
  | TPower (_, _) -> failwith "TPower nyi"
  | TTimes (_, _) -> failwith "TTimes nyi"
  | TDiv (_, _) -> failwith "TDiv nyi"
  | TList _ -> failwith "TList nyi"
  | TTupple _ -> failwith "TTupple nyi"

let rec pi_to_why3 env (pi : pi) =
  Format.printf "pi %s@." (Pretty.string_of_pi pi);
  match pi with
  | True -> Term.t_true
  | False -> Term.t_false
  | Atomic (EQ, a, b) ->
    let a1, t1 = term_to_why3 env a in
    let b1, t2 = term_to_why3 env b in
    (match (t1, t2) with
    | Bool, Bool ->
      (* Term.t_app_infer Theories.(get_symbol extras "eqb" env.theories) [a1; b1] *)
      Term.t_equ a1 b1
    | _, _ ->
      (* this seems to be ok *)
      Term.t_equ a1 b1)
  | Atomic (GT, a, b) ->
    let a1, _ = term_to_why3 env a in
    let b1, _ = term_to_why3 env b in
    Term.t_app_infer Theories.(get_symbol int "infix >" env.theories) [a1; b1]
  | Atomic (LT, a, b) ->
    let a1, _ = term_to_why3 env a in
    let b1, _ = term_to_why3 env b in
    Term.t_app_infer Theories.(get_symbol int "infix <" env.theories) [a1; b1]
  | Atomic (GTEQ, a, b) ->
    let a1, _ = term_to_why3 env a in
    let b1, _ = term_to_why3 env b in
    Term.t_app_infer Theories.(get_symbol int "infix >=" env.theories) [a1; b1]
  | Atomic (LTEQ, a, b) ->
    let a1, _ = term_to_why3 env a in
    let b1, _ = term_to_why3 env b in
    Term.t_app_infer Theories.(get_symbol int "infix <=" env.theories) [a1; b1]
  | And (a, b) -> Term.t_and (pi_to_why3 env a) (pi_to_why3 env b)
  | Or (a, b) -> Term.t_or (pi_to_why3 env a) (pi_to_why3 env b)
  | Imply (a, b) -> Term.t_implies (pi_to_why3 env a) (pi_to_why3 env b)
  | Not a -> Term.t_not (pi_to_why3 env a)
  | Predicate (_, _) -> failwith "nyi Predicate"

let rec expr_to_why3 env e =
  Format.printf "expr %s@." (Pretty.string_of_core_lang e);
  match e with
  | CLet (v, e1, a) ->
    Format.printf "??%s@." (Pretty.string_of_core_lang e);
    let h = Term.create_vsymbol (Ident.id_fresh v) (type_to_why3 env Int) in
    let e2 = expr_to_why3 env e1 in
    let a1 = expr_to_why3 { env with letbound = SMap.add v h env.letbound } a in
    (* a1 *)
    Term.t_let e2 (Term.t_close_bound h a1)
    (* failwith "sdasd" *)
  | CValue t ->
    let t, _ty = term_to_why3 env t in
    t
  | CIfELse (_, _, _) -> failwith "unimplemented CIfELse"
  | CFunCall (s, args) when Globals.is_pure_fn_defined s ->
    (* SMap.mem s env.names *)
    let args1 = List.map (term_to_why3 env) args |> List.map fst in
    let fn = Globals.pure_fn s in
    (* Format.printf "%s@." s; *)
    Term.t_app (SMap.find s env.names) args1
      (Some (type_to_why3 env fn.pf_ret_type))
    (* failwith "unimplemented CFunCall" *)
  | CFunCall (s, _) -> failwith (Format.asprintf "unknown function %s" s)
  | CWrite (_, _) -> failwith "unimplemented CWrite"
  | CRef _ -> failwith "unimplemented CRef"
  | CRead _ -> failwith "unimplemented CRead"
  | CAssert (_, _) -> failwith "unimplemented CAssert"
  | CPerform (_, _) -> failwith "unimplemented CPerform"
  | CMatch (_, scr, None, [], cases) ->
    (* x :: xs -> e is represented as ("::", [x, xs], e) *)
    (* and constr_cases = (string * string list * core_lang) list *)
    Term.t_case (expr_to_why3 env scr)
      (List.map
         (fun (constr, args, body) ->
           let pat =
             match (constr, args) with
             | "::", [x; y] ->
               let h =
                 Term.create_vsymbol (Ident.id_fresh x) (type_to_why3 env Int)
               in
               let t =
                 Term.create_vsymbol (Ident.id_fresh y)
                   (type_to_why3 env List_int)
               in
               (* TODO nested patterns not supported *)
               Term.pat_app
                 Theories.(get_symbol list "Cons" env.theories)
                 [Term.pat_var h; Term.pat_var t]
                 (type_to_why3 env List_int)
             | "[]", [] ->
               Term.pat_app
                 Theories.(get_symbol list "Nil" env.theories)
                 []
                 (type_to_why3 env List_int)
             | c, _ -> failwith (Format.asprintf "unhandled constr %s" c)
           in
           Term.t_close_branch pat (expr_to_why3 env body))
         cases)
  | CMatch (_, _, _, _, _) -> failwith "unimplemented effect CMatch"
  | CResume _ -> failwith "unimplemented CResume"
  | CLambda (_, _, _) -> failwith "unimplemented CLambda"

let pure_fn_to_logic_fn env pure_fn =
  let params =
    List.map (fun (p, t) -> (p, type_to_why3 env t)) pure_fn.pf_params
  in
  (* on first translation, we store the name *)
  let fn_name =
    match SMap.find_opt pure_fn.pf_name env.names with
    | None ->
      let ls =
        Term.create_lsymbol
          (Ident.id_fresh pure_fn.pf_name)
          (List.map snd params)
          (Some (type_to_why3 env pure_fn.pf_ret_type))
      in
      env.names <- SMap.add pure_fn.pf_name ls env.names;
      (* failwith "asd"; *)
      ls
    | Some ls -> ls
  in
  let params =
    List.map
      (fun (v, t) -> (v, Term.create_vsymbol (Ident.id_fresh v) t))
      params
  in
  let params_l = List.map snd params in
  (* let params_m = SMap.of_list params in *)
  Decl.make_ls_defn fn_name params_l (expr_to_why3 env pure_fn.pf_body)

let attempt_proof task1 =
  (* Format.printf "task: %a@." Pretty.print_task task1; *)
  let prover_z3 = "Z3" in
  let prover_alt_ergo = "Alt-Ergo" in
  [prover_alt_ergo; prover_z3]
  |> List.exists (fun prover ->
         ensure_prover_loaded prover;
         let pc = get_prover_config prover in
         let driver = load_prover_driver pc prover in
         let result1 =
           Call_provers.wait_on_call
             (Driver.prove_task
                ~limit:
                  {
                    Call_provers.empty_limit with
                    Call_provers.limit_time = 0.5;
                  }
                ~config:why3_config_main ~command:pc.Whyconf.command driver
                task1)
         in
         (* Format.printf "%s: %a@." prover
            (Call_provers.print_prover_result ?json:None)
            result1; *)
         match result1.pr_answer with Valid -> true | _ -> false)

let prove tenv qtf f =
  let env =
    {
      tenv;
      theories = Theories.create ();
      names = SMap.empty;
      quantified = SMap.empty;
      letbound = SMap.empty;
    }
  in

  (* after pure functions are translated, so the names are in the env *)
  (* before adding theories to the task *)
  (* requires theories to build the formula *)
  let ass, goal = f env in

  (* let theories_needed =
       let ts =
         find_needed_theories#visit_pi () f
         :: List.map
              (fun (_, v) -> find_needed_theories#visit_core_lang () v.pf_body)
              (Globals.pure_fns ())
       in
       List.fold_right TSet.union ts TSet.empty
     in *)

  (* let int = (["int"], "Int")
     let bool = (["bool"], "Bool")
     let list = (["list"], "List")
     let extras = (["extras"], "Extras") *)

  (* set up task *)
  let task1 : Task.task = None in

  (* add theories to the task *)
  (* TODO with z3, somehow the builtin theory containing things like unit is not loaded unless at least one other theory is pulled in, so use the int theory every time for now *)
  (* use all theories. analyzing the formula for theories requires building it first, which is cyclic. this used to be broken by loading theories on demand, however *)
  (* List.iter
       (fun t -> Theories.(needed t env.theories))
       Theories.[int; bool; list; extras];
     Format.printf "initial@.";
     (* Theories.(needed int env.theories); *)
     TSet.iter (fun t -> Theories.needed t env.theories) theories_needed; *)
  let task1 =
    Theories.fold
      (fun (tid, why_th) task ->
        Format.printf "theory added: %s@." (string_of_tid tid);
        Task.use_export task why_th)
      env.theories task1
  in

  (* handle names, which requires types, which means theories must be loaded by now *)
  (* let names =
       let vis = build_name_map env in
       SMap.merge_all_disjoint
         (vis#visit_pi () f
         :: List.map
              (fun (_, v) -> vis#visit_core_lang () v.pf_body)
              (Globals.pure_fns ()))
     in *)
  let quantified =
    qtf
    |> List.map (fun v ->
           let ty = SMap.find_opt v tenv |> Option.value ~default:Int in
           (v, Term.create_vsymbol (Ident.id_fresh v) (type_to_why3 env ty)))
    |> List.to_seq |> SMap.of_seq
  in

  (* { initial with quantified } *)

  (* let env = create_env names tenv qtf in *)
  let env = { env with quantified } in

  (* let create_env names tenv qtf = *)

  (* add pure functions *)
  (* before adding theories to the task, as translation loads them *)
  Format.printf "adding pure fns@.";
  let task1 =
    let fns =
      List.map
        (fun (_k, v) ->
          Format.printf "translating %s@." _k;
          pure_fn_to_logic_fn env v)
        (Globals.pure_fns ())
    in
    match fns with [] -> task1 | _ :: _ -> Task.add_logic_decl task1 fns
  in

  (* Format.printf "task: %a@." Why3.Pretty.print_task task1; *)

  (* add variables as parameters *)
  let task1 =
    List.fold_right
      (fun (_k, v) t -> Task.add_param_decl t v)
      (SMap.bindings env.names) task1
  in

  (* Format.printf "tenv: %s@." (Pretty.string_of_typ_env tenv); *)
  (* Format.printf "assumptions: %a@." Pretty.print_term ass; *)
  (* Format.printf "goal: %a@." Pretty.print_term goal; *)
  let task1 : Task.task =
    Task.add_prop_decl task1 Decl.Paxiom
      (Decl.create_prsymbol (Ident.id_fresh "ass1"))
      ass
  in
  let task1 : Task.task =
    Task.add_prop_decl task1 Decl.Pgoal
      (Decl.create_prsymbol (Ident.id_fresh "goal1"))
      goal
  in
  Format.printf "--- prove@.";
  attempt_proof task1

let cache : (pi * string list * pi, bool) Hashtbl.t = Hashtbl.create 10

let memo k f =
  match Hashtbl.find_opt cache k with
  | None ->
    let r = f () in
    Hashtbl.add cache k r;
    r
  | Some r -> r

let suppress_error_if_not_debug f =
  if Debug.in_debug_mode () then f ()
  else
    try f ()
    with e ->
      (* the stack trace printed is not the same (and is much less helpful) if the exception is caught *)
      Debug.debug ~at:1 ~title:"an error occurred, assuming proof failed" "%a"
        Exn_printer.exn_printer e;
      (* Printexc.print_backtrace stdout; *)
      false

let entails_exists tenv left ex right =
  let@ _ = memo (left, ex, right) in
  let@ _ = suppress_error_if_not_debug in
  prove tenv ex (fun env ->
      ( pi_to_why3 env left,
        Term.t_exists_close
          (SMap.bindings env.quantified |> List.map snd)
          [] (pi_to_why3 env right) ))
