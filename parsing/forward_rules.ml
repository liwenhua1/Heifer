
open Hiptypes
open Pretty
open Normalize


let concatenateSpecsWithEvent (current:spec list) (event:stagedSpec list) :  spec list = 
  List.map (fun a -> List.append a event) current

let concatenateEventWithSpecs  (event:spec) (current:spec list) :  spec list = 
  List.map (fun a -> List.append event a ) current


let concatenateSpecsWithSpec (current:spec list) (event:spec list) :  spec list = 
  let temp  = List.map (fun a -> concatenateSpecsWithEvent current a) event in 
  List.flatten temp

let rec retrieve_return_value (spec:spec) : term = 
  match spec with 
  | [] -> failwith "retrieve_return_value empty spec"
  | [HigherOrder (_, _, _, retN)] 
  | [NormalReturn (_, _, retN)] 
  | [RaisingEff(_, _, _, retN)] -> retN
  | _ :: xs -> retrieve_return_value xs 

let rec replace_return_value (t:term) (spec:spec) : spec = 
  match spec with 
  | [] -> failwith "replace_return_value empty spec"
  | [HigherOrder (p, h, i, _)] -> [HigherOrder (p, h, i, t)]
  | [NormalReturn (p, h, _)] -> [NormalReturn (p, h, t)]
  | [RaisingEff(p, h, i, _)] -> [RaisingEff (p, h, i, t)]
  | s :: ss -> s :: replace_return_value t ss

(** Environment used for forward verification *)
type fvenv = {
  (* defined methods, may be added to if lambdas are given names *)
  fv_methods : meth_def SMap.t;

  (* definitions of lambda terms (which are opaque to smt),
     added to when lambdas are defined *)
  fv_lambda : meth_def SMap.t;

  (* methods which need to be changed if a lambda is changed, e.g. by substitution *)
  fv_lambda_names : (string * string) list;

  (* proof obligations generated by defining lambdas with specifications.
     we don't check them immediately to avoid a cyclic dependency with entailment.
     this allows us to propagate them out. *)
  fv_lambda_obl : (disj_spec * disj_spec) list;
}

let create_fv_env fv_methods = {
  fv_methods;
  fv_lambda = SMap.empty;
  fv_lambda_obl = [];
  fv_lambda_names = [];
}

let retrieveSpecFromEnv (fname: string) (env:fvenv) : (string list * spec list) option = 
  SMap.find_opt fname env.fv_methods
  |> Option.map (fun m -> (m.m_params, Option.get m.m_spec))


let bindFormalNActual (formal: string list) (actual: core_value list) : ((string * core_value) list)= 
  List.map2 (fun a b -> (a, b)) formal actual

let bindNewNames (formal: string list) (actual: string list) : ((string * string) list)= 
  List.map2 (fun a b -> (a, b)) formal actual




  







let rec getExistientalVar (spec:normalisedStagedSpec) : string list = 
  let (effS, normalS) = spec in 
  match effS with 
  | [] -> 
    let (ex, _, _, _) = normalS in ex 
  | eff :: xs -> 
    eff.e_evars @ getExistientalVar (xs, normalS)


let rec findNewName str vb_li =
    match vb_li with 
    | [] -> str 
    | (name, new_name) :: xs -> if String.compare name str == 0 then new_name else  findNewName str xs



let rec instantiateExistientalVar_aux (li:string list)   (bindings:((string * string) list)) : string list = 
  match li with 
  | [] -> []
  | str :: xs  -> 
    let str' = findNewName  str bindings in 
    str' :: instantiateExistientalVar_aux xs bindings


let rec instantiateExistientalVar 
  (spec:normalisedStagedSpec) 
  (bindings:((string * string) list)): normalisedStagedSpec = 

  let (effS, normalS)  =  spec  in 
  match effS with 
  | [] -> 

    (* print_endline ("nROMRAL STATGE"); *)
    let (ex, req, ens, ret) = normalS in 
    ([], (instantiateExistientalVar_aux ex bindings, req, ens, ret))
  | eff :: xs -> 
    (* print_endline ("EFF STATGE"); *)
    let (rest, norm') = instantiateExistientalVar (xs, normalS) bindings in 
    (({eff with e_evars = instantiateExistientalVar_aux eff.e_evars bindings}) :: rest, norm')


let instantiateExistientalVarSpec   (spec:spec) 
(bindings:((string * string) list)): spec = 
  let normalSpec = normalise_spec spec in 
  normalisedStagedSpec2Spec (instantiateExistientalVar normalSpec bindings)



let isFreshVar str : bool = 
  if String.length str < 1 then false 
  else 
    let a = String.get str 0 in 
    (*let b = String.get str 1 in *)
    if a='f' (*&& b ='f'*) then true else false 

let () = assert (isFreshVar "f10" ==true )
let () = assert (isFreshVar "s10" ==false )

(** substitutes existentials with fresh variables *)
let renamingexistientalVar (specs:spec list): spec list = 
  List.map (
    fun spec -> 
      let normalSpec = normalise_spec spec in 
      let existientalVar = getExistientalVar normalSpec in 
      let newNames = List.map (fun n -> (verifier_getAfreeVar n)) existientalVar in 
      let newNameTerms = List.map (fun a -> Var a) newNames in 
      let bindings = bindNewNames existientalVar newNames in 
      let temp = instantiateExistientalVar normalSpec bindings in 
      let bindings = bindFormalNActual existientalVar newNameTerms in 
      instantiateSpec bindings (normalisedStagedSpec2Spec temp)
  ) specs

(** substitutes existentials with fresh variables, and the resulting formula has no quantifiers *)
let freshen (specs:disj_spec): disj_spec = 
  renamingexistientalVar specs
  |> List.map (List.filter (function Exists _ -> false | _ -> true))


let instantiate_higher_order_functions fname fn_args (spec:disj_spec) : disj_spec =
  let rec loop (acc:stagedSpec Acc.t list) (ss:spec) =
    match ss with
    | [] -> List.map Acc.to_list acc
    | s :: ss1 ->
      (match s with
      | Exists _ | Require (_, _) | NormalReturn _ | RaisingEff _ ->
        loop (List.map (fun tt -> Acc.add s tt) acc) ss1
      | HigherOrder (p, h, (name, args), ret) ->
        let matching = List.find_map (fun (mname, mspec) ->
          match mspec with
          | Some (mparams, msp) when String.equal mname name && List.length mparams = List.length args ->
          Some (mparams, msp)
          | _ -> None) fn_args
        in
        (match matching with
        | None ->
          loop (List.map (fun tt -> Acc.add s tt) acc) ss1
        | Some (mparams, mspec) ->
          let bs = bindFormalNActual mparams args in
          let instantiated = instantiateSpecList bs (renamingexistientalVar mspec)
            (* replace return value with whatever x was replaced with *)
          in
          let res = acc |> List.concat_map (fun tt ->
            List.map (fun dis -> tt |> Acc.add (NormalReturn (p, h, UNIT)) |> Acc.add_all dis) instantiated)
          in
          let ret1 = match ret with Var s -> s | _ -> failwith (Format.asprintf "return value of ho %s was not a var" (string_of_term ret)) in
          (* instantiate the return value in the remainder of the input before using it *)
          let ss2 =
            let bs = List.map (fun s -> (ret1, retrieve_return_value s)) instantiated in
            instantiateSpec bs ss1
          in
          loop res ss2))
  in
  (* in acc, order of disjuncts doesn't matter *)
  let res = List.concat_map (fun ss -> loop [Acc.empty] ss) spec in
info ~title:(Format.asprintf "instantiate higher order function: %s" fname) "args: %s\n\n%s\n==>\n%s" (string_of_list (string_of_pair Fun.id (string_of_option (string_of_pair (string_of_list Fun.id) (string_of_list string_of_spec)))) fn_args) (string_of_disj_spec spec) (string_of_disj_spec res);
  res


let rec lookforHandlingCases ops (label:string) = 
  match ops with 
  | [] -> None
  | (str, arg, spec, expr)::xs -> 
    if String.compare label str == 0 
    then Some (arg, expr) 
    else lookforHandlingCases xs label 

let (continueationCxt: ((spec list * string * (string * core_lang) * core_handler_ops) list) ref)  = ref [] 

(** Like concat_map, but threads an extra "environment" argument through which can be updated by the function *)
let concat_map_state env f xs =
  let r, env =
    List.fold_right (fun c (t, env) ->
      let r, e1 = f c env
      in (r :: t, e1)
    ) xs ([], env)
  in
  List.concat r, env

let%expect_test _ =
  let r, e = (concat_map_state 0 (fun x e -> [x; x * 3], e + 1) [1; 2; 3]) in
  Format.printf "%s %d@." (string_of_list string_of_int r) e;
  [%expect
    {| [1; 3; 2; 6; 3; 9] 3 |}]

let primitives = ["+"; "-"; "="; "not"; "::"; "&&"; "||"; ">"; "<"; ">="; "<="]

let rec handling_spec env (spec:normalisedStagedSpec) (normal:(string * core_lang)) (ops:core_handler_ops) : spec list * fvenv = 
  
  (*print_endline("\nhandling_spec =====> " ^ string_of_spec (normalisedStagedSpec2Spec spec));
*)
  let (normFormalArg, expRet) = normal in 
  let (effS, normalS) = spec in 
  match effS with 
  | [] -> 
    let (existiental, (p1, h1), (p2, h2), ret) = normalS in 

    let bindings = bindFormalNActual [normFormalArg] [ret] in 
    let current = [Exists existiental; Require(p1, h1); NormalReturn(p2, h2, ret)] in 
    let temp, env = infer_of_expression env [current] expRet in 
    instantiateSpecList bindings temp, env

    
  | x :: xs -> 
    let ret = match x.e_ret with 
    | Var ret -> ret
    | _ -> failwith "effect return is not var"

    in

    let (label, effactualArgs) = x.e_constr in
    match lookforHandlingCases ops label with 
    | None ->
      let r, env = handling_spec env (xs, normalS) normal ops in
      concatenateEventWithSpecs (effectStage2Spec [x]) (r), env
    | Some (effFormalArg, exprEff) -> 
      let bindings = 
        match effFormalArg, effactualArgs with 
        | _, [] | None, _ -> [] 
        | Some e, effactualArg ::_ -> [(e, effactualArg)]
      in 
    let (p1, h1) = x.e_pre in
    let (p2, h2) = x.e_post in
      let current = [Exists x.e_evars; Require(p1, h1); 
        NormalReturn(p2, h2, UNIT)] in  (* Var ret *)

      let continueation_spec = normalisedStagedSpec2Spec (xs, normalS) in 
      let () = continueationCxt := ([continueation_spec],  ret, normal, ops) :: !continueationCxt in 
      let temp, env = infer_of_expression env [current] exprEff in 
      let () = continueationCxt := List.tl (!continueationCxt) in 
      instantiateSpecList bindings temp, env


 
(* may update the environment because of higher order functions *)
and infer_of_expression (env:fvenv) (current:disj_spec) (expr:core_lang): disj_spec * fvenv = 
  (* TODO infer_of_expression is likely O(n^2) due to appending at the end *)
  let res, env =
    match expr with
    | CValue v -> 
      let event = NormalReturn (True, EmptyHeap, v) in 
      concatenateSpecsWithEvent current [event], env

    | CLet (str, expr1, expr2) ->
      let phi1, env = infer_of_expression env current expr1 in 
      phi1 |> concat_map_state env (fun spec env -> 
        (*print_endline (string_of_spec(spec)); *)
        let retN = retrieve_return_value spec in 
        match retN with 
        | UNIT ->
          infer_of_expression env [spec] expr2
        (*| Var freshV -> 
          if String.compare str "_" == 0 then infer_of_expression env [spec] expr2
          
          else if String.compare str "i" == 0 || String.compare str "j" == 0  then 
            (
            (*print_endline ("replacing " ^ freshV ^ " with " ^str);
            print_endline ("spec   " ^ string_of_spec spec);*)
            (* instantiate the exist value first *)
            let bindings = bindNewNames [freshV] [str] in 
            let spec' = instantiateExistientalVarSpec spec bindings in 
            (*print_endline ("spec'  " ^ string_of_spec spec');*)
            (* instantiate the terms value first *)
            let bindings = bindFormalNActual [freshV] [Var str] in 
            let spec' = instantiateSpec bindings spec' in 
            (*print_endline ("spec'' " ^ string_of_spec spec'); *)
            (*let spec' = removeExist [spec'] freshV in *)
            infer_of_expression env [spec'] expr2)
          
          else 
            let bindings = bindFormalNActual [str] [retN] in 
            let phi2 = infer_of_expression env [spec] expr2 in 
            instantiateSpecList bindings phi2
            *)
        | _ when String.equal str "_" ->
          infer_of_expression env [spec] expr2
        | _ ->

          (* rather than create an existential, we substitute the new variable away *)
          let bindings = bindFormalNActual [str] [retN] in 

          (* if expr1 is a lambda, copy its binding to the method env before processing expr2 *)
          let env =
            match phi1 with
            | [p] ->
              (match retrieve_return_value p with
              | TLambda n ->
                let lspec = SMap.find n env.fv_lambda in
                (* rename, so created predicate will match *)
                let lspec = { lspec with m_name = str } in
                { env with fv_methods = env.fv_methods |> SMap.add str lspec;
                  fv_lambda_names = (n, str) :: env.fv_lambda_names }
              | _ -> env)
            | _ -> env
          in

          let phi2, env, lambdas_in_expr2 =
            let phi2, env1 = infer_of_expression env [freshNormalReturnSpec] expr2 in
            let ll =
              let prev = SMap.bindings env.fv_lambda |> List.map fst |> SSet.of_list in
              let curr = SMap.bindings env1.fv_lambda |> List.map fst |> SSet.of_list in
              SSet.diff curr prev
            in
            phi2, env1, ll
          in

          (* Here, we only instantiate the expr2 and *)
          let phi2' = instantiateSpecList bindings phi2 in

          (* also substitute inside the specs of lambdas defined in expr2. this has to be done after expr2 is inferred (as then we will know which lambda specs to change), so we need to fix all methods which lambdas have been bound to as well *)
          let env =
            let fv_lambda =
              env.fv_lambda |> SMap.mapi (fun k v -> if SSet.mem k lambdas_in_expr2 then { v with m_spec = Option.map (fun s -> instantiateSpecList bindings s) v.m_spec } else v)
            in
            let fv_methods =
              let methods_to_change =
                env.fv_lambda_names |> List.filter_map (fun (n, m) -> if SSet.mem n lambdas_in_expr2 then Some m else None) |> SSet.of_list
              in
              SMap.mapi (fun k v -> if SSet.mem k methods_to_change then { v with m_spec = Option.map (fun s -> instantiateSpecList bindings s) v.m_spec } else v) env.fv_methods
            in
            { env with fv_lambda; fv_methods }
          in

          (* concat spec -- the spec for expr 1 -- in front *)
          concatenateSpecsWithSpec [spec] phi2', env
        )
    | CRef v -> 
      let freshVar = verifier_getAfreeVar "ref" in 
      let event = NormalReturn (True, PointsTo(freshVar, v), Var freshVar) in 
      concatenateSpecsWithEvent current [Exists [freshVar];event], env


    | CRead str -> 
      let freshVar = verifier_getAfreeVar str in 
      let event = [Exists [freshVar];Require(True, PointsTo(str, Var freshVar)); 
        NormalReturn (True, PointsTo(str, Var freshVar) , Var freshVar)] in 
      concatenateSpecsWithEvent current event, env


    | CAssert (p, h) -> 
      let temp = concatenateSpecsWithEvent current [Require(p, h)] in 
      concatenateSpecsWithEvent temp [(NormalReturn(p, h, UNIT))], env

    | CPerform (label, arg) -> 
          
      let arg = 
        match arg with 
        | Some v -> [v]
        | _ -> []
      in 
      let freshVar = verifier_getAfreeVar "res" in 
      (* after adding the perfome stage, we need to add a normal return. *)
      concatenateSpecsWithEvent current 
      [Exists [freshVar];RaisingEff(True, EmptyHeap, (label,arg), Var freshVar);
      NormalReturn (True, EmptyHeap, Var freshVar)], env


    | CResume v ->  
        (match !continueationCxt with 
        | [] -> failwith "resume in a wrong context"
        | (continue_specs, ret, normal, ops) :: _ -> 

            (*
            print_endline ("C = " ^ string_of_spec continue_spec);
            *)
            let bindings = bindFormalNActual [ret] [v] in 
            (* instantiate the rest of the stages *)

            (*print_endline (string_of_spec_list continue_specs); *)
            let continue_specs = renamingexistientalVar continue_specs in 
            (*print_endline ("=====\n" ^string_of_spec_list continue_specs);*)
      
            let instantiatedSpecs =  instantiateSpecList bindings continue_specs in 
            (* instantiate the pre stages *)
            let instantiatedCurrent =  instantiateSpecList bindings current in 
            (* after instantiate the pre stages, remove the existential quantifier for ret *)
            let instantiatedCurrent' = removeExist instantiatedCurrent ret in 

            let temp, env = 
              concat_map_state env (fun a env -> handling_spec env (normalise_spec a)  normal ops) instantiatedSpecs
            in 
            concatenateSpecsWithSpec instantiatedCurrent' temp, env
        )

    | CFunCall (fname, actualArgs) -> 
      (match List.mem fname primitives with
      | true ->
        (match fname, actualArgs with
        | "+", [x1; x2] ->
          let event = NormalReturn (True, EmptyHeap, Plus(x1, x2)) in
          concatenateSpecsWithEvent current [event], env
        | "-", [x1; x2] ->
          let event = NormalReturn (True, EmptyHeap, Minus(x1, x2)) in
          concatenateSpecsWithEvent current [event], env
        | "=", [x1; x2] ->
          (* let event = NormalReturn (Atomic (EQ, x1, x2), EmptyHeap, Eq (x1, x2)) in *)
          let event = NormalReturn (True, EmptyHeap, Rel (EQ, x1, x2)) in
          concatenateSpecsWithEvent current [event], env
        | "not", [x1] ->
          let event = NormalReturn (True, EmptyHeap, TNot (x1)) in
          concatenateSpecsWithEvent current [event], env
        | "&&", [x1; x2] ->
          let event = NormalReturn (True, EmptyHeap, TAnd (x1, x2)) in
          concatenateSpecsWithEvent current [event], env
        | "||", [x1; x2] ->
          let event = NormalReturn (True, EmptyHeap, TOr (x1, x2)) in
          concatenateSpecsWithEvent current [event], env
        | ">", [x1; x2] ->
          let event = NormalReturn (True, EmptyHeap, Rel (GT, x1, x2)) in
          concatenateSpecsWithEvent current [event], env
        | "<", [x1; x2] ->
          let event = NormalReturn (True, EmptyHeap, Rel (LT, x1, x2)) in
          concatenateSpecsWithEvent current [event], env
        | ">=", [x1; x2] ->
          let event = NormalReturn (True, EmptyHeap, Rel (GTEQ, x1, x2)) in
          concatenateSpecsWithEvent current [event], env
        | "<=", [x1; x2] ->
          let event = NormalReturn (True, EmptyHeap, Rel (LTEQ, x1, x2)) in
          concatenateSpecsWithEvent current [event], env
        | "::", [x1; x2] ->
          let event = NormalReturn (True, EmptyHeap, TApp ("cons", [x1; x2])) in
          concatenateSpecsWithEvent current [event], env
        | _ -> failwith (Format.asprintf "unknown primitive: %s, args: %s" fname (string_of_list string_of_term actualArgs)))
      | false ->
        let spec_of_fname =
          (match retrieveSpecFromEnv fname env with 
          | None ->
            let ret = verifier_getAfreeVar "ret" in
            [[Exists [ret]; HigherOrder (True, EmptyHeap, (fname, actualArgs), Var ret)]]
          | Some (formalArgs, spec_of_fname) -> 
            (* TODO should we keep existentials? *)
            let spec = renamingexistientalVar spec_of_fname in
            (* let spec = freshen spec_of_fname in *)
            (* Format.printf "after freshen: %s@." (string_of_disj_spec spec); *)
            if List.compare_lengths formalArgs actualArgs <> 0 then
              failwith (Format.asprintf "too few args. formals: %s, actual: %s@." (string_of_list Fun.id formalArgs) (string_of_list string_of_term actualArgs));
            let spec =
              (* we've encountered a function call, e.g. f x y.
                we look up the spec for f. say it looks like:
                  let f g h (*@ g$(...); ... @*) = ...
                we now want to instantiate g with the spec for x. *)
              let fnArgs = List.map2 (fun p x ->
                match x with
                | Var a ->
                  (p, retrieveSpecFromEnv a env)
                | TLambda _ ->
                  (* if a is a lambda, get its spec here *)
                  failwith "lambda case not implemented"
                | _ -> (p, None)
                ) formalArgs actualArgs
              in
              (* fnArgs is a map of g -> spec, h -> none, assuming h is not a functino *)
              instantiate_higher_order_functions fname fnArgs spec
            in
            (* Format.printf "after ho fns: %s@." (string_of_disj_spec spec); *)
            let bindings = bindFormalNActual (formalArgs) (actualArgs) in 
            let instantiatedSpec = instantiateSpecList bindings spec in 
            instantiatedSpec)
            (*print_endline ("====\n"^ string_of_spec_list spec_of_fname);*)
        in
        let _spec_of_fname =
          (* this is an alternative implementation for this whole case, which simply generates an uninterpreted function and lets the entailment procedure take care of unfolding (since the implementation above can be seen as unfolding once). unfortunately the handler reasoning in the effects work relies on unfolding in the forward reasoning, so we can't switch to it yet, but this implementation should work for higher-order *)
          let ret = verifier_getAfreeVar "ret" in
          [[Exists [ret]; HigherOrder (True, EmptyHeap, (fname, actualArgs), Var ret); NormalReturn (True,EmptyHeap, Var ret)]]
        in
        concatenateSpecsWithSpec current spec_of_fname, env)
    | CWrite  (str, v) -> 
      let freshVar = verifier_getAfreeVar "wr" in 
      let event = [Exists [freshVar];Require(True, PointsTo(str, Var freshVar)); 
                    NormalReturn (True, PointsTo(str, v), UNIT)] in 
      concatenateSpecsWithEvent current event, env


    | CIfELse (v, expr2, expr3) -> 
      let eventThen = NormalReturn (Atomic (EQ, v, TTrue), EmptyHeap, UNIT) in 
      let eventElse = NormalReturn (Not (Atomic (EQ, v, TTrue)), EmptyHeap, UNIT) in 
      let currentThen = concatenateSpecsWithEvent current [eventThen] in 
      let currentElse = concatenateSpecsWithEvent current [eventElse] in 
      let r1, env = infer_of_expression env currentThen expr2 in
      let r2, env = infer_of_expression env currentElse expr3 in
      r1 @ r2, env


    | CLambda (params, given_spec, body) ->
      let inferred, env = infer_of_expression env [[]] body in
      let inferred = inferred |> List.map (fun s -> s |> normalise_spec |> normalisedStagedSpec2Spec) in
      let lid = verifier_getAfreeVar "lambda" in
      debug ~title:(Format.asprintf "lambda spec %s" lid) "%s" (string_of_disj_spec inferred);
      let mdef = {
        m_name = lid;
        m_params = params; (* TODO unit param? *)
        m_spec = Some inferred; (* TODO given? *)
        m_body = body;
        m_tactics = [];
      } in
      let env = { env with fv_lambda = SMap.add lid mdef env.fv_lambda } in
      let env =
        match given_spec with
        | None -> env
        | Some g -> { env with fv_lambda_obl = (inferred, g) :: env.fv_lambda_obl }
      in
      let event = NormalReturn (True, EmptyHeap, TLambda lid) in 
      concatenateSpecsWithEvent current [event], env

    | CMatch (expr1, Some vcase, ops, []) ->
      (* effects *)
      let (normFormalArg, expRet) = vcase in
      let phi1, env = infer_of_expression env [freshNormalReturnSpec] expr1 in 
      let afterHandling, env =
        concat_map_state env (fun spec env -> 
          (*print_endline("\nCMatch =====> " ^ string_of_spec spec); *)
          let normalisedSpec= (normalise_spec spec) in 
          (* print_endline("\nnormaled =====> " ^ string_of_normalisedStagedSpec normalisedSpec);  *)
          handling_spec env normalisedSpec (normFormalArg, expRet) ops
        ) phi1
      in 
      concatenateSpecsWithSpec current afterHandling, env
    | CMatch (discr, None, _, cases) ->
      (* pattern matching *)
      (* this is quite similar to if-else. generate a disjunct for each branch with variables bound to the result of destructuring *)
      let dsp, env = infer_of_expression env current discr in
      let dsp, env = dsp |> concat_map_state env (fun sp env ->
        let ret = retrieve_return_value sp in
        cases |> concat_map_state env (fun (constr, vars, body) env -> 
          (* TODO this is hardcoded for lists for now *)
          match constr, vars with
          | "[]", [] ->
            let nil_case =
              let c = conj [Atomic (EQ, TApp ("is_nil", [ret]), TTrue)] in
              [NormalReturn (c, EmptyHeap, UNIT)]
            in 
            infer_of_expression env (concatenateSpecsWithEvent current nil_case) body
          | "::", [v1; v2] ->
            let cons_case =
              let c = conj [
                Atomic (EQ, TApp ("is_cons", [ret]), TTrue);
                Atomic (EQ, TApp ("head", [ret]), Var v1);
                Atomic (EQ, TApp ("tail", [ret]), Var v2);
              ] in
              [Exists [v1; v2]; NormalReturn (c, EmptyHeap, UNIT)]
            in
            infer_of_expression env (concatenateSpecsWithEvent current cons_case) body
          | _ -> failwith (Format.asprintf "unknown constructor: %s" constr)))
      in
      dsp, env
    | CMatch (_, Some _, _, _ :: _) ->
      (* TODO combine both kinds of matches *)
      failwith "unimplemented"
  in
  debug ~title:"forward rules" "{%s}\n%s\n{%s}" (string_of_disj_spec current) (string_of_core_lang expr) (string_of_disj_spec res);
  res, env

