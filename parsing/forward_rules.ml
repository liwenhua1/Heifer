
open Hipcore
open Hiptypes
open Pretty
open Normalize
include Subst
open Debug


let concatenateSpecsWithEvent (current:disj_spec) (event:spec) : disj_spec = 
  List.map (fun a -> List.append a event) current

let concatenateEventWithSpecs  (event:spec) (current:disj_spec) : disj_spec = 
  List.map (fun a -> List.append event a ) current


let concatenateSpecsWithSpec (current:disj_spec) (event:disj_spec) :  disj_spec = 
  List.concat_map (fun a -> concatenateSpecsWithEvent current a) event

let rec retrieve_return_value (spec:spec) : term = 
  match spec with 
  | [] -> failwith "retrieve_return_value empty spec"
  | [NormalReturn (pi, _)] -> get_res_value pi
  | [HigherOrder (_, _, _, retN)]
  | [RaisingEff(_, _, _, retN)] -> retN
  | _ :: xs -> retrieve_return_value xs 

let rec replace_return_value (t:term) (spec:spec) : spec = 
  match spec with 
  | [] -> failwith "replace_return_value empty spec"
  | [HigherOrder (p, h, i, _)] -> [HigherOrder (p, h, i, t)]
  | [NormalReturn (p, h)] -> [NormalReturn (p, h)]
  | [RaisingEff(p, h, i, _)] -> [RaisingEff (p, h, i, t)]
  | s :: ss -> s :: replace_return_value t ss

(** add an equality to v to the end of all disjuncts *)
let constrain_final_res (sp:disj_spec) (v:term) : disj_spec =
  sp |> List.map (fun s ->
    let l = List.length s in
    s |> List.mapi (fun i a -> if i < l-1 then a else
      match a with
      | Exists _ -> a
      | Require (_, _) -> a
      | NormalReturn (p, h) ->
        NormalReturn (And (p, res_eq v), h)
      | HigherOrder (p, k, (c, va), r) ->
        HigherOrder (And (p, Atomic (EQ, r, v)), k, (c, va), r)
      | RaisingEff (p, k, (c, va), r) ->
        RaisingEff (And (p, Atomic (EQ, r, v)), k, (c, va), r)
      | TryCatch _ -> failwith "unimplemented"))

(** Environment used for forward verification *)
type fvenv = {
  (* defined methods, may be added to if lambdas are given names *)
  fv_methods : meth_def SMap.t;

  fv_predicates : pred_def SMap.t;
  (* proof obligations generated by defining lambdas with specifications.
     we propagate them out instead of checking them immediately to avoid a cyclic dependency with entailment. *)
  fv_lambda_obl : (disj_spec * disj_spec) list;

  (* proof obligations generated by user-supplied match specs *)
  fv_match_obl : (disj_spec * disj_spec) list;

  fv_match_summary : (string * string list * disj_spec) list;
}

let create_fv_env fv_methods fv_predicates = {
  fv_methods;
  fv_predicates;
  fv_lambda_obl = [];
  fv_match_obl = [];
  fv_match_summary = [];
}

let retrieveSpecFromEnv (fname: string) (env:fvenv) : (string list * spec list) option = 
  (* Format.printf "ENV %s@." (string_of_smap string_of_meth_def env.fv_methods); *)
  match 
    SMap.find_opt fname env.fv_methods
    |> Option.map (fun m -> 
      (match m.m_spec with 
      | None -> ()
      | Some _sp ->
        (* print_endline ("retrieveSpecFromEnv: " ^ string_of_disj_spec _sp); *)
        ()
      );
      (m.m_params, Option.get m.m_spec))
  with 
  | Some res -> 
    (* let (_, specs) = res in 
    print_endline ("retrieveSpecFromEnv1: " ^ string_of_disj_spec specs); *)
    Some res
  | None -> 

  SMap.find_opt fname env.fv_predicates
  |> Option.map (fun p -> (p.p_params, p.p_body))

let rec specContainUndefinedHO (spec:spec) (env:fvenv) : bool = 
  match spec with 
  | [] -> false 
  | HigherOrder (_p, _k, (c, _va), _r):: xs -> 
    (match retrieveSpecFromEnv c env with 
    | None -> true 
    | _ -> specContainUndefinedHO xs env  
    )
  | TryCatch _  :: _ -> true 
  | _ :: xs -> specContainUndefinedHO xs env   


let retrieveMatchSummaryFromEnv (fname: string) (env:fvenv) : (string list * spec list) option = 
  let records = env.fv_match_summary in 

  let rec helper li = 
    match li with 
    | [] -> None  
    | (label, (args: string list), summary) :: xs -> 
      if String.compare fname label == 0 then Some (args, summary) 
      else helper xs  
  in helper records




let instantiateExistientalVarSpec   (spec:spec) 
(bindings:((string * string) list)): spec = 
  let normalSpec = normalize_spec spec in 
  normalisedStagedSpec2Spec (instantiateExistientalVar normalSpec bindings)



let isFreshVar str : bool = 
  if String.length str < 1 then false 
  else 
    let a = String.get str 0 in 
    (*let b = String.get str 1 in *)
    if a='f' (*&& b ='f'*) then true else false 

let () = assert (isFreshVar "f10" ==true )
let () = assert (isFreshVar "s10" ==false )

let renamingexistientalVarState (existientalVar:string list) ((pi, kappa):state): (string list * state) = 
  let newNames = List.map (fun n -> (verifier_getAfreeVar n)) existientalVar in 
  let newNameTerms = List.map (fun a -> Var a) newNames in 
  let bindings = bindFormalNActual existientalVar newNameTerms in 
  (newNames, (instantiatePure bindings pi, instantiateHeap bindings kappa))


(** substitutes existentials with fresh variables *)
let renamingexistientalVar (specs:disj_spec): disj_spec = 
  (* Format.printf "specs: %s@." (string_of_disj_spec specs); *)
  List.map (
    fun spec -> 
      (* Format.printf "spec: %s@." (string_of_spec spec); *)
      let normalSpec = normalize_spec spec in 
        (* Format.printf "normalSpec: %s@." (string_of_normalisedStagedSpec normalSpec); *)
      let existientalVar = getExistentialVar normalSpec in 
      (* Format.printf "existientalVar: %s@." ( string_of_list Fun.id existientalVar); *)
      let newNames = List.map (fun n -> (verifier_getAfreeVar n)) existientalVar in 
      let newNameTerms = List.map (fun a -> Var a) newNames in 
      let bindings = bindNewNames existientalVar newNames in 
      let temp = instantiateExistientalVar normalSpec bindings in 
        (* Format.printf "temp: %s@." (string_of_normalisedStagedSpec temp); *)
      let bindings = bindFormalNActual existientalVar newNameTerms in 
      let r = 
      instantiateSpec bindings (normalisedStagedSpec2Spec temp)
      in
        (* Format.printf "r: %s@." (string_of_spec r); *)
      r
  ) specs

let renameSpecListAndInstantiate specList bindings = 
  let specList' = renamingexistientalVar specList in 
  instantiateSpecList bindings specList'

let renameSpecAndInstantiate spec bindings = 
  let specList' = renamingexistientalVar [spec] in 
  match specList' with 
  | hd::_ -> instantiateSpec bindings hd 
  | _ -> failwith ("error renameSpecAndInstantiate")

(** substitutes existentials with fresh variables, and the resulting formula has no quantifiers *)
let freshen (specs:disj_spec): disj_spec = 
  renamingexistientalVar specs
  |> List.map (List.filter (function Exists _ -> false | _ -> true))

(** Find the case that handles the effect [label] *)
let lookforHandlingCases ops (label:string) = 
  List.find_map (fun (s, arg, spec) ->
    if String.equal s label then Some (arg, spec) else None) ops

(* let (continueationCxt: ((spec list * string * (string * core_lang) * core_handler_ops) list) ref)  = ref []  *)

let map_state env f xs =
  let r, env =
    List.fold_right (fun c (t, env) ->
      let r, e1 = f c env
      in (r :: t, e1)
    ) xs ([], env)
  in
  r, env

(** Like concat_map, but threads an extra "environment" argument through which can be updated by the function *)
let concat_map_state env f xs =
  let r, env = map_state env f xs in
  List.concat r, env

let%expect_test _ =
  let r, e = (concat_map_state 0 (fun x e -> [x; x * 3], e + 1) [1; 2; 3]) in
  Format.printf "%s %d@." (string_of_list string_of_int r) e;
  [%expect
    {| [1; 3; 2; 6; 3; 9] 3 |}]

let foldl1 f xs =
  match xs with
  | [] -> failwith "foldl1"
  | x :: xs1 ->
    List.fold_left f x xs1

let call_primitive env history fname actualArgs =
  match fname, actualArgs with
  | "+", [x1; x2] ->
    let event = NormalReturn (res_eq (Plus(x1, x2)), EmptyHeap) in
    concatenateSpecsWithEvent history [event], env
  | "-", [x1; x2] ->
    let event = NormalReturn (res_eq (Minus(x1, x2)), EmptyHeap) in
    concatenateSpecsWithEvent history [event], env
  | "=", [x1; x2] ->
    (* let event = NormalReturn (Atomic (EQ, x1, x2), EmptyHeap, Eq (x1, x2)) in *)
    let event = NormalReturn (res_eq (Rel (EQ, x1, x2)), EmptyHeap) in
    concatenateSpecsWithEvent history [event], env
  | "not", [x1] ->
    let event = NormalReturn (res_eq (TNot (x1)), EmptyHeap) in
    concatenateSpecsWithEvent history [event], env
  | "&&", [x1; x2] ->
    let event = NormalReturn (res_eq (TAnd (x1, x2)), EmptyHeap) in
    concatenateSpecsWithEvent history [event], env
  | "||", [x1; x2] ->
    let event = NormalReturn (res_eq (TOr (x1, x2)), EmptyHeap) in
    concatenateSpecsWithEvent history [event], env
  | ">", [x1; x2] ->
    let event = NormalReturn (res_eq (Rel (GT, x1, x2)), EmptyHeap) in
    concatenateSpecsWithEvent history [event], env
  | "<", [x1; x2] ->
    let event = NormalReturn (res_eq (Rel (LT, x1, x2)), EmptyHeap) in
    concatenateSpecsWithEvent history [event], env
  | ">=", [x1; x2] ->
    let event = NormalReturn (res_eq (Rel (GTEQ, x1, x2)), EmptyHeap) in
    concatenateSpecsWithEvent history [event], env
  | "<=", [x1; x2] ->
    let event = NormalReturn (res_eq (Rel (LTEQ, x1, x2)), EmptyHeap) in
    concatenateSpecsWithEvent history [event], env
  | "::", [x1; x2] ->
    let event = NormalReturn (res_eq (TCons (x1, x2)), EmptyHeap) in
    concatenateSpecsWithEvent history [event], env
  | _ -> failwith (Format.asprintf "unknown primitive: %s, args: %s" fname (string_of_list string_of_term actualArgs))

(* Given the specs of the scrutinee, symbolically execute it against the handler's spec to produce a single flow, e.g.

    match A(a,r); ens res=c with
    | A d -> continue(f,g); ens res=g
    | v -> v

    first, handle A. we need [d:=a] to bind the argument in the handler branch, and [r:=f] to pass back the argument from continue.

    this gives us a flow like the following, due to deep handling:

    ens d=a /\ r=f;
      match continue(f,g) with
      | ...
    ; ens res=g

    the spec of the continue is (ens res=c), the rest of the scrutinee after A. replace the continue stage with ens res=c.

    ens d=a /\ r=f;
      match ens res=c with
      | v -> ens res=v
      | ...
    ; ens res=g 

    now recursively handle the continue. the base case occurs when there are no more effects to handle. substitute [v:=c] in the value branch.
    c will then be returned from the (substituted-away) continue stage, so [res:=g], resulting in this trace.

    ens d=a /\ r=f; ens g=c; ens res=g

    the actual code below handles extra complexity such as the scrutinee having disjunction, multiple continue stages, res not being an equality (in which case "bridging" fresh variables are needed), freshening existentials

*)

let replaceContinueWithHypo (afterHandling:disj_spec) (match_summary:disj_spec option):disj_spec =  
  match match_summary with 
  | None -> afterHandling 
  | Some ([[HigherOrder (_, _, (f, _::formal), _)]]) -> 
    (*print_endline ("replaceContinueWithHypo: " ^ string_of_staged_spec (HigherOrder (p, h, (f, hd::formal), r))); 
    *)
    List.map (
      fun spec -> 

        let rec helper (history:spec) (specIn:spec) : spec = 
          match specIn with 
          | [] ->  history 
          | HigherOrder (p', h', (f', hd'::actual), r') :: xs  -> 
            let x = HigherOrder (p', h', (f', hd'::actual), r') in 
            if String.compare f' "continue" != 0 then helper (history@[x]) xs 
            else 
              let newStage =HigherOrder (p', h', (f, hd'::formal), r') in 
              history@[newStage]@xs
              
          | x :: xs -> helper( history@[x]) xs 

        in 
        helper [] spec

    ) afterHandling 



  | _ -> failwith ("replaceContinueWithHypo not yet")


(* This function better to be used after a normalisation *)
let retriveLastRes (a:spec) : term option = 
  let rec retriveLastResFromPi (pi:pi) : term option = 
    match pi with 
    | Atomic(EQ, Var "res", t) -> Some t 
    | And (pi1, pi2) -> 
      (match retriveLastResFromPi pi1 with 
      | Some t -> Some t 
      | None -> retriveLastResFromPi pi2 
      )
    | _ -> None 
  in 
  (*print_endline ("current " ^ string_of_spec a); *)
  let src = List.rev a in 
  match src with 
  | NormalReturn (pi, _) :: _ 
  | HigherOrder (pi, _, _, _) :: _
  | RaisingEff(pi, _, _, _) :: _ -> 
    (match retriveLastResFromPi pi with 
    | None -> failwith ("retriveLastResFromPi  no res value prescribed")
    | Some t -> Some t 
    )
  | [] -> failwith "empty"
  | TryCatch _ :: _ -> failwith "tc"
  | Require _ :: _ ->
    None
  | Exists _ :: _ -> failwith ("retriveLastRes ending with ex")



let rec flattenList lili = 
  match lili with 
  | [] -> []
  | x :: xs -> List.append x (flattenList xs) 

  

let cartesian_product li1 li2 = 
  flattenList (List.map  (fun l1 -> 
    List.map  (fun l2 -> (l1, l2)) li2) li1)



let instantiateSpecListUponResume (handlingSpec: spec list) (contiInput:string) (continuation: spec list) : spec list = 
  (*print_endline ("contiInput = " ^ contiInput  );  *)
  let rec helper (handlingSpecIn:spec) (continuationIn:spec list) : spec list =
    match handlingSpecIn with 
    | [] -> [[]] 
    | HigherOrder ((p', h', (f', hd'::actual::rest), r')) :: xs  -> 
      let x = (p', h', (f', hd'::actual::rest), r') in  (* hd' is k, and we assume there is only one argument for effects *)
      if String.compare f' "continue" == 0 
      then 
        let instantiations =  normalise_spec_list ( (renameSpecListAndInstantiate continuationIn [(contiInput, actual)])) in 
          (*(normalise_spec_list (instantiateSpecList [(contiInput, actual)] )) in *)
        (*print_endline ("instantiation_continuationIn: " ^ string_of_spec_list instantiations);
        *)

        List.map (fun instantiation -> 
          let contiRet = retrieve_return_value instantiation in 

          let instantiation = instantiateSpec ["res", contiRet] instantiation in 

          let newPi=  And (p', Atomic(EQ, contiRet, r' ) ) in
          (*print_endline (string_of_pi newPi);  *)

          let (prefix:spec) = NormalReturn (newPi, h') :: instantiation in 

          List.flatten(List.map (fun (rest:spec) -> prefix @ rest) (helper xs continuationIn))

        
        )
        instantiations

        
      else 
        List.map (fun rest -> (HigherOrder x):: rest) (helper xs continuationIn)

    | x :: xs -> 
      List.map (fun rest -> x:: rest) (helper xs continuationIn)
  
  in 

  List.flatten (List.map (fun (h_spec) -> 
    let temp = 
    helper h_spec continuation in 
    (*print_endline ("!!!!!!!!" ^ string_of_spec h_spec  ^ " \nand conti = " ^ string_of_spec_list continuation);
    print_endline ("= " ^ string_of_spec_list temp);
    *)
    temp
    
    ) handlingSpec) 


let findTheActualArg4AccTerm arg (term:term): term option =
  match term with
  | Plus (t1, t2) 
  | TAnd (t1, t2)  -> 
    if stricTcompareTerm t1 arg then Some t2 
    else if stricTcompareTerm t2 arg then Some t1
    else None  
  | _ -> None 



let rec findTheActualArg4AccPure arg (pi:pi): term option =
  match pi with 
  | Atomic (_, t1, t2) -> 
    (match findTheActualArg4AccTerm arg t1 with 
    | Some t -> Some t 
    | None -> findTheActualArg4AccTerm arg t2
    )
  | And   (p1, p2) 
  | Or    (p1, p2) 
  | Imply (p1, p2) -> 
    (match findTheActualArg4AccPure arg p1 with 
    | Some t -> Some t 
    | None -> findTheActualArg4AccPure arg p2
    )
  | Not    p -> findTheActualArg4AccPure arg p
  | _ -> None





let findTheActualArg4Acc_x_e_ret (arg:term) (specs:disj_spec): term =
  match normalise_spec_list specs with
  | spec_n :: _ -> 
    let (allPure:pi) = getherPureFromSpec spec_n in 
    (match findTheActualArg4AccPure arg allPure with
    | None  -> failwith ("can not find findTheActualArg4Acc_x_e_ret ")
    | Some t -> t 
    )
  
  | _ -> failwith ("findTheTermAssocatiedWith_x_e_ret empty spec")




(** this is the entrance of the try-catch reduction **)
let rec handling_spec typ env (match_summary:tryCatchLemma option) (scr_spec:normalisedStagedSpec) (h_norm:(string * disj_spec)) (h_ops:(string * string option * disj_spec) list) : spec list * fvenv = 
  
  let@ _ = Debug.span (fun r ->
    debug ~at:3 ~title:"handling_spec" "match\n  (*@@ %s @@*)\nwith\n| ...\n| ...\n==>\n%s" (string_of_spec (normalisedStagedSpec2Spec scr_spec)) (string_of_result string_of_disj_spec (Option.map fst r))
    ) in
  let (scr_eff_stages, scr_normal) = scr_spec in 
  match scr_eff_stages with 
  | [] ->
    (* the scrutinee's effects have been completely handled, so go into the value case *)
    let (h_val_param, h_val_spec) = h_norm in 

    let current =
      let (ex, (p1, h1), (p2, h2)) = scr_normal in

      (*
      Format.printf "\\phi_n: %s@." (string_of_disj_spec h_val_spec); 
      Format.printf "N(r): %s@." (string_of_normalisedStagedSpec ([], scr_normal)); 
      Format.printf "formal: %s@."  h_val_param;
*)
      let actualRet = get_res_value p2 in 
      (*
      Format.printf "actualRet: %s@."  (string_of_term actualRet);
*)
      let p2 = instantiatePure ["res", actualRet] p2 in
      let scr_normal = (normalStage2Spec (ex, (p1, h1), (p2, h2))) in 
      (*
      Format.printf "scr_normal: %s@." (string_of_spec scr_normal); 
*)
      let h_spec = instantiateSpecList [h_val_param, actualRet] h_val_spec in
      (*
      Format.printf "h_spec: %s@." (string_of_disj_spec h_spec); 
*)
      concatenateSpecsWithSpec (normalise_spec_list [(scr_normal)]) h_spec

    in
    print_endline ("\nhandling_spec " ^ (string_of_spec (normalisedStagedSpec2Spec scr_spec))); 

    print_endline ("Final results [] : " ^ string_of_spec_list current);
    
    current, env

  | (TryCatchStage _) :: _ -> [(normalisedStagedSpec2Spec scr_spec)], env

  | (EffHOStage x) :: xs -> 
    let (label, effActualArg) = x.e_constr in

    let handledContinuation, env = 
      match typ with 
      | Shallow -> 
        if String.compare label "Flip" == 0 then 
          let normalCase, env = handling_spec Shallow env match_summary ([], scr_normal) h_norm h_ops in 
          let prefix = effectStage2Spec xs in 
          List.map (fun a -> prefix @ a ) normalCase , env
        else 
          [normalisedStagedSpec2Spec (xs, scr_normal)] , env
      | Deep -> handling_spec Deep env match_summary (xs, scr_normal) h_norm h_ops 
    in 
    let handledContinuation = normalise_spec_list handledContinuation in 
    (*
    print_endline ("handledContinuation: " ^ string_of_spec_list handledContinuation);
*)


    (* there is an effect stage x, which may or may not be handled *)
    let perform_ret = retriveFormalArg x.e_ret in 
    let performEx = x.e_evars in 
    let performPre = x.e_pre in 
    let performPost = x.e_post in 

    let norm = (performEx, performPre, performPost) in 
    

    if startingFromALowerCase label then 
      ((* lower case stages are for function calls *)
      (*print_endline ("lower case " ^ label)  ; *)


      match match_summary with 
      | Some (tcl_head, Some tcl_handledCont, (*(handlerSpec),*)  tcl_summary) -> 

(*
        print_endline ("======================================\n");

        print_endline (string_of_try_catch_lemma (tcl_head, Some tcl_handledCont, (*handlerSpec,*) tcl_summary) ^ "\n");
        print_endline (string_of_effHOTryCatchStages (EffHOStage x) ^ " # " ^ string_of_spec_list handledContinuation);
        print_endline ("");
        print_endline (string_of_handlingcases (h_norm,h_ops)); 


        print_endline ("======================================\n");
*)
      


      
      
        let effFormalArg = 
          match normalize_spec tcl_head with 
          | ([(EffHOStage y) ], _) -> 
            let (label_lemma, effActualArg_lemma) = y.e_constr in
            if String.compare label_lemma label == 0 then 
              List.map (fun a -> retriveFormalArg a ) (effActualArg_lemma@[y.e_ret])
            else failwith (Format.asprintf "lemma does not start with a label %s" label);
          | _ -> failwith (Format.asprintf "lemma does not start with a HO stage for %s" label);

        in 

        
        Format.printf "effActualArg: %s@." (string_of_list string_of_term (effActualArg@[x.e_ret])); 
        Format.printf "effFormalArg: %s@." (string_of_list Fun.id effFormalArg); 
      
        let bindings = bindFormalNActual (effFormalArg) (effActualArg@[x.e_ret]) in  


        let contiRet = findTheActualArg4Acc_x_e_ret x.e_ret handledContinuation in 

        
        (* SYH: here hard coded the instantiation for m and acc for the purpose of the toss example. *)
        let mi = match typ with | Deep -> ("m", Num 2) | Shallow -> ("m", Num 1) in 
        let instantiate_tcl_summary = renameSpecListAndInstantiate tcl_summary (mi::("acc", contiRet)::bindings) in
        let instantiate_tcl_summary = normalise_spec_list instantiate_tcl_summary in 
        

        
        print_endline ("\nhandling_spec " ^ (string_of_spec (normalisedStagedSpec2Spec scr_spec))); 
        print_endline ("Final results lower case: \n" ^  string_of_spec_list instantiate_tcl_summary ^"\n"); 


        instantiate_tcl_summary, env


      | Some _  -> 
        failwith (Format.asprintf "lemma without continuation %s" label);

      | None -> [(normalisedStagedSpec2Spec scr_spec)], env
          (*if String.compare label "continue" == 0 then 
          else failwith (Format.asprintf "no lemma provided for %s" label);
          *)

      )
      
    else 
    (* upper case stages are for effects *)
    (
    match lookforHandlingCases h_ops label with 
    | None -> concatenateEventWithSpecs (effectStage2Spec [EffHOStage x]) handledContinuation, env
    | Some (effFormalArg, handler_body_spec) ->
      let effFormalArg = match effFormalArg with | None -> [] | Some v -> [v] in
      Format.printf "effActualArg: %s@." (string_of_list string_of_term effActualArg); 
      Format.printf "effFormalArg: %s@." (string_of_list Fun.id effFormalArg); 
      
      let bindings = bindFormalNActual (effFormalArg) (effActualArg) in 
      print_endline ("binding length " ^ string_of_int (List.length bindings));
      (* effect x is handled by a branch of the form (| (Eff effFormalArg) k -> spec) *)
      (* debug ~at:5 ~title:"before freshen" "%s" (string_of_disj_spec handler_body_spec); *)
      (* freshen, as each instance of the handler body should not interfere with previous ones *)

      let handler_body_spec = renamingexistientalVar handler_body_spec in
      let handler_body_spec = instantiateSpecList bindings handler_body_spec in 
      (* debug ~at:5 ~title:(Format.asprintf "handler_body_spec for effect stage %s" (fst x.e_constr)) "%s" (string_of_disj_spec handler_body_spec); *)
      print_endline ("Effect: " ^label ^ " and handler_body_spec: "  ^ string_of_disj_spec handler_body_spec); 
      (* the rest of the trace is now the spec of the continuation *)
      print_endline ("continuation_spec: " ^ string_of_spec_list handledContinuation);
      

      let handler_body_specAfterSubstituteK = normalise_spec_list(instantiateSpecListUponResume handler_body_spec perform_ret handledContinuation) in 
      
      
      print_endline ("handlerbodyAfterSubstituteK: " ^ string_of_spec_list handler_body_specAfterSubstituteK);
      
      let res = concatenateEventWithSpecs  (normalStage2Spec norm) ( handler_body_specAfterSubstituteK) in 

      print_endline ("\nhandling_spec " ^ (string_of_spec (normalisedStagedSpec2Spec scr_spec))); 
      print_endline ("Final results upper case: \n" ^  string_of_spec_list res ^"\n"); 
      
      res, env
    )




let ifAsyncYiled env  = 
  match retrieveSpecFromEnv "dequeue" env with
  | None  -> false 
  | Some _ -> true  

let recursivelyInstantiateFunctionCalls env instantiatedSpec = 

  let rec helper acc li : spec list = 
    match li with 
    | [] -> [acc] 
    | x :: xs  -> 
      (match x with 
      | Require _ | Exists _  | NormalReturn _ | RaisingEff _ | TryCatch _ -> helper (acc@[x]) xs 
      | HigherOrder (pi, kappa, (fname, actualArgs), _ret)  -> 
        if String.compare fname "helper" == 0 then  (* check if it is recursive *)
        (match retrieveSpecFromEnv fname env with 
        | None -> helper (acc@[x]) xs 
        | Some (formalArgs, spec_of_fname) -> 
          let bindings = bindFormalNActual ((*"res"::*)formalArgs) ((*ret::*)actualArgs) in 
          let instantiatedSpec = instantiateSpecList bindings spec_of_fname in 
          List.flatten (List.map (fun spec -> 
            helper (acc@[NormalReturn (pi, kappa)]@spec) xs 
          ) instantiatedSpec)
        )
        else helper (acc@[x]) xs 
      )
    
    

  in 
  List.flatten (List.map (fun spec -> helper [] spec) instantiatedSpec)



 
(** may update the environment because of higher order functions *)
(** This is the entrence of the forward reasoning **)
let rec infer_of_expression (env:fvenv) (history:disj_spec) (expr:core_lang): disj_spec * fvenv =
  if SSet.mem "res" (used_vars_disj_spec history) then
    failwith (Format.asprintf "invariant violated: { %s } %s { ... } is not res-free" (string_of_disj_spec history) (string_of_core_lang expr));
  (* TODO infer_of_expression is likely O(n^2) due to appending at the end *)
  let res, env =
    match expr with
    | CValue v -> 
      let event = NormalReturn (res_eq v, EmptyHeap) in 
      concatenateSpecsWithEvent history [event], env

    | CLet (str, expr1, expr2) ->
      let phi1, env = infer_of_expression env history expr1 in 

      let phi2, env = infer_of_expression env [freshNormalReturnSpec] expr2 in

      (* preserve invariant that left side is res-free *)
      phi1 |> concat_map_state env (fun spec env -> 
          (* the return value is context-sensitive and depends on what in the history came before *)
          let ret =
            match unsnoc spec with
            | _, RaisingEff (_pre, _post, _constr, Var ret) -> ret
            | _, HigherOrder (_pre, _post, _constr, Var ret) -> ret
            | _, RaisingEff (_, _, _, ret) | _, HigherOrder (_, _, _, ret) -> failwith (Format.asprintf "ret not a variable: %s" (string_of_term ret))
            | _ -> "res"
          in

          (* create an existential by creating a fresh variable, and preserve the invariant by removing res from the post of the first expr, as it will now appear on the left of the second premise *)
          let nv = verifier_getAfreeVar "let" in
          let spec = instantiateSpec [ret, Var nv] spec in
          (* let spec = spec @ [NormalReturn (Atomic (EQ, Var nv, Var str), EmptyHeap)] in *)
          let spec = (Exists [nv]) :: spec in

          (* let var = verifier_getAfreeVar "let" in *)
          let phi2 = instantiateSpecList [str, Var nv] phi2 in

          concatenateSpecsWithSpec [spec] phi2, env
        )
    | CRef v -> 
      let freshVar = verifier_getAfreeVar "ref" in 
      let event = NormalReturn (res_eq (Var freshVar), PointsTo(freshVar, v)) in 
      concatenateSpecsWithEvent history [Exists [freshVar];event], env


    | CRead str -> 
      let freshVar = verifier_getAfreeVar str in 
      let event = [Exists [freshVar];Require(True, PointsTo(str, Var freshVar)); 
        NormalReturn (res_eq (Var freshVar), PointsTo(str, Var freshVar))] in 
      concatenateSpecsWithEvent history event, env


    | CAssert (p, h) -> 
      let temp = concatenateSpecsWithEvent history [Require(p, h)] in 
      concatenateSpecsWithEvent temp [(NormalReturn(And (res_eq UNIT, p), h))], env

    | CPerform (label, arg) -> 
          
      let arg = 
        match arg with 
        | Some v -> [v]
        | _ -> []
      in 
      let freshVar = verifier_getAfreeVar "per" in 
      (* after adding the perfome stage, we need to add a normal return. *)
      concatenateSpecsWithEvent history 
      [Exists [freshVar];RaisingEff(True, EmptyHeap, (label,arg), Var freshVar);
      NormalReturn (res_eq (Var freshVar), EmptyHeap)], env


    | CResume tList ->  
      let f = verifier_getAfreeVar "re" in
      let res =
        concatenateSpecsWithEvent history [Exists [f]; HigherOrder (True, EmptyHeap, ("continue", tList), Var f)]
      in
      res, env
    | CFunCall (fname, actualArgs) when List.mem fname primitive_functions -> 
      call_primitive env history fname actualArgs
    | CFunCall (fname, actualArgs) -> 
      let fn_spec : disj_spec =
        match retrieveSpecFromEnv fname env with 
        | None ->
          (* no known spec, produce a stage *)
          let ret = verifier_getAfreeVar "ret" in
          [[Exists [ret]; HigherOrder (True, EmptyHeap, (fname, actualArgs), Var ret)]]
        | Some (spec_params, known_spec) ->


          let@ _ =
            Debug.span (fun r ->
                debug ~at:3
                  ~title:(Format.asprintf "function %s has known spec" fname)
                  "forall %s\n%s\n==>\n%s" (String.concat " " spec_params) (string_of_disj_spec known_spec)
                  (string_of_result string_of_disj_spec r))
          in

          let trf s f x =
            let r = f x in
            debug ~at:3
              ~title:s
              "%s\n==>\n%s" (string_of_disj_spec x)
              (string_of_disj_spec r);
            r
          in

          (* if any args are HO and have specs, substitute them as well *)
          let arg_specs =
            List.filter_map (fun arg ->
              match arg with
              | Var a ->
                (match retrieveSpecFromEnv a env with
                | None -> None
                | Some (params, sp) ->
                  let res = verifier_getAfreeVar "res" in
                  let params = params @ [res] in
                  Some (a, TLambda (verifier_getAfreeVar "lambda", params, sp |> renamingexistientalVar |> instantiateSpecList ["res", Var res], None)))
              | _ -> None) actualArgs
          in

          let spec = known_spec |> trf "existentials" renamingexistientalVar in

          
          let instantiatedSpec =
            spec |> trf "actuals" (instantiateSpecList (bindFormalNActual spec_params actualArgs))
              (* substituting too zealously (e.g. expanding every term argument into a lambda) can cause proofs to fail, due to either bugs or incomplete handling of lambda. to work around this, do only the minimal substitution needed here, as subsumption formulae aren't helped by unfolding, whereas functions are *)
              |> trf "ho args" ((subst_visitor_subsumptions_only arg_specs)#visit_disj_spec ())
          in 

          let instantiatedSpec = instantiatedSpec |> trf "function stages" (recursivelyInstantiateFunctionCalls env) in 

          instantiatedSpec
      in
      let _fn_spec =
        (* this is an alternative implementation for this whole case, which simply generates an uninterpreted function and lets the entailment procedure take care of unfolding (since the implementation above can be seen as unfolding once). unfortunately the handler reasoning in the effects work relies on unfolding in the forward reasoning, so we can't switch to it yet, but this implementation should work for higher-order *)
        let ret = verifier_getAfreeVar "ret" in
        [[Exists [ret]; HigherOrder (True, EmptyHeap, (fname, actualArgs), Var ret); NormalReturn (res_eq (Var ret), EmptyHeap)]]
      in
      concatenateSpecsWithSpec history fn_spec, env
    | CWrite  (str, v) -> 
      let freshVar = verifier_getAfreeVar "wr" in 
      let event = [Exists [freshVar];Require(True, PointsTo(str, Var freshVar)); 
                    NormalReturn (res_eq UNIT, PointsTo(str, v))] in 
      concatenateSpecsWithEvent history event, env


    | CIfELse (v, expr2, expr3) -> 
      let eventThen = NormalReturn (v (*EQ, v, TTrue*), EmptyHeap) in 
      let eventElse = NormalReturn (Not v (*EQ, v, TTrue*), EmptyHeap) in 
      let currentThen = concatenateSpecsWithEvent history [eventThen] in 
      let currentElse = concatenateSpecsWithEvent history [eventElse] in 
      let r1, env = infer_of_expression env currentThen expr2 in
      let r2, env = infer_of_expression env currentElse expr3 in
      r1 @ r2, env


    | CLambda (params, given_spec, body) ->
      let inferred, env = infer_of_expression env [[]] body in
      let inferred = normalise_spec_list inferred in
      let lid = verifier_getAfreeVar "lambda" in

      debug ~at:2 ~title:(Format.asprintf "lambda %s spec" lid) "body: %s\n\ninferred: %s\ngiven: %s" (string_of_core_lang body) (string_of_option string_of_disj_spec given_spec) (string_of_disj_spec inferred);

      let spec_to_use =
        match given_spec with
        | None -> inferred
        | Some g -> g
      in

      let ret = verifier_getAfreeVar "res" in
      let spec_to_use = instantiateSpecList ["res", Var ret] spec_to_use in

      let env =
        match given_spec with
        | None -> env
        | Some g -> { env with fv_lambda_obl = (inferred, g) :: env.fv_lambda_obl }
      in
      let event = NormalReturn (res_eq (TLambda (lid, params @ [ret], spec_to_use, Some body)), EmptyHeap) in 
      concatenateSpecsWithEvent history [event], env        


    | CMatch (typ, match_summary, scr, Some val_case, eff_cases, []) -> (* effects *)
      (* infer specs for branches of the form (Constr param -> spec), which also updates the env with obligations *)

      
      (*print_endline (string_of_handler_type typ); *)
      

      let inferred_branch_specs, env =
        List.fold_right (fun (effname, param, spec, body) (t, env) ->
          let r, env = infer_of_expression env [[]] body in
          let env, sp =
            match spec with
            | None -> env, r
            | Some s -> { env with fv_match_obl = (r, s) :: env.fv_match_obl }, s
          in
          (*let sp = normalize_spec sp in *)
          let sp = (normalise_spec_list sp) in 

          (*
          print_endline (string_of_effect_cases_specs [(effname,param, sp)]); 
    *)
    

          
          (effname, param, sp) :: t, env
        ) eff_cases ([], env)
      in

      let inferred_val_case, env =
        let (param, body)  = val_case in
        let inf_val_spec, env = infer_of_expression env [[]] body in
        let inf_val_spec = normalise_spec_list inf_val_spec in 

        (*
        print_endline (string_of_normal_case_specs (param, inf_val_spec)); 
*)


        (param, inf_val_spec), env
      in
      (* for each disjunct of the scrutinee's behaviour, reason using the handler *)
      let phi1, env = infer_of_expression env [freshNormalReturnSpec] scr in 
      (*
      let phi1 = 
        match phi1 with 
        | _::hd::_-> [hd]
      in    
      *)
      
      
      

      (*
      print_endline ("\nSpec of the try block: " ^ string_of_disj_spec phi1 ^ "\n\n"); 
*)
      let afterHandling, env =
        concat_map_state env (fun spec env -> 
          let spec_n = (normalize_spec spec) in 
          let temp = handling_spec typ env match_summary spec_n inferred_val_case inferred_branch_specs in 
          (*print_endline ("-------------------"); *)
          temp
        ) phi1
      in 
      (*print_endline ("\nAfter afterHandling at handler: \n" ^ string_of_disj_spec afterHandling ^ "\n\n");  
      *)
      concatenateSpecsWithSpec history afterHandling, env 
      

    | CMatch (_, _, discr, None, _, cases) -> (* pattern matching *)

      (* this is quite similar to if-else. generate a disjunct for each branch with variables bound to the result of destructuring *)
      let dsp, env = infer_of_expression env history discr in
      let dsp, env = dsp |> concat_map_state env (fun sp env ->
        let ret = retrieve_return_value sp in
        cases |> concat_map_state env (fun (constr, vars, body) env -> 
          (* TODO this is hardcoded for lists for now *)
          match constr, vars with
          | "[]", [] ->
            let nil_case =
              (* let c = conj [Atomic (EQ, TApp ("is_nil", [ret]), TTrue)] in *)
              (* [NormalReturn (c, EmptyHeap)] *)
              (* [] *)
              [NormalReturn (Atomic (EQ, ret, Nil), EmptyHeap)]
            in 
            infer_of_expression env (concatenateSpecsWithEvent history nil_case) body
          | "::", [v1; v2] ->
            let cons_case =
              let c = conj [
                (* Atomic (EQ, TApp ("is_cons", [ret]), TTrue);
                Atomic (EQ, TApp ("head", [ret]), Var v1);
                Atomic (EQ, TApp ("tail", [ret]), Var v2); *)
                Atomic (EQ, ret, TCons (Var v1, Var v2))
                (* IsDatatype (ret, "list", "cons", [Var v1; Var v2]) *)
              ]
              (* [] *)
            in
              [Exists [v1; v2]; NormalReturn (c, EmptyHeap)]
              (* [Exists [v1; v2]; NormalReturn (, EmptyHeap)] *)
              (* [] *)
            in
            infer_of_expression env (concatenateSpecsWithEvent history cons_case) body
          | _ -> failwith (Format.asprintf "unknown constructor: %s" constr)))
      in
      dsp, env
    | CMatch (_, _, _, Some _, _, _ :: _) -> 
      (* TODO *)
      failwith "combining effect handlers and pattern matching not yet implemented"
  in
  debug ~at:2 ~title:"forward rules" "{%s}\n%s\n{%s}" (string_of_disj_spec history) (string_of_core_lang expr) (string_of_disj_spec res);
  res, env
