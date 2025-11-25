open Hipcore_typed
open Typedhip

val split_one : kappa -> ((string * term) * kappa) option

val split_find : string -> kappa -> (term * kappa) option

val xpure : kappa -> pi

val find_var_in_heap : string -> kappa -> term list

val find_var_in_pure : string -> pi -> term list

val check_alising : name list -> name list -> pi -> name list

val find_in_state : string -> pi * kappa -> string * term

val swap_var_name_in_state : name -> name -> pi * kappa -> pi * kappa

val swap_content_in_state : ty_var -> term -> pi * kappa -> pi * kappa

val unify_var_name_in_state: name -> term -> pi * kappa -> pi * kappa

(** [check id vars h1 h2 k] solves the heap entailment {m H_1 \vdash H_2 * F}.
  It may backtrack given existentially quantified locations on the right.

  [id] is a human-readable name to say what this this entailment is for in logs, e.g. a precondition.

  [vars] is a list of existentially quantified location variables in [h2].

  [h1] and [h2] are heap formulae, which are tuples of a spatial part [kappa] and a pure part [pi].

  [check] "returns" via the continuation [k], which is invoked with:

  - residual pure assumptions from [h1]
  - remaining pure obligations from [h2]
  - the inferred frame {m F}

  [k] may continue to backtrack (and fail).
*)
(* val check :
  string ->
  string list ->
  state ->
  state ->
  (pi * pi * kappa -> 'a Search.t) -> 'a Search.t *)
