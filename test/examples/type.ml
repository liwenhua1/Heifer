type h = |A 
         |B 



let id2 y x = x = !y+1  
 (*@ req y->#Ref[int] /\ x:#str; ens res : # Ref[y] @*)

 let id y = let x = y in x;;
  (*@  forall t. req y:#t';   ens res: # t'  $ req y:#str ; ens res:#str $ req y->#int; ens  y->#int /\res=y $ req y -> # List[(int \/ str)]; ens res = y /\ y -> # List[(int \/ str)] @*)

(* let string_of_int x = failwith "to be implemented" *)
 (*@ req true; ens res : # Ref[y] @*)
(* let inc_inplace x = x := string_of_int !x +1 *)
 (*@ req true; ens res : # Ref[y] @*)
 
 (* let f t = 
    !t := 5
let test q= 
  let x = ref 2 in 
  let y = ref x in 
  f y;
  x := 3; print_int (!(!y)) ; q *)

