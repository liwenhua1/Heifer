let id y = let x = y in x
  (*@  forall t. req y:#t';   ens res: # t'  $ req y:#str ; ens res:#str $ req y->#int; ens  y->#int /\res=y $ req y -> # List[(int \/ str)]; ens res = y /\ y -> # List[(int \/ str)] @*)
let f t = 
    !t := 5
let test q= 
  let x = ref 2 in 
  let y = ref x in 
  f y;
  x := 3; print_int (!(!y)) ; q

