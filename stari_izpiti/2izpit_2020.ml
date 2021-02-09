let dot_prod (x,y,z) (a,b,c) = (x*.a, y*.b, z*.c)

(* let fix_second f x =  *)

let safe_minus x y = if x > y then Some(x-y) else None


let comine_and_filter f xs ys = 
  let rec trenutno acc sez1 sez2 = match sez1 with
    |[]->List.rev  acc
    |x::xs->match sez2 with
      |[]->List.rev acc
      |y::ys-> match (f x y) with 
        |Some a-> (trenutno[@tailcall]) (a :: acc) xs ys
        |_->(trenutno[@tailcall]) acc xs ys
in trenutno [] xs ys

let a = comine_and_filter safe_minus [1;0;4;3] [2;1;0;2;5]




let conditional_print p sez = 
  let rec trenutno acc sez = match sez with
    |[]->if String.length acc > 0 then String.sub acc 0 ((String.length acc)-1)
    else acc
    |x::xs->
      if p x then trenutno (acc^x^",") xs
      else trenutno acc xs
    in print_endline(trenutno "" sez)

let long s= (String.length s > 3) 

let neki = ["Ta";"izpit";"je";"neumen!"]

(*2.NALOGA*)

type ('a,'b) tree = 
  |Empty
  |ANode of ('a,'b) tree * 'a * ('a,'b) tree
  |BNode of ('a,'b) tree * 'b * ('a,'b) tree

let test = ANode(BNode(Empty,true,Empty),12,ANode(ANode(Empty,0,Empty),5,BNode(Empty,false,Empty)))

let adepth drevo = 
  let rec shrani globina vsota  drevo = match drevo with
    | Empty -> (match vsota with
        |0->0
        |1->globina)
    | ANode(levo,_, desno) ->max (shrani (globina + 1) 1 levo) (shrani (globina + 1)1 desno)
    | BNode(levo,_, desno)-> max (shrani (globina + 1)0 levo) (shrani (globina + 1)0 desno)
in shrani 0 0 drevo

let bdepth drevo = 
  let rec shrani globina vsota  drevo = match drevo with
    | Empty -> (match vsota with
        |0->0
        |1->globina)
    | ANode(levo,_, desno) ->max (shrani (globina + 1) 0 levo) (shrani (globina + 1)0 desno)
    | BNode(levo,_, desno)-> max (shrani (globina + 1)1 levo) (shrani (globina + 1)1 desno)
in shrani 0 0 drevo

type result = {aNodes:int; bNodes:int}

let count drevo = {aNodes=adepth drevo; bNodes = bdepth drevo}

let is_typemirror drevo1 drevo2 = 