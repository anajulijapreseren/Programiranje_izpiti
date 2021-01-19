(*1.NALOGA*)

let uporabi f x = f x

let ibaropu x f = f x

let zacetnih n xs = 
  if List.length xs < n then None
  else 
    let rec trenutno acc n sez = match n with
      | 0 -> acc
      | _ -> match sez with
        | [] -> acc (*ta šansa ne more bit ker gre pod prvi if*)
        | y::ys -> trenutno (y::acc) (n-1) ys
    in Some (trenutno [] n xs)

(*2.NALOGA*)

type 'a neprazen_sez = 
  | Konec of 'a
  | Sestavljen of 'a * 'a neprazen_sez

let prvi sez = match sez with
  | Konec x-> x
  | Sestavljen (x,_) -> x

let rec zadnji sez = match sez with
  | Konec x-> x
  | Sestavljen (_,ys) -> zadnji ys

let rec dolzina sez = match sez with
  | Konec _-> 1
  | Sestavljen (_,ys) -> 1 + dolzina ys

let pretvori_v_seznam sez = 
  let rec trenutno acc sez = match sez with
    | Konec x -> acc @ [x]
    | Sestavljen(x,xs) -> trenutno (x :: acc) xs
  in trenutno [] sez 

let rec zlozi f x sez = match sez with
  | Konec a -> f x a
  | Sestavljen(y,ys) -> zlozi f (f x y) ys