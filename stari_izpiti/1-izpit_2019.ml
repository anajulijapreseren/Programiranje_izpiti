(*1.NALOGA*)

type complex = {re : float ; im : float}

let complex_add {re=a; im=b} {re=x;im=y} = 
  {re=(a+.x); im=(b+.y)}

let complex_conjugate {re=a; im=b} = 
  {re=a; im=(-.b)}

let list_apply_either pred f g xs = 
  let rec uporabi xs acc = match xs with
    | [] -> List.rev acc
    | y::ys -> 
      if pred y then uporabi ys ((f y)::acc)
      else uporabi ys ((g y)::acc)
    in uporabi xs []

(*funkcija pow: https://stackoverflow.com/questions/16950687/integer-exponentiation-in-ocaml*)
let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n -> 
    let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)
    
let eval_poly koef tocka = 
  let rec shrani koef acc  = match koef with
    |[]-> acc
    | x::xs -> shrani xs (acc +(x * (pow tocka (List.length xs))))
    in shrani (List.rev koef) 0

(*2.NALOGA*)

type najemnik = string

type vrt = Obdelovan of najemnik
    | Oddan of najemnik * (vrt * vrt list)
    | Prost

let vrt_primer = Oddan ("K" ,(Obdelovan "Galois" ,[Obdelovan "Lagrange"; Prost]))
let vrtic = Obdelovan "Katja"

let obdelovalec_vrta vrt = match vrt with
  |Obdelovan ime -> Some ime
  |_->None

let rec globina_oddajanja vrt = match vrt with
  |Prost -> 0
  (* |Oddan (_,(x,xs)) -> 1 + globina_oddajanja x + globina_oddajanja xs
  |Obdelovan _ -> 0  *)

let a = globina_oddajanja vrt_primer


