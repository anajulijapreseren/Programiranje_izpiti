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

let obdelovalec_vrta' = function Obdelovan x -> Some x | _ -> None





(*----------------------------------------------------------------------*)
let podvoji_vsoto x y = 2+(x+y)

let povsod_vecji (a,b,c) (x,y,z) = a>x && b> y && c>z

let uporabi_ce_lahko f x = match x with
  |Some x -> Some (f x)
  |None -> None

let pojavi_dvakrat x sez = 
  let rec aux acc sez = match sez with
    | [] -> acc
    | y::ys -> if y = x then aux (acc+1) ys else aux acc ys
  in (aux 0 sez) = 2

  let izracunaj_v_tocki x f_lst =
    let rec racunaj acc = function
      | [] -> List.rev acc
      | f :: fs -> racunaj (f x :: acc) fs
    in
    racunaj [] f_lst

let rec eksponent x p = 
  let rec aux  x p acc= 
    if p <= 0 then acc else aux  x (p-1) (acc * x)
  in 
  aux x p 1

  let rec eksponent' x pow = 
    let rec exp x pow acc =
      if pow <= 0 then acc else exp x (pow - 1) (x * acc)
    in
    exp x pow 1


(*2.NALOGA*)
type 'a mn_drevo = 
  |Prazno
  |Vozlisce of 'a mn_drevo * 'a * int * 'a mn_drevo

let rec vstavi x = function
  | Prazno -> Vozlisce(Prazno,x,1,Prazno)
  | Vozlisce(l,v,s,d) when x=v-> Vozlisce(l,v,s+1,d)
  | Vozlisce(l,v,s,d) when x<=v -> Vozlisce(vstavi x l,v,s,d)
  | Vozlisce(l,v,s,d) when x>v -> Vozlisce(l,v,s,vstavi x d)

let multimnozica_iz_seznama sez = 
  let rec trenutno drevo sez = match sez with
  |[]-> drevo
  |x::xs -> trenutno (vstavi x drevo) xs
  in trenutno Prazno sez

let c = multimnozica_iz_seznama [1;7;3;1;5;1]

let rec multimnozica_iz_seznama' = 
  List.fold_left (fun mmtree x -> vstavi x mmtree) Prazno

let c' = multimnozica_iz_seznama' [1;7;3;1;5;1]

let rec velikost_multimnozice = function
  |Prazno -> 0
  |Vozlisce(l,_,s,d)->s + velikost_multimnozice l + velikost_multimnozice d

let vstavi_n_krat n x sez= 
  let rec trenutno acc n = match n with
  | 0 -> acc
  | _ -> trenutno (x::acc) (n-1)
in trenutno sez n

let rec zdruzi l1 l2 =
  match l1 with
  | h :: t -> h :: zdruzi t l2
  | [] -> l2

let rec min_element sez = match sez with
  | [] -> 0 (*imamo le naravna st tako da bo ok*)
  | [x] -> x
  | x::xs -> min x (min_element xs)

let rec odstrani x sez = match sez with
  | [] -> []
  | y::ys when x=y-> ys
  | y::ys -> y::odstrani x ys
  

let rec uredi_seznam sez = match sez with
  | [] -> []
  | sez -> (min_element sez):: uredi_seznam (odstrani (min_element sez) sez)

let seznam_iz_multimnozice mnozica= 
  let rec trenutno acc mnozica = match mnozica with
    | Prazno -> uredi_seznam acc
    | Vozlisce(Prazno,v,c,d) -> trenutno (vstavi_n_krat c v acc) d
    | Vozlisce(l,v,c,Prazno) -> trenutno (vstavi_n_krat c v acc) l
    | Vozlisce(l,v,c,d) -> zdruzi (trenutno (vstavi_n_krat c v acc) l) (trenutno acc d)
  in trenutno [] mnozica
 
let x = seznam_iz_multimnozice c'

let seznam_iz_multimnozice' mmtree = 
  let rec ponovi x n = if n <= 0 then [] else x :: ponovi x (n-1) in
  let rec v_seznam = function
    | Prazno -> []
    | Vozlisce (lt, x, c, rt) -> (v_seznam lt) @ ponovi x c @ (v_seznam rt)
  in
  v_seznam mmtree
