(* =================== *)
(* 1. naloga: funkcije *)
(* =================== *)

let is_root x y = x * x = y

let pack3 x y z= (x,y,z)

let sum_if_not p sez = 
  let rec trenutno acc sez = match sez with
    |[]-> acc
    |x::xs when p x -> trenutno acc xs
    |x::xs -> trenutno (acc + x) xs
  in trenutno 0 sez 

let apply sez_funkcij sez_elementov = 
  let rec trenutno acc sez_funkcij sez_elementov = match sez_elementov with
    | [] -> acc
    | x::xs ->
      let rec uporabi acc2 sez_funkcij = match sez_funkcij with
        |[]-> acc2::acc
        |f::fs -> uporabi ((f x) :: acc2) fs
    in uporabi [] sez_funkcij
  in trenutno [] sez_funkcij sez_elementov

(*# apply [(+) 1; (-) 2; ( * ) 3] [1; 2; 3];;
	- : int list list = [[2; 1; 3]; [3; 0; 6]; [4; -1; 9]]
	# apply [(<) 1; (=) 2; (>) 3] [1; 2; 3; -1];;
	- : bool list list =
	[[false; false; true]; 
	 [true;  true;  true];
	 [true;  false; false]; 
	 [false; false; true]]*)


(* ======================================= *)
(* 2. naloga: podatkovni tipi in rekurzija *)
(* ======================================= *)


type vrsta_srecanja = 
  | Predavanja
  | Vaje

type srecanje = {predmet:string; vrsta:string; trajanje:string}

type urnik = List of srecanje list

let vaje = "dopolni me"
let predavanja = "dopolni me"

let urnik_profesor = "dopolni me"

let je_preobremenjen = "dopolni me"

let bogastvo = "dopolni me"