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

let apply sez_funkcij' sez_elementov = 
  let rec trenutno acc sez_funkcij sez_elementov = match sez_elementov with
    | [] -> acc
    | x::xs ->
      let rec uporabi acc2 sez_funkcij = match sez_funkcij with
        |[]-> trenutno (acc @ [List.rev acc2]) sez_funkcij' xs
        |f::fs -> uporabi ((f x) :: acc2) fs
    in uporabi [] sez_funkcij'
  in trenutno [] sez_funkcij' sez_elementov

(*# apply [(+) 1; (-) 2; ( * ) 3] [1; 2; 3];;
	- : int list list = [[2; 1; 3]; [3; 0; 6]; [4; -1; 9]]
	# apply [(<) 1; (=) 2; (>) 3] [1; 2; 3; -1];;
	- : bool list list =
	[[false; false; true]; 
	 [true;  true;  true];
	 [true;  false; false]; 
	 [false; false; true]]*)

(*let apply sez_funkcij sez_elementov = 
  let rec trenutno acc sez_funkcij sez_elementov = match sez_elementov with
    | [] -> acc
    | x::xs ->
      let rec uporabi acc2 sez_funkcij = match sez_funkcij with
        |[]-> acc2::acc
        |f::fs -> uporabi ((f x) :: acc2) fs
    in uporabi [] sez_funkcij
  in trenutno [] sez_funkcij sez_elementov*)



(* ======================================= *)
(* 2. naloga: podatkovni tipi in rekurzija *)
(* ======================================= *)


type vrsta_srecanja = 
  | Predavanja
  | Vaje

type srecanje = {predmet:string; vrsta:vrsta_srecanja; trajanje:int}

type urnik = srecanje list list

let vaje = {predmet="Analiza 2a"; vrsta=Vaje; trajanje=3}
let predavanja = {predmet="Programiranje 1"; vrsta=Predavanja; trajanje=2}

let urnik_profesor = 
[[{predmet="A"; vrsta=Vaje; trajanje=2}];
[];
[{predmet="B"; vrsta=Predavanja; trajanje=1}];
[];
[];
[{predmet="C"; vrsta=Vaje; trajanje=1}];
[]]

let je_preobremenjen' dan=
  let rec stevec vaje predavanja dan= match dan with
    |[] -> vaje > 4 || predavanja > 4
    |{predmet=_; vrsta=Vaje; trajanje=t}::xs -> stevec (vaje+t) predavanja xs
    |{predmet=_; vrsta=Predavanja; trajanje=t}::xs -> stevec vaje (predavanja+t) xs
  in stevec 0 0 dan

let je_preobremenjen urnik = List.fold_left (||) false (List.map je_preobremenjen' urnik)

let bogastvo' dan = 
  let rec stevec vaje predavanja dan= match dan with
      |[] -> vaje + predavanja
      |{predmet=_; vrsta=Vaje; trajanje=t}::xs -> stevec (vaje+t) predavanja xs
      |{predmet=_; vrsta=Predavanja; trajanje=t}::xs -> stevec vaje (predavanja+2*t) xs
    in stevec 0 0 dan

let bogastvo urnik = List.fold_left (+) 0 (List.map bogastvo' urnik)


let p = (bogastvo) urnik_profesor

