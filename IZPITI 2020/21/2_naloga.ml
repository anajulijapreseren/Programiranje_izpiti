(*============================================================================*]
  Filip potrebuje pomoč pri organiziranju kuhinje. Posode in omare mu je uspelo
  popisati in ustrezno označiti, sedaj pa mora nad tem izvajati kopico
  arhivskih nalog, kjer nastopite vi.
[*============================================================================*)

type 'a kuhinjski_element =
  | Ponev of 'a
  | Lonec of 'a * 'a
  | Omara of 'a list

(* a *)
(*----------------------------------------------------------------------------*]
  Definirajte primer seznama kuhinjskih elementov [kuhinja], kjer ponev vsebuje
  niz "tuna", lonec vsebuje "brokoli" in "mango", omara pa vsebuje "sir",
  "toast", "sok" in "ragu".
[*----------------------------------------------------------------------------*)

let kuhinja = [Ponev "tuna"; Lonec("brokoli","mango"); Omara(["sir";
"toast"; "sok";"ragu"])]


(* b *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo [prestej], ki za podani seznam kuhinjskih elementov vrne
  skupno število vsebinskih elementov. Za zgornji primer [kuhinja] bi tako
  vrnila 7.
[*----------------------------------------------------------------------------*)

let rec prestej kuhinja = match kuhinja with
  |[]->0
  |Ponev _::xs -> 1 + prestej xs
  |Lonec(_,_)::xs -> 2 + prestej xs
  |Omara (sez)::xs -> List.length sez + prestej xs


(* c *)
(*----------------------------------------------------------------------------*]
  Definirajte funkcijo, ki sprejme funkcijo [f] in kuhinjski element ter
  funkcijo [f] uporabi na celotni vsebini elementa.

    pretvori : (’a -> ’b) -> ’a kuhinjski_element -> ‘b kuhinjski_element

[*----------------------------------------------------------------------------*)

let pretvori f kuhinjski_el = match kuhinjski_el with
  | Ponev x -> Ponev (f x)
  | Lonec (x,y) -> Lonec(f x, f y)
  | Omara(sez) -> Omara(List.map f sez)



(* d *)
(*----------------------------------------------------------------------------*]
  Definirajte funkcijo [pospravi], ki sprejme seznam kuhinjskih elementov in
  vsebino vseh elementov pospravi v eno samo [Omaro]. Vrstni red elementov v
  končni omari je nepomemben. Za vse točke naj bo funkcija repno rekurzivna. 

    pospravi : ’a kuhinjski_element list -> ‘a kuhinjski_element

[*----------------------------------------------------------------------------*)

let pospravi sez = 
  let rec shranjuj acc sez = match sez with
    | [] -> Omara(acc)
    | (Ponev x)::xs -> shranjuj (x::acc) xs
    | (Lonec (x,y))::xs -> shranjuj (x::y::acc) xs
    | (Omara(sez))::xs -> shranjuj (acc @ sez) xs
  in shranjuj [] sez
(* e *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo [oceni], ki sprejme seznam tipa ['a kuhinjski_element list]
  in cenilko vsebine tipa [‘a -> int]. Funkcija izračuna skupno ceno celotnega
  seznama, kjer je cena vsebine v loncih množena s 3, v omarah pa s 5.

  Ocena testne kuhinje za cenilko [String.length] je 115.
[*----------------------------------------------------------------------------*)

let oceni sez cenilka = 
  let rec shranjuj acc sez = match sez with
    | [] -> acc
    | (Ponev x)::xs -> shranjuj (acc + (cenilka x)) xs
    | (Lonec (x,y))::xs -> shranjuj (acc + 3*(cenilka x) + 3*(cenilka y)) xs
    | (Omara(sez))::xs -> shranjuj (acc + 5*(List.fold_left (+) 0 (List.map cenilka sez))) xs
 in shranjuj 0 sez