(* Definirajte funkcijo, ki vzame tri cela števila ter vrne njihov produkt. *)

let zmnozi x y z = x * y * z

(*Definirajte funkcijo, ki vzame celo število x in celo število k, ter
   vrne vrednost izraza x^3 + k.*)

let potenciraj_in_pristej x k =  zmnozi x x x + k

(*Definirajte funkcijo, ki vzame seznam in izračuna seznam vrednosti funkcije
   f(x) = x^3 + 2 za elemente vhodnega seznama.*)

let rec izracunaj_seznam sez = match  sez  with
  | [] -> []
  | x::xs -> (potenciraj_in_pristej x 2) :: izracunaj_seznam xs

let vse_kubiraj_in_pristej_dva = List.map (fun x -> potenciraj_in_pristej x 2)

(*Definirajte funkcijo, ki varno vrne zadnji element seznama v primeru,
da seznam ni prazen. Uporabite tip option.*)

let vrni_zadnji sez = match List.rev sez with
  | [] -> None
  | x::xs -> Some x

let rec zadnji_element = function |[] -> None | [x] -> Some x | _ :: xs -> zadnji_element xs


(*Definirajte funkcijo, ki izračuna n-to Fibonaccijevo število.
   Pri tem upoštevamo začetna pogoja /fibonacci 0 = 1/ in /fibonacci 1 = 1/*)

let rec fibonacci n = match n with
  | 0 -> 1
  | 1 -> 1
  | _ -> fibonacci (n -1) + fibonacci (n -2)

(* ======================================= *)
(* 2. naloga: podatkovni tipi in rekurzija *)
(* ======================================= *)

(* 2.1) Rožno drevo je drevo, v katerem ima vsak koren poljubno mnogo otrok,
   ki so zopet rožna drevesa. Rožna drevesa predstavimo s parametričnim
   tipom /'a drevo/ z enim konstruktorjem, ki sprejme:
   - vrednost (koren) tipa /'a/ in
   - seznam (gozd) dreves tipa /'a drevo/. *)

type 'a drevo = Rose of 'a * 'a drevo list

   (* 2.2) Definirajte naslednja rožna drevesa:
      t = 1,  t' =  2   ,      t'' =  3
                   / \               /| \
                  t  t              / |  \
                                  -1  t'  0
    *)
   
let t = Rose (1,[])

let t' = Rose(2,[t;t])

let t'' = Rose(3,[Rose(-1,[]);t';Rose(0,[])])

(* 2.3) Definirajte funkcijo, ki preveri ali je dano rožno drevo list drevesa,
   torej ima prazen gozd poddreves. *)
let je_list drevo = match drevo with
  |Rose(x,[])->true
  |_-> false

let je_list' (Rose (_, forest)) = (forest = [])

(* 2.4) Definirajte funkcijo, ki preveri, ali drevo celih števil vsebuje zgolj pozitivna števila. *)
 
let rec vsa_pozitivna (Rose (root, forest)) =
  let rec for_all f = function
    | [] -> true
    | x :: xs -> f x && for_all f xs 
  in
  root > 0 && for_all vsa_pozitivna forest

(* 2.5) Definirajte funkcijo, ki izračuna največjo širino rožnega drevesa, torej največjo dolžino
   gozda, ki se pojavi v kateremkoli vozlišču rožnega drevesa. *)
let rec sirina_drevesa (Rose (root, forest)) = 
  let rec for_all f = function
    | [] -> 0
    | x :: xs -> 1 + for_all f xs 
  in for_all sirina_drevesa forest

let rec sirina_drevesa' (Rose (_, forest)) =
  List.map sirina_drevesa forest |> List.fold_left max (List.length forest)

(* 2.6) Definirajte funkcijo, ki sestavi (poljubno) rožno drevo globine n.
   Vrednosti v korenih so poljubne. *)
let rec globoko_drevo n = match n with
  | 0 -> Rose(1,[])
  | _ -> Rose(1, [globoko_drevo (n-1)])

  let globoko_drevo' n =
    let rec aux acc n =
      if n > 0
      then aux (Rose (n, [acc])) (n-1)
      else acc
    in aux (Rose (n, [])) (n-1)
  
(* 2.7) Definirajte funkcijo, ki pretvori rožno drevo v seznam. Vrstni red vrednosti v seznamu
   pri tem ni pomemben.
   Primer: /drevo_v_seznam t'' = [3; -1; 2; 1; 1; 0]/ (ali katerakoli permutacija [3; -1; 2; 1; 1; 0])
   Če želite vse točke, mora biti funkcija repno rekurzivna.
   Opomba: kot ste videli na vajah, nekatere funkcije iz modula List,
   na primer List.map, niso repno rekurzivne, zato se jim raje
   izognite. *)

(*NASLEDNJI KOLOKVIJ*)

(* =================== *)
(* 1. naloga: funkcije *)
(* =================== *)

(* 1.1) Definirajte funkcijo, ki vzame par in zamenja komponenti para.
   Primer: /obrni (2, 4) = (4, 2)/ *)
   let obrni (x,y)= (y,x)

   (* 1.2) Definirajte funkcijo, ki vzame par p in vrednost x in zamenja drugo
      komponento para p z x.
      Primer: /zamenjaj_drugo (2, 4) 7 = (2, 7)/ *)
    let zamenjaj_drugo (x,_) z = (x,z)
   
   (* 1.3) Definirajte funkcijo, ki vzame seznam parov in izračuna nov seznam parov,
      ki imajo drugo komponento zamenjano z 42.
      Primer: /vsem_zamenjaj_drugo_z_42 [(12, 1); (2, 4)] = [(12, 42); (2, 42)]/ *)
    let rec vsem_zamenjaj_drugo_z_42 pari = match pari with
      | [] -> []
      | (x,y)::xs -> (x,42):: vsem_zamenjaj_drugo_z_42 xs
   
    let vsem_zamenjaj_drugo_z_42' l = List.map (fun p -> zamenjaj_drugo p 42) l


   (* 1.4) Definirajte funkcijo, ki varno vrne glavo seznama v primeru, ko seznam ni prazen.
      Uporabite tip option.
      Primer: /glava [1; 2; 3] = Some 1/ *)
    let glava sez= match sez with
      |[]-> None
      | x::xs -> Some x
      
   
   (* 1.5) Definirajte funkcijo, vzame funkcijo (f: 'a -> 'b), neko vrednost (x : 'a) in
      celo število n. Funkcija naj vrne vrednost, ki jo dobimo če f n-krat uporabimo na x,
      torej f (f ... (f x)...).
      Primer: /uporabi_veckrat succ 0 420 = 420/ *)
    let rec uporabi_veckrat f n x =
      if n <= 0 then x
      else uporabi_veckrat f (n-1) (f x)

(* ======================================= *)
(* 2. naloga: podatkovni tipi in rekurzija *)
(* ======================================= *)

(* 2.1) Rožno drevo je drevo, v katerem ima vsak koren poljubno mnogo otrok,
   ki so zopet rožna drevesa. Rožna drevesa predstavimo s parametričnim
   tipom /'a drevo/ z enim konstruktorjem, ki sprejme:
   - vrednost (koren) tipa /'a/ in
   - seznam (gozd) dreves tipa /'a drevo/. *)
   type 'a drevo = Rose of 'a * 'a drevo list

   (* 2.2) Definirajte naslednja rožna drevesa:
      t = 1,  t' =  2   ,      t'' =  3
                   / \               /| \
                  t  t              / |  \
                                  -1  t'  0
    *)
   
   let t = Rose(1,[])
   let t' = Rose(2,[t;t])
   let t'' = Rose(3,[Rose(-1,[]);t';Rose(0,[])])
   
   (* 2.3) Definirajte funkcijo, ki vrne gozd rožnega drevesa. *)
   let vrni_gozd (Rose(_,forest)) = forest
   
   (* 2.4) Definirajte funkcijo, ki izpiše vse vrednosti v rožnem drevesu celih števil.
      Števila naj bodo v ločenih vrsticah. Uporabite (print_int : int -> unit) in
      (print_newline : unit -> unit). *)



   
   (* 2.5) Definirajte funkcijo, ki izračuna globino rožnega drevesa, t.j. dolžino
      najdaljše poti od korena do lista. *)
   let globina = failwith "dopolni me"
   
   (* 2.6) Definirajte funkcijo, ki sestavi (poljubno) rožno drevo globine n.
      Vrednosti v korenih so poljubne. *)
   let globoko_drevo = failwith "dopolni me"
   
   (* 2.7) Definirajte funkcijo, ki sprejme funkcijo (f : 'b -> 'a -> 'b) in začetno vrednost (acc : 'b)
      in funkcijo f zloži [fold] preko drevesa (t : 'a drevo). Vrstni red pri tem ni pomemben.
      Za primer t' želimo vrednost f (f (f acc 1) 2) 2)  (lahko tudi f (f (f acc 2) 1) 2))
      Primer: /zlozi (fun acc x -> x + acc) 0 t'' = 6/
      Če želite vse točke, mora biti funkcija repno rekurzivna.
      Opomba: kot ste videli na vajah, nekatere funkcije iz modula List,
      na primer List.map, niso repno rekurzivne, zato se jim raje
      izognite. *)
   let zlozi = failwith "dopolni me"

