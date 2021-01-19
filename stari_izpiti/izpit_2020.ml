(*1.NALOGA*)

let option_sum x y = match x with
  | Some x -> match y with
    | Some y -> Some (x + y)
    | _ -> None
  | _-> None

let twostep_map f1 f2 f3 x = 
  let uporabi (y,z) = (f2 y, f3 z)
in uporabi (f1 x)

let y = twostep_map (fun x ->(x,x)) ((+)1) ((-)2) 3

let rec ponovi_n_krat n x = 
  if n<= 0 then []
  else x::ponovi_n_krat (n-1) x

let function_repeat f sez = 
  let rec shranjuj acc sez = match sez with
    | [] -> acc
    | x::xs when (f x)<=0 -> shranjuj acc xs
    | x::xs -> shranjuj (acc @ (ponovi_n_krat (f x) x)) xs
  in shranjuj [] sez 

let rec iterate f p x = 
  if (p (f x) ) then (f x)
  else iterate f p (f x)


(*--------------VAJA--------------------------------------------------------------------------*)
let my_list = [0; 2; 3; 4; 5; 6; 7; 8; 9; 10]

let bo = [false;false;false]

let a = List.mem 2 my_list (*true*)(*List.mem pove ce je element member/clan seznama*)

(* let a = List.iter (print_int) my_list *)

let c = List.for_all ((>)6) [1;3;4;2] (*to pomeni ali so vsi manjsi od 6-> 6>i*)

let d = List.exists ((>)6) [9;7;8]

let e = List.exists2 (<) [9;2;3] [4;5;6]

let f = List.for_all2 (<) [9;2;3] [4;5;6]

let g = List.map2 (+) [1;4;1] [3;2;8]

let h = List.filter ((<)6) [2;17;8;2;9;1] (*vrne vse elemente, ki so večji od 6*)

(* let rec pivotiraj p xs =
  List.partition (fun x -> x <= p) xs

ler w = pivotiraj 3 [2; 5; 9; 1; 7; 4; 3] *)
(*------------------------------------------------------------*)

(*2.NALOGA*)


type 'a improved_list = 
  | Prazen
  | Vozlisce of 'a list * 'a improved_list

let test = Vozlisce([1;2;20],Vozlisce([17;19;20;30],Vozlisce([100],Prazen)))

let rec count sez = match sez with
  |Prazen -> 0
  |Vozlisce(l,preostanek) -> List.length l + count preostanek

(*c manjka*)

let rec is_sorted sez = match sez with
  | Prazen -> true
  | Vozlisce(x,xs) -> (x = List.sort compare x) || is_sorted xs
(*ta funkcija vrne true če so urejene tabele na vozliscih, ali
mora vrniti tudi ujemanje urejenosti vseh tabel?*)

let naredi_tabelo sez = 
  let rec shrani acc sez = match sez with
    | Prazen -> acc
    | Vozlisce(x,xs)-> shrani (x @ acc) xs
in shrani [] sez

let rec is_sorted' sez = (naredi_tabelo sez = List.sort compare (naredi_tabelo sez))
(*ta funkcija pogleda da so tudi vsi seznami skupaj urejeni*)