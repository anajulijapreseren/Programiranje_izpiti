let odstej_trojici (x,y,z) (a,b,c) = (x-a,y-b,z-c)

let rec max_rezultat_do_n f n = match n with
  | 0-> f 0
  |_-> max (f 0) (max_rezultat_do_n f (n-1))

let rec pocisti_seznam list =
  let rec pocisti acc = function
    | None :: xs -> pocisti acc xs
    | Some x :: xs -> pocisti (x :: acc) xs
    | [] -> List.rev acc
  in
  pocisti [] list

let preveri_urejenost sez = 
  let rec razdeli lihi sodi sez = match sez with
    | []-> (sodi = List.sort compare sodi) && (lihi = List.rev (List.sort compare lihi))
    | x::xs when x mod 2 = 0-> razdeli lihi (x::sodi) xs
    | x::xs -> razdeli (x::lihi) sodi xs
  in razdeli [] [] sez

let primer1 = [5;2;4;1;6]
let primer2 = [3;2;4;5;6]

let rec preveri_urejenost' list =
  let rec narasca = function
    | [] | _ :: [] -> true
    | x1 :: x2 :: xs -> if x1 < x2 then narasca (x2 :: xs) else false
  in
  (list |> List.filter (fun x -> x mod 2 == 0) |> narasca)
  && (list |> List.filter (fun x -> x mod 2 == 1) |> List.rev |> narasca)


(*2.NALOGA*)

type 'a gnezdenje = 
  | Element of 'a
  | Podseznam of 'a gnezdenje list

(*[1;2;[3;[4];[]];[5]]*)

let gnezdenje_primer = 
  [Element 1;Element 2;
    Podseznam [Element 3; Podseznam [Element 4]; Podseznam []];
    Podseznam[Element 5]
  ]

let rec najvecja_globina = function
  | [] -> 1
  | Element _ :: xs -> najvecja_globina xs
  | Podseznam podsez :: xs ->
      max (najvecja_globina podsez + 1) (najvecja_globina xs)
