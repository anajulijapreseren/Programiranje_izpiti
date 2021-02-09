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

let rec preslikaj f sez = match sez with
  | [] -> []
  | Element x :: xs -> (Element (f x)) :: (preslikaj f xs)
  | Podseznam p :: xs -> Podseznam (preslikaj f p) :: (preslikaj f xs)

let rec splosci =function
  |[]->[]
  |Element x ::xs -> x::splosci xs
  |Podseznam x::xs -> (splosci x) @ splosci(xs) 

let alternirajoci_konstruktorji sez = 
  let rec zapomni_si e_p sez = match sez with(*0 za e in 1 za p*)
    |[]->true
    |Element x ::xs -> 
      if e_p = 0 then false
      else zapomni_si 0 xs
    |Podseznam x :: xs ->
      if e_p = 1 then false
      else zapomni_si 1 xs
    in zapomni_si 2 sez

    let rec zlozi_preko_gnezdenja f acc gnezdenje =
      (* Napišemo lastno repno rekurzivno funkcijo za združevanje. *)
      let zdruzi xs ys =
        let rec prelozi ys = function
          | [] -> ys
          | x :: xs -> prelozi (x :: ys) xs
        in
        prelozi ys (List.rev xs)
      in
      let rec zlozi f acc = function
        | [] -> acc
        | Element x :: xs -> zlozi f (f acc x) xs
        | Podseznam podsez :: xs -> zlozi f acc (zdruzi podsez xs)
      in
      zlozi f acc gnezdenje