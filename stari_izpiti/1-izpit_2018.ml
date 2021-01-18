
(*2.NALOGA*)

type filtracijsko_drevo = 
  | Vozlisce of filtracijsko_drevo * int * filtracijsko_drevo
  | Skatla of (int list)

let primer = Vozlisce (Vozlisce(Skatla [1],5,Skatla[]),10,Vozlisce(Skatla[],15,Skatla[19;20]))



let rec vstavi st drevo = match drevo with
  | Skatla xs -> Skatla(List.sort compare (st::xs))
  | Vozlisce (l,v,d) -> 
    if  (st <= v) then Vozlisce(vstavi st l,v,d)
    else Vozlisce(l,v,vstavi st d)

let seznam = [1;24;2]

let rec vstavi_seznam sez drevo = match seznam with
    | []-> drevo
    | x::xs -> vstavi_seznam xs (vstavi x drevo)

(*d manjka*)
  
(*3.NALOGA*)





