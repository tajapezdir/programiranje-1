(* a *)

let odstej_trojici (a1, a2, a3) (b1, b2, b3) = 
    (a1 - b1, a2 - b2, a3 - b3)

(* b *)

let rec max_rezultat_do_n f = function
    | 0 -> f 0
    | n -> max (f n) (max_rezultat_do_n f (n - 1))

(* c *)

let rec pocisti_seznam = function
    | [] -> []
    | None :: xs -> pocisti_seznam xs
    | x :: xs -> x :: pocisti_seznam xs

(* d *)

let preveri_urejenost list = 
    let rec je_urejen = function
    | [] -> true
    | x :: [] -> true
    | x1 :: x2 :: xs -> x1 <= x2 && je_urejen (x2 :: xs)
    in
    let lihi = List.filter (fun x -> x mod 2 = 1) list in
    let sodi = List.filter (fun x -> x mod 2 = 0) list in
    je_urejen sodi && je_urejen (List.rev lihi)