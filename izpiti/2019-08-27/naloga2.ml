type 'a gnezdenje = 
    | Element of 'a
    | Podseznam of 'a gnezdenje list

(* a *)

let gnezdenje_primer =
    Podseznam [
        Element 1;
        Element 2;
        Podseznam[
            Element 3;
            Podseznam [Element 4];
            Podseznam []
        ];
        Podseznam [Element 5]
    ]

(* b *)

(* c *)

let rec preslikaj f = function
  | [] -> []
  | Element x :: gnezdenje -> (
      Element (f x) :: preslikaj f gnezdenje
  )
  | Podseznam sez :: gnezdenje -> (
      Podseznam (preslikaj f sez) :: preslikaj f gnezdenje
  )

(* d *)

let rec splosci = function 
    | [] -> []
    | Element x :: gnezdenje -> x :: splosci gnezdenje
    | Podseznam sez :: gnezdenje ->
        (splosci sez) @ splosci gnezdenje

(* e *)

let rec alternirajoci_konstruktorji = function
  | [] -> []
  | Element _ :: Podseznam _ :: _ 
  | Podseznam _ :: Element _ :: _ -> true
  | Element _ :: Element _ :: _ -> 
  | Podseznam _ :: Podseznam _ :: _ -> false


(* f *)