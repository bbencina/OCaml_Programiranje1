(* =================== *)
(* 1. naloga: funkcije *)
(* =================== *)

(* 1.1) Definirajte funkcijo, ki vzame dve celi števili ter vrne njuno vsoto.
   Primer: /sestej 2 3 = 5/ *)
let sestej = (+)

(* 1.2) Definirajte funkcijo, ki svojemu argumentu prišteje 3.
   Primer: /pristej_tri 10 = 13/ *)
let pristej_tri = (+) 3

(* 1.3) Definirajte funkcijo, ki vsem elementom seznama prišteje 5.
   Primer: /vsem_pristej_pet [1; 2] = [6; 7]/ *)
let rec vsem_pristej_pet l =
  match l with
  | [] -> []
  | hd::tl -> (hd + 5) :: (vsem_pristej_pet tl)

(* 1.4) Definirajte funkcijo, ki vrne zadnjo komponento nabora s tremi elementi.
   Primer: /tretji (1, "horse", [None]) = [None]/ *)
let tretji nabor =
  match nabor with
  | (y, z, x) -> x
  | _ -> failwith "Nabor mora imeti natanko 3 elemente!"

(* 1.5) Definirajte funkcijo, ki vzame dve funkciji ter vrne njun kompozitum.
   Primer: /kompozitum succ string_of_int 5 = "6"/ *)
let kompozitum f g = (fun x -> g (f x))


(* ======================================= *)
(* 2. naloga: podatkovni tipi in rekurzija *)
(* ======================================= *)

(* 2.1) Rožno drevo je drevo, v katerem ima vsak koren poljubno mnogo otrok,
   ki so zopet rožna drevesa. Rožna drevesa predstavimo s parametričnim
   tipom /'a drevo/ z enim konstruktorjem, ki sprejme:
   - vrednost (koren) tipa /'a/ in
   - seznam (gozd) dreves tipa /'a drevo/. *)
type 'a drevo =
  | Node of 'a * 'a drevo list

(* 2.2) Napišite funkcijo, ki vrne koren danega rožnega drevesa. *)
let koren = function
  | Node(root, forest) -> root

(* 2.3) Napišite funkcijo, ki preveri, ali drevo celih števil vsebuje kakšno negativno število. *)
let rec kaksno_negativno tree =
  match tree with
  | Node(x, []) -> if x < 0
    then true
    else false
  | Node(x, forest) -> if x < 0
    then true
    else
    (
      let rec mapping sez =
        match sez with
        | [] -> false
        | hd::tl -> (kaksno_negativno hd) || (mapping tl)
      in mapping forest
      )

(* 2.4) Sestavite funkcijo, ki sprejme naravno število ter sestavi (poljubno)
   drevo, ki ima toliko otrok.
   Namig: napišite pomožno funkcijo, ki ustvari poljuben seznam dane dolžine. *)
let rec clown_car n =
  match n with
  | 0 -> []
  | x -> Node(1, []) :: (clown_car (x-1))

let drevo_z_veliko_otroci n = Node(1, clown_car n)
(* 2.5) Sestavite funkcijo, ki izračuna število vseh vozlišč v drevesu.
   Če želite vse točke, mora biti funkcija repno rekurzivna.

   Opomba: kot ste videli na vajah, nekatere funkcije iz modula List,
   na primer List.map, niso repno rekurzivne, zato se jim raje
   izognite. *)

let velikost = ()
