(* Lit le fichier "dict.dat" et construit une liste de chaînes appelée
words_list contenant l'ensemble des mots considérés valides *)
let words_list =
  let f = open_in "dict.dat" in
    let rec aux lst n =
      if n=0 then List.rev lst else aux ((input_line f)::lst) (n-1)
    in aux [] (int_of_string (input_line f))

(*On determine la fréquence d'apparition de chaque lettre*)

let frq = Array.make 27 0

(*cette fonction incrémente la case du tableau correspondant
 à la lettre donnée et du total*)
let car_frq car = frq.(26) <- frq.(26)+1; match car with
	|'a' -> frq.(0) <- frq.(0) + 1
	|'b' -> frq.(1) <- frq.(1) + 1
	|'c' -> frq.(2) <- frq.(2) + 1
	|'d' -> frq.(3) <- frq.(3) + 1
	|'e' -> frq.(4) <- frq.(4) + 1
	|'f' -> frq.(5) <- frq.(5) + 1
	|'g' -> frq.(6) <- frq.(6) + 1
  |'h' -> frq.(7) <- frq.(7) + 1
	|'i' -> frq.(8) <- frq.(8) + 1
	|'j' -> frq.(9) <- frq.(9) + 1
	|'k' -> frq.(10) <- frq.(10) + 1
	|'l' -> frq.(11) <- frq.(11) + 1
	|'m' -> frq.(12) <- frq.(12) + 1
	|'n' -> frq.(13) <- frq.(13) + 1
	|'o' -> frq.(14) <- frq.(14) + 1
	|'p' -> frq.(15) <- frq.(15) + 1
	|'q' -> frq.(16) <- frq.(16) + 1
	|'r' -> frq.(17) <- frq.(17) + 1
	|'s' -> frq.(18) <- frq.(18) + 1
	|'t' -> frq.(19) <- frq.(19) + 1
	|'u' -> frq.(20) <- frq.(20) + 1
	|'v' -> frq.(21) <- frq.(21) + 1
	|'w' -> frq.(22) <- frq.(22) + 1
	|'x' -> frq.(23) <- frq.(23) + 1
	|'y' -> frq.(24) <- frq.(24) + 1
	|'z' -> frq.(25) <- frq.(25) + 1
  | _ -> ()

(*cette fonction remplit le tableau des occurences pour un mot*)
let rec word_frq = function
	| "" -> ()
	|w -> car_frq w.[0]; word_frq (String.sub w 1 (String.length w -1))

(*cette fonction remplit le tableau des occurences avec
 la liste de mots passée en paramètre*)
let rec det_frq = function
	|[] -> ()
	|h::q -> word_frq h; det_frq q;;

det_frq words_list;;
(*print_int frq.(15)*)

(*cette fonction détermine la case du tableau
 dans laquelle la valeur est la plus faible*)
let rec min_arr arr = 
  let i = ref 0 in
  for j = 1 to 25 do
    if arr.(j) < arr.(!i) then i:=j
  done;
  !i;;

(*cette fonction assaocie à un nombre la lettre lui correspondant*)
let cara = function
  |1 -> 'a'
  |2 -> 'b'
  |3 -> 'c'
  |4 -> 'd'
  |5 -> 'e'
  |6 -> 'f'
  |7 -> 'g'
  |8 -> 'h'
  |9 -> 'i'
  |10 -> 'j'
  |11 -> 'k'
  |12 -> 'l'
  |13 -> 'm'
  |14 -> 'n'
  |15 -> 'o'
  |16 -> 'p'
  |17 -> 'q'
  |18 -> 'r'
  |19 -> 's'
  |20 -> 't'
  |21 -> 'u'
  |22 -> 'v'
  |23 -> 'w'
  |24 -> 'x'
  |25 -> 'y'
  |26 -> 'z'
  |_ -> ' '

(*cette fonction transforme le tableau contenant l'occurence des lettres 
en un tableau dans lequel elle sont par ordre d'occurence croissante*)
let sort frq = 
  let car = Array.make 26 ' ' in
  for i = 0 to 25 do
    let min_frq = min_arr frq in
    car.(i) <- cara (min_frq+1);
    frq.(min_frq) <- max_int;
  done;
  car;;

let ord_car = sort frq

(*le type tree sera celui qui permettrade contenir nos données*)
type tree = N of (char  * tree list) | L of string
exception Found of (int * int)

let tab_word = Array.make 15 (N (' ',[]))

(*Cette fonction permet de transformer un mot en une liste contenant
 les lettres dans l'ordre d'occurence définit plus tôt*)
let rec ord_word last = function
  | "" -> []
  |w -> try 
        for j = 0 to String.length w -1 do
            if w.[j] = '?'
              then raise (Found(-1, j))
        done;
        for i = last to 25 do
          for j = 0 to String.length w -1 do
            if w.[j] = ord_car.(i)
              then raise (Found(i, j))
            done
          done;
        []
   with
     | Found (-1, j) -> '?'::
              (ord_word 0 (String.sub w 0 j ^ if j+1 <String.length w
              then (String.sub w (j+1) (String.length w -j-1))
              else ""))
      | Found (i, j)-> ord_car.(i) ::
            (ord_word i (String.sub w 0 j ^ if j<String.length w
              then (String.sub w (j+1) (String.length w -j-1))
              else ""));;

(*Array.iter (print_char) ord_car;;*)

(*cette fonction prend un mot sous la forme d'une
 chaine de caractère et le place dans l'arbre*)
let place_word word =
  let rec aux = function (*cette fonction permet de descendre dans le noeud qui correspond à la première lettre du mot et de le faire récursivement*)
    | [], lst -> L word :: lst
    |h :: q, lst -> if List.exists (function  |N(x, _) -> x = h
                                              |L _ -> failwith "erreur1"
                                            ) lst (*autrement dit, si il existe un noeud correspondant à la lettre qui nous interesse*)
                              then let rec aux_aux = function (*cette fonction permet de trouver le noeud correspondant à la lettre qui nous ineteresse*)
                                    |N(car, lst1) ::q1
                                     when car = h 
                                    ->N(h, aux (q, lst1)) :: q1
                                    |N(car, lst1) :: q1 -> N(car,lst1) :: aux_aux q1
                                    |L _::_ -> failwith "erreur2"
                                    |[]-> failwith "erreur2"
                                  in aux_aux lst
                              else N(h, aux (q, [])) :: lst (*si on ne trouve pas de noeud qui correspond à notre lettre, on en crée un*)
    in let len = String.length word -1
    in tab_word.(len) <-
      N(' ', aux ((ord_word 0 word), (function |N(_, lst) -> lst
                                               |L _ -> failwith "erreur3"
                      ) tab_word.(len)));;

(*cette fonction place tous les mots du dictionnaire dans la structure*)
let rec construit_arbre = function
  |[] -> ()
  |h::q -> place_word h; construit_arbre q;;

construit_arbre words_list;;

(*cette fonction associe à une lettre le numéro qui lui correspond*)
let numb car =
  let i = ref 0 in
  while car <> ord_car.(!i) do
    i := !i +1
  done;
  !i;;

(*print_int (numb 'z')*)

(* Prend une requête (chaine de caractères) et affiche les mots possibles *)
let find req =
  let word = ref (ord_word 0 req) in
  let inter = ref 0 in
  let result = ref []in
  let rec count_inter = function
    | '?'::lst -> inter := !inter +1; count_inter lst
    |lst -> lst
  in word := count_inter !word;
  let rec aux word inter = function (*cette fonction place les mots qu'on cherche dans la liste "result"*)
    | [] -> ()
    |t::q -> begin match t with
              | L w -> result := w:: !result; aux word inter q
              | N (car, lst) -> begin  match word with
                                  | [] -> if inter = 0 
                                          then failwith "erreur 4"
                                        else (aux word (inter-1) lst;
                                              aux word inter q)
                                  | tw::qw -> if car = tw
                                              then aux qw inter lst1        (*cette partie correspond aux différentes*)
                                            else                            (*possibilité et permet de gérer correctement*)
                                              aux word inter q;             (*les points d'interrogations*)
                                            if inter>0
                                            then begin 
                                              if car = tw 
                                              then aux word inter q;
                                              if numb car < numb tw 
                                              then aux word (inter-1) lst;
                                            end
                                end
            end
  in aux !word !inter ((function |N(_, lst) -> lst
                                 |L _ -> failwith "erreur3"
                      ) tab_word.(String.length req-1));
  List.iter (Printf.printf "%s\n") !result;;

(* Boucle principale *) 
let () =
  let rec loop () =
    let req = print_string "Lettres : "; read_line () in
      if req = "!" then () else (find req; loop ())
    in loop ();;