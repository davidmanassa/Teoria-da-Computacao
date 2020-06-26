(** Algoritmo de MacNaughton - Yamada para calcular uma expressão regular a partir de um autómato **)
open List
open Scanf
open Array


(* Definição dos tipos de dados para os autómatos e para as expressões regulares *)
(* TODO!!!! type .... type automato = .... *)
    
type regexp =
 | V  
 | E
 | C of char
 | U of regexp * regexp 
 | P of regexp * regexp 
 | S of regexp    

(* simple pretty printing function *)
let rec string_of_regexp s =
  match s with
  | V       -> "0"
  | E       -> "1"
  | C  c    -> String.make 1 c    
  | U (f,g) -> "("^(string_of_regexp f)^" + "^(string_of_regexp g)^")"
  | P (f,g) -> "("^(string_of_regexp f)^" . "^(string_of_regexp g)^")"
  | S s     -> (string_of_regexp s)^"*"

(* definições das funções de leitura *)
(* ... TODO!!! ... *)
(* leitura dos dados de input *)
let leitura () = (* TODO *) assert false


(* "max" = número de estados do autómato "maq" *)
let max,maq = leitura ()


(*Funções de calculo da expressão regular a partir do autómato "maq"*)

let mat = Array.init (max+1) (fun _ -> Array.init (max+1) (fun _ -> (Array.init (max+2) (fun _ -> V))))

(* normalize l = l sem duplicados, de forma eficiente ie. linear! *) 
let normalize l =
  let tbl = Hashtbl.create (List.length l) in
  let f l e = try let _ = Hashtbl.find tbl e in l
              with Not_found ->  Hashtbl.add tbl e (); e::l
  in  List.rev (List.fold_left f [] l)

(*
simplify= função que simplifica "um pouco" a expressão regular
realisa uma simplificação maior do que a que foi sugerida no enunciado do problema
*)  
let rec simplify (a:regexp) = 
 match a with 
 | U (r,s) ->
   let sr = simplify r in
   let ss = simplify s in
   if sr = V then ss
   else if ss = V then sr
   else if ss = sr then sr
   else U (sr,ss) 
 | P (r,s) ->
   let sr = simplify r in
   let ss = simplify s in
   if sr = V then V
   else if ss = V then V
   else if sr = E then ss
     else if ss = E then sr
   else P (sr,ss) 
 | S r -> let sr = simplify r in
   if sr = V || sr = E 
   then E else (
     match sr with
       U (E,rr) | U (rr,E) -> S rr       
       | _ -> S sr
     )
 |  _ -> a
 
(* TODO!!!  - calcular a expressão regular - funções em falta aqui*)



(*calculo efecivo da expressão regular resultante, a partir das funções cuja definição se espera  *)
let result = V  (*TODO!!!!! (substituir "V" pelo código em falta *)
  

(* vizualização do resultado, simplificado *)
let () = result |> simplify |> string_of_regexp |> print_endline
(* equivalente a: let () = print_endline (string_of_regexp (simplify result)) *)
    
