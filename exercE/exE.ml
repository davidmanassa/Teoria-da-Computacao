
(*

  ñ terminais -> Maiúsculas
  terminais -> Minúsculas

  Simbolo inicial -> (ñ terminal) S

  Produções
  N -> a (sequência não vazia de simbolos)
  Epsilon = _

*)

(* Leitura

  int n : quantas produções têm a gramática
  n linhas : produções da gramática

*)

exception BreakLoop

let n = read_int ()
let productions = Array.make n ('-', [])

let getInitial (a, _) = a 
let getList (_, b) = b

let rec read_inputs k n =
  if k = n
  then ()
  else (
    let line = read_line () in
      let initial = line.[0] in
      (*
        "S -> A B C d"
        " -> " 4 Chars + "S" + "A B C d" 
        12 Chars - 5 = 7
        7 + 1 = 8
        8 / 2 = 4 
        4 positions
        pos 0 = 5
      *)
        let rest_list = ref [] in (
            for i = 0 to (((String.length line) - 4 (* 5 - 1 *)) / 2) - 1 do
              rest_list := !rest_list@[(line.[(5+(i*2))])]
            done;
            (productions.(k) <- (initial, !rest_list));
            read_inputs (k+1) n
          )
  )

let () = read_inputs 0 n

let non_terminal_list = ref []
let () = 
  for i = 0 to n-1 do
    if not (List.mem (getInitial productions.(i)) (!non_terminal_list))
    then (non_terminal_list := !non_terminal_list@[(getInitial productions.(i))])
  done

let nullTbl = Hashtbl.create (List.length !non_terminal_list)
let firstTbl = Hashtbl.create (List.length !non_terminal_list)
let followTbl = Hashtbl.create (List.length !non_terminal_list)

let isUppercase c =
  let string_value = String.make 1 c in
    if String.uppercase string_value = string_value then true else false

let rec list_to_string lst =
  match lst with
    | a::b -> (" "^(String.make 1 a))^(list_to_string b)
    | [] -> ""

(*
    NULL
*)
let i_null_checked = ref []
let rec null c =
  if Hashtbl.mem nullTbl c
  then Hashtbl.find nullTbl c
  else (
    i_null_checked := [];
    let null_result = null_aux0 c in
      Hashtbl.add nullTbl c null_result;
      null_result
  )
and null_aux0 c = (* c não terminal *)
  if c = 'S' then false
  else (
    let ret = ref false in
      (try
        for i = 0 to n-1 do
          if (not (List.mem i !i_null_checked))
          then (if getInitial (productions.(i)) = c
            then (i_null_checked := ((!i_null_checked)@[i]);
              if (null_aux (getList (productions.(i))))
              then raise BreakLoop)
          )
        done
      with BreakLoop -> ret := true);
      !ret
    )
and null_aux c_list =
  match c_list with
    | a::b -> if a = '_'
      then true
      else (if isUppercase a
        then (if null_aux0 a then true else null_aux b)
        else null_aux b)
    | [] -> false

(*
        FIRST
*)
let i_first_checked = ref []
let rec first c =
  if Hashtbl.mem firstTbl c
  then Hashtbl.find firstTbl c
  else (
    i_first_checked := [];
    let first_result = first_aux0 c in
      Hashtbl.add firstTbl c first_result;
      first_result
  )
and first_aux0 c =
  if not (isUppercase c)
  then [c]
  else (
    let return = ref [] in (
      for i = 0 to n-1 do
        if not (List.mem i !i_first_checked)
        then (if (getInitial (productions.(i)) == c)
          then (
            i_first_checked := !i_first_checked@[i];
            (return := ((!return)@(first_aux (getList (productions.(i))))))
          ))
        else ()
      done; !return
    )
  )
and first_aux lst =
  match lst with
    | a::b -> if not (a == '_')
      then (
        if not (isUppercase a)
        then [a]
        else (
          if null a (* A esta altura já sabemos que a é não terminal *)
          then ((first_aux b)@(first_aux0 a)) (* a é null *)
          else (first_aux0 a)
        )
      ) else []
    | [] -> []

(*
        FOLLOW
*)
let i_follow_checked = ref []
let to_add = ref []
let rec follow c =
  if Hashtbl.mem followTbl c
  then Hashtbl.find followTbl c
  else (
    i_follow_checked := [];
    let follow_result = follow_aux0 c in
      Hashtbl.add followTbl c follow_result;
      follow_result
  )
and next l =
  match l with
    | a::b -> (to_add := b; follow a)
    | [] -> [] (* Não usado *)
and follow_aux0 c =
  let return = ref [] in (
    for i = 0 to n-1 do
      if not (List.mem i !i_follow_checked)
        then (
          i_follow_checked := !i_follow_checked@[i];
          (return := ((!return)@(follow_aux c (getInitial productions.(i)) (getList (productions.(i))) false)))
        )
    done; if !to_add == []
      then !return
      else (
        while !to_add != [] do
          return := (next !to_add)@(!return);
        done; !return
      )
  )
and follow_aux c c_from lst canDoFollow =
  match lst with
    | a::b -> if canDoFollow
      then (
        if (isUppercase a)
        then (
          if null a
          then (
            if a = c
            then (first a)@(follow_aux c c_from b true)
            else (
              (if c != c_from
              then (to_add := !to_add@[c_from]));
              (first a)@(follow_aux c c_from b false)
            )
          )
          else (first a)@(follow_aux c c_from b false)
        )
        else ((first a)@(follow_aux c c_from b false)) (* a é terminal *)
      )
      else (
        if a = c
        then follow_aux c c_from b true
        else follow_aux c c_from b false
      )
    | [] -> (if canDoFollow && c_from != c 
      then (to_add := !to_add@[c_from]));
      if c = 'S'
      then ['#']
      else []


let rec print_nulls lst =
  match lst with
    | a::b -> print_string "NULL("; print_string (String.make 1 a); print_string ") = ";
      if null a then print_string "True\n" else print_string "False\n"; print_nulls b
    | [] -> ()

(*

  normalize l = l sem duplicados, de forma eficiente ie. linear!

  Créditos ao esqueleto dado pelo professor no exercicio D !

*) 
let normalize l =
  let tbl = Hashtbl.create (List.length l) in
  let f l e = try let _ = Hashtbl.find tbl e in l
              with Not_found ->  Hashtbl.add tbl e (); e::l
  in  List.rev (List.fold_left f [] l)

let rec print_firsts lst =
  match lst with
    | a::b -> print_string "FIRST("; print_string (String.make 1 a); print_string ") =";
    let lst1 = normalize (first a) in
      let sorted = List.sort compare lst1 in
        (sorted |> list_to_string |> print_string); print_string "\n"; print_firsts b
    | [] -> ()

let rec print_follows lst =
  match lst with
    | a::b -> print_string "FOLLOW("; print_string (String.make 1 a); print_string ") ="; 
    let lst1 = normalize (follow a) in
      let sorted = List.sort compare lst1 in
      (sorted |> list_to_string |> print_string); print_string "\n"; print_follows b
    | [] -> ()


let () = print_nulls !non_terminal_list
let () = print_firsts !non_terminal_list
let () = print_follows !non_terminal_list
