
let input1 = read_line ()

let input2 = read_line ()


let rec kmpSearch txt pat i j =
  if j == (String.length pat)
  then true
  else if i == (String.length txt)
    then false
    else let check1 = (String.get txt i) == (String.get pat j) in
      let check2 = (String.get txt i) == (String.get pat 0) in
        if check1
        then kmpSearch txt pat (i+1) (j+1)
        else if check2
          then kmpSearch txt pat (i+1) 1
          else kmpSearch txt pat (i+1) 0

(* 
  k começa em 1 (1 letra)
  i 0

  Se chega ao fim -> k - 1
*)
let rec lcSubStr str1 str2 i k =
  if (i + k) > (String.length str1)
  then (k-1)
  else if kmpSearch str2 (String.sub str1 i k) 0 0
    then lcSubStr str1 str2 0 (k+1) 
    else lcSubStr str1 str2 (i+1) k

let result = lcSubStr input1 input2 0 1

let () = print_endline (string_of_int result)

