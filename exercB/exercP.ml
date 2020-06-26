let input1 = read_line ()

let input2 = read_line ()

let rec kmpSearch txt lt pat lp i j =
  if j = lp
  then true
  else if i = lt
    then false
    else if txt.[i] = pat.[j]
         then kmpSearch txt lt pat lp (i+1) (j+1)
         else if txt.[i] = pat.[0]
          then kmpSearch txt lt pat lp (i+1) 1
          else kmpSearch txt lt pat lp (i+1) 0

let rec lcSubStr str1 ls1 str2 ls2 i k =
  if (i + k) > (String.length str1)
  then (k-1)
  else
    let nstr =  (String.sub str1 i k) in
    let lst  =  String.length nstr in
    if kmpSearch str2 ls2 nstr lst 0 0
    then lcSubStr str1 ls1 str2 ls2 0 (k+1) 
    else lcSubStr str1 ls1 str2 ls2 (i+1) k
let result = lcSubStr input1 (String.length input1) input2 (String.length input2) 0 1

let () = print_endline (string_of_int result)