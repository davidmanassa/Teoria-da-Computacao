
let input1 = read_line ()

let input2 = read_line ()

let lcSubStr x y m n =
  let stuff = Array.make_matrix (m+1) (n+1) 0 in
    let result = ref 0 in
      for i = 0 to m do
        for j = 0 to n do
          if i==0 || j==0
          then (stuff.(i).(j) <- 0)
          else if (String.get x (i-1)) == (String.get y (j-1))
            then (stuff.(i).(j) <- (stuff.(i-1).(j-1) + 1);
              result := (max !result stuff.(i).(j)))
            else stuff.(i).(j) <- 0
        done;
      done;
      !result

let result = lcSubStr input1 input2 (String.length input1) (String.length input2)

let () = print_endline (string_of_int result)

