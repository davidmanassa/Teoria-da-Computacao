
let minimum a b c =
  min a (min b c)

let levenshteinDistance str1 str2 =
  let n1 = String.length str1 in
    let n2 = String.length str2 in
      let matrix = Array.make_matrix (n1 + 1) (n2 + 1) 0 in
        begin
          for x = 0 to n1 do
            matrix.(x).(0) <- x
          done;
          for y = 0 to n2 do
            matrix.(0).(y) <- y
          done;
          for x = 1 to n1 do
            for y = 1 to n2 do
                if str1.[(x - 1)] = str2.[(y - 1)]
                then matrix.(x).(y) <- matrix.(x - 1).(y - 1)
                else matrix.(x).(y) <- minimum (matrix.(x - 1).(y) + 1) (matrix.(x).(y - 1) + 1) (matrix.(x - 1).(y - 1) + 1)
            done
          done;
          matrix.(n1).(n2)
        end


let input1 = read_line ()

let input2 = read_line ()

let () = print_endline (string_of_int (levenshteinDistance input1 input2))