let input1 = read_line ()

let input2 = read_line ()

(*

  Fonte: https://www.geeksforgeeks.org/kmp-algorithm-for-pattern-searching/

  Versão simples e adaptada do Algoritmo de Knuth-Morris-Pratt

  Esta função irá procurar uma palavra 'pat' na string 'txt', percorrendo a string de uma em uma posição.

  Retortaná um boleano mal encontre uma correspondecia ou no final da string se não encontrar nada.

  i representa a posição em que estamos no texto e j o tamanho da sequencia encontrada até ao momento

  Por exemplo: Se estivermos à procura da palavra ACGT em uma string qualquer
  Se j igual a 0 significa que estamos à procura de um A
  Se encotrarmos um A, j será incrementado em 1 (O que significa que na próxima iteração irá procurar por C)
  Se na próxima iteração não encontrar um C, j retornará a 0 ou 1 [Se invês de C encontrarmos outro A (O que significa o inicio da palavra que estamos à procura)]
  Se j for igual ao tamanho da palavra que estamos à procura significa que encontramos essa palavra na string
  Se i chegar ao final da string significa que não encontramos a palavra na string

*)
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
  Queremos o tamanho da maior palavra continua comum nas duas str

  k representa o numero de letras que estamos a procurar
  i a posição em que estamos na str1 (precorremos esta string de 1 em 1 até encontrar uma palavra igual na str2 ou até ao fim)

  Iremos procurar palavras de tamanho k da str1 na str2 (com o algoritmo de Algoritmo de Knuth-Morris-Pratt)
  
  Sempre que encontrar uma palavra de tamanho k, passamos a procurar outra palavra de tamanho k+1
  Se não encontrar a nehuma palavra de tamanho k na str2 significa que chegamos ao final e o tamanho da maior palavra é k-1

  k começa em 1 (1 letra)
  i 0
*)
let rec lcSubStr str1 str2 i k =
  if (i + k) > (String.length str1)
  then (k-1)
  else if kmpSearch str2 (String.sub str1 i k) 0 0
    then lcSubStr str1 str2 0 (k+1) 
    else lcSubStr str1 str2 (i+1) k

let result = lcSubStr input1 input2 0 1

let () = print_endline (string_of_int result)
