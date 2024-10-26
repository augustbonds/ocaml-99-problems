open Problems_99.Functions

let () = print_newline ()
let () = print_endline "Hello, World!"

let () = 
  let elem = last ["a"; "b"; "c"] in
  match elem with
  | Some elem -> (print_endline elem)
  | None -> ()

let () = 
  let elem = last ["a"; "b"; "c"; "d"] in
  match elem with
  | Some elem -> (print_endline elem)
  | None -> ()

let () =
  let elem = last_two ["a"; "b"; "c"] in
  match elem with
    | Some (x, y) -> print_string x; print_endline y
    | None -> ()

let () =
  let elem = last_two ["a"; "b"; "c"] in
  match elem with
    | Some (x, y) -> Printf.printf "%s%s\n" x y
    | None -> ()

let () =
  let elem = nth 4 ["a"; "b"; "c"; "d"; "e"; "f"] in
  match elem with
    | Some x -> Printf.printf "%s\n" x
    | None -> ()

let () =
  let elem = nth 4 ["a"; "b"; "c"; "d"; "e"; "f"] in
  match elem with
    | Some x -> Printf.printf "%s\n" x
    | None -> ()

let () =
  let length = len ["a"; "b"; "c"; "d"; "e"; "f"] in
  Printf.printf "length is :%d\n" length

let () =
  let length = len ["a"; "b"; "c"; "d"; "e"; "f"] in
  Printf.printf "length is :%d\n" length

let () = 
  match reverse ["a"; "b"; "c"] |> nth 0 with 
  | None -> ()
  | Some x -> Printf.printf "first in reverse: %s\n" x

let () = 
  match is_palindrome ["a"; "b"; "a"] with 
  | true -> Printf.printf "it's a palindrome!\n"
  | false -> Printf.printf "it's not a palindrome..\n"

let () = 
  match is_palindrome_str_opt "aba" with 
  | true -> Printf.printf "it's a palindrome!\n"
  | false -> Printf.printf "it's not a palindrome..\n"

let () = 
  match is_palindrome_opt (Array.of_list ["a"; "b"; "a"]) with 
  | true -> Printf.printf "it's a palindrome!\n"
  | false -> Printf.printf "it's not a palindrome..\n"