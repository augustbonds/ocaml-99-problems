let () = print_newline ()
let () = print_endline "Hello, World!"

let rec last l =
  match l with
    | [] -> None
    | h :: [] -> Some h
    | _ :: t -> last t

let () = 
  let elem = last ["a"; "b"; "c"] in
  match elem with
  | Some elem -> (print_endline elem)
  | None -> ()


let rec last l =
  match l with
    | [] -> None
    | [x] -> Some x
    | _ :: tail -> last tail

let () = 
  let elem = last ["a"; "b"; "c"; "d"] in
  match elem with
  | Some elem -> (print_endline elem)
  | None -> ()


let rec last_two l =
  match l with
    | [] -> None
    | _ :: [] -> None
    | h :: h' :: [] -> Some (h, h')
    | _ :: h' :: t -> last_two (h' :: t)  (* could just use "t" here...  *)

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


let rec nth n l =
  match l with
    | [] -> None
    | h :: t -> match n with
      | 0 -> Some h
      | n -> nth (n-1) t

let () =
  let elem = nth 4 ["a"; "b"; "c"; "d"; "e"; "f"] in
  match elem with
    | Some x -> Printf.printf "%s\n" x
    | None -> ()

let rec nth idx list =
  match (idx, list) with
  | (_, []) -> None
  | (0, h :: _) -> Some h
  | (i, _ :: t) -> nth (i-1) t

let () =
  let elem = nth 4 ["a"; "b"; "c"; "d"; "e"; "f"] in
  match elem with
    | Some x -> Printf.printf "%s\n" x
    | None -> ()

let rec len l =
  match l with
   | [] -> 0
   | _ :: t -> 1 + len t

let () =
  let length = len ["a"; "b"; "c"; "d"; "e"; "f"] in
  Printf.printf "length is :%d\n" length

let len l =
  let rec aux acc l =
    match l with
      | [] -> acc
      | _ :: t -> aux (acc+1) t
  in
  aux 0 l

let () =
  let length = len ["a"; "b"; "c"; "d"; "e"; "f"] in
  Printf.printf "length is :%d\n" length

let reverse l =
  let rec aux acc l =
    match l with
      | [] -> acc
      | h :: t -> aux (h :: acc) t
  in
  aux [] l


let () = 
  match reverse ["a"; "b"; "c"] |> nth 0 with 
  | None -> ()
  | Some x -> Printf.printf "first in reverse: %s\n" x
