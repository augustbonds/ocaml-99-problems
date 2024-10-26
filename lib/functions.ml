let rec last l =
  match l with
    | [] -> None
    | h :: [] -> Some h
    | _ :: t -> last t

let rec last l =
  match l with
    | [] -> None
    | [x] -> Some x
    | _ :: tail -> last tail

let rec last_two l =
  match l with
    | [] -> None
    | _ :: [] -> None
    | h :: h' :: [] -> Some (h, h')
    | _ :: h' :: t -> last_two (h' :: t)  (* could just use "t" here...  *)


let rec nth n l =
  match l with
    | [] -> None
    | h :: t -> match n with
      | 0 -> Some h
      | n -> nth (n-1) t

let rec nth idx list =
  match (idx, list) with
  | (_, []) -> None
  | (0, h :: _) -> Some h
  | (i, _ :: t) -> nth (i-1) t

let rec len l =
  match l with
   | [] -> 0
   | _ :: t -> 1 + len t

let len l =
  let rec aux acc l =
    match l with
      | [] -> acc
      | _ :: t -> aux (acc+1) t
  in
  aux 0 l

let reverse l =
  let rec aux acc l =
    match l with
      | [] -> acc
      | h :: t -> aux (h :: acc) t
  in
  aux [] l

let rec same l1 l2 = 
  match (l1, l2) with
    | ([], []) -> true 
    | (h1 :: x1 , h2 :: x2) -> if h1 = h2 then same x1 x2 else false
    | _ -> false
  

let is_palindrome l = 
  let rev = reverse l in
  same l rev

let is_palindrome l = 
  l = reverse l


let is_palindrome_str_opt s =
  let len = String.length s in
  let rec check i =
    if i >= 2 then true
    else if s.[i] <> s.[len-i-1] then false
    else check (i+1)
  in
  check 0

let is_palindrome_opt l = 
  let len = Array.length l in
  let rec check i = 
    if i >= 2 then true
    else if Array.get l i <> Array.get l (len-i-1) then false
    else check (i+1)
  in
  check 0

let rec concat l1 l2 =
  match (l1, l2) with 
    | ([], xs) -> xs
    | (x::xs, y) -> x :: concat xs y

type 'a node =
  | One of 'a
  | Many of 'a node list

let rec flatten l =
  match l with
  | [] -> []
  | x :: xs -> match x with 
    | One s -> s :: flatten xs
    | Many subl -> concat (flatten subl) (flatten xs)


(* This flatten was the suggested solution *)
let flatten list =
  let rec aux acc = function
    | [] -> acc
    | One x :: t -> aux (x::acc) t
    | Many l :: t -> aux (aux acc l) t
  in 
  reverse (aux [] list)