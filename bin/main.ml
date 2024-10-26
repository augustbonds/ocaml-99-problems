open Problems_99.Functions
open Problems_99.Bench

let () = print_newline ()
let () = print_endline "Hello, World!"

let () =
  let elem = last [ "a"; "b"; "c" ] in
  match elem with Some elem -> print_endline elem | None -> ()

let () =
  let elem = last [ "a"; "b"; "c"; "d" ] in
  match elem with Some elem -> print_endline elem | None -> ()

let () =
  let elem = last_two [ "a"; "b"; "c" ] in
  match elem with
  | Some (x, y) ->
      print_string x;
      print_endline y
  | None -> ()

let () =
  let elem = last_two [ "a"; "b"; "c" ] in
  match elem with Some (x, y) -> Printf.printf "%s%s\n" x y | None -> ()

let () =
  let elem = nth 4 [ "a"; "b"; "c"; "d"; "e"; "f" ] in
  match elem with Some x -> Printf.printf "%s\n" x | None -> ()

let () =
  let elem = nth 4 [ "a"; "b"; "c"; "d"; "e"; "f" ] in
  match elem with Some x -> Printf.printf "%s\n" x | None -> ()

let () =
  let length = len [ "a"; "b"; "c"; "d"; "e"; "f" ] in
  Printf.printf "length is :%d\n" length

let () =
  let length = len [ "a"; "b"; "c"; "d"; "e"; "f" ] in
  Printf.printf "length is :%d\n" length

let () =
  match reverse [ "a"; "b"; "c" ] |> nth 0 with
  | None -> ()
  | Some x -> Printf.printf "first in reverse: %s\n" x

let () =
  match is_palindrome [ "a"; "b"; "a" ] with
  | true -> Printf.printf "it's a palindrome!\n"
  | false -> Printf.printf "it's not a palindrome..\n"

let () =
  match is_palindrome_str_opt "aba" with
  | true -> Printf.printf "it's a palindrome!\n"
  | false -> Printf.printf "it's not a palindrome..\n"

let () =
  match is_palindrome_opt (Array.of_list [ "a"; "b"; "a" ]) with
  | true -> Printf.printf "it's a palindrome!\n"
  | false -> Printf.printf "it's not a palindrome..\n"

let test_input = Array.of_list [ "a"; "b"; "a" ]
let test_input2 = [ "a"; "b"; "a" ]
let function_under_test1 () = ignore (is_palindrome_opt test_input)
let function_under_test2 () = ignore (is_palindrome test_input2)

let functions_to_test =
  [
    ("array_based", function_under_test1);
    ("list_based", function_under_test2);
  ]

let rec run_benchmarks l s_per_benchmark warmup_iters =
  match l with
  | [] -> ()
  | (name, f) :: t ->
      Printf.printf "Warming up %s %d times\n" name warmup_iters;
      warmup f warmup_iters;
      Printf.printf "Running %s for %d s\n" name s_per_benchmark;
      let ops_per_sec = benchmark f (float_of_int s_per_benchmark) in
      Printf.printf "Benchmark %s did %.0f ops/s\n" name ops_per_sec;
      run_benchmarks t s_per_benchmark warmup_iters

let run_benchmark_suite functions =
  Printf.printf "Going to run %d benchmarks\n" (List.length functions);
  run_benchmarks functions 3 100
(*let _ = benchmark (function_under_test1) 5.
  let _ = benchmark (function_under_test2) 5. *)

(* let () = run_benchmark_suite functions_to_test *)
