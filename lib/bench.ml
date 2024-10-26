open Unix

let time () = gettimeofday ()

let format_time t =
  let tm = localtime t in
  Printf.sprintf "Current date and time: %04d-%02d-%02d %02d:%02d:%02d\n"
    (tm.tm_year + 1900) (* Year is counted from 1900 *)
    (tm.tm_mon + 1) (* Month is 0-based, so we add 1 *)
    tm.tm_mday (* Day of the month *) tm.tm_hour (* Hour *) tm.tm_min
    (* Minute *) tm.tm_sec (* Second *)

let print_time t = format_time t |> Printf.printf "%s"
let () = time () |> print_time

let rec warmup f iterations =
  if iterations <= 0 then ()
  else (
    ignore (f ());
    warmup f (iterations - 1))

let benchmark f duration_s =
  let rec loop count start_time =
    let now = time () in
    if now -. start_time >= duration_s then count
    else (
      ignore (f ());
      loop (count + 1) start_time)
  in
  let start_time = time () in
  let ops_count = loop 0 start_time in
  let end_time = time () in
  let ops_per_sec = float_of_int ops_count /. (end_time -. start_time) in
  ops_per_sec
