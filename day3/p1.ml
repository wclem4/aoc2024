let regex = Str.regexp "mul(\\([0-9]+\\),\\([0-9]+\\))"

let acc_line line =
  let rec process_muls start_idx acc =
    try
      let _ = Str.search_forward regex line start_idx in
      let num1 = Str.matched_group 1 line in
      let num2 = Str.matched_group 2 line in
      process_muls (Str.match_end ())
        (acc + (int_of_string num1 * int_of_string num2))
    with Not_found -> acc
  in
  process_muls 0 0

let rec acc_lines acc =
  try
    let line = input_line stdin in
    let total = acc_line line in
    acc_lines (acc + total)
  with End_of_file -> acc

let () =
  let output = acc_lines 0 in
  Printf.printf "%d\n" output
