let do_regex = Str.regexp "do()"
let dont_regex = Str.regexp "don't()"
let mul_regex = Str.regexp "mul(\\([0-9]+\\),\\([0-9]+\\))"

let acc_line line domul =
  let rec process_muls position acc domul =
    if position < String.length line then
      let rec find_next_match best_match = function
        | [] -> best_match
        | (rgx, action) :: rest -> (
            try
              let start_pos = Str.search_forward rgx line position in
              match best_match with
              | None -> find_next_match (Some (start_pos, action)) rest
              | Some (best_start, _) when start_pos < best_start ->
                  find_next_match (Some (start_pos, action)) rest
              | _ -> find_next_match best_match rest
            with Not_found -> find_next_match best_match rest)
      in
      let actions =
        [
          (do_regex, fun () -> (true, acc));
          (dont_regex, fun () -> (false, acc));
          ( mul_regex,
            fun () ->
              if domul then
                let num1 = int_of_string (Str.matched_group 1 line) in
                let num2 = int_of_string (Str.matched_group 2 line) in
                let total = num1 * num2 in
                (domul, acc + total)
              else (domul, acc) );
        ]
      in
      match find_next_match None actions with
      | Some (start_pos, action) ->
          let domul, total = action () in
          process_muls (start_pos + 1) total domul
      | None -> (acc, domul)
    else (acc, domul)
  in
  process_muls 0 0 domul

let rec acc_lines acc domul =
  try
    let line = input_line stdin in
    let total, domul = acc_line line domul in
    acc_lines (acc + total) domul
  with End_of_file -> acc

let () =
  let output = acc_lines 0 true in
  Printf.printf "%d\n" output
