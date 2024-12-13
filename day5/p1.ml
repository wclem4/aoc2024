let read_row_of_ints split_char =
  let rec build_arr acc =
    try
      let line = input_line stdin in
      if line = "" then List.rev acc
      else
        let nums =
          line |> String.split_on_char split_char |> List.map int_of_string
        in
        build_arr (nums :: acc)
    with End_of_file -> List.rev acc
  in
  build_arr []

let rec validate_update rules update =
  match rules with
  | [] -> true
  | head :: tail -> (
      let num1_i = List.find_index (fun x -> x = List.nth head 0) update in
      let num2_i = List.find_index (fun x -> x = List.nth head 1) update in

      match (num1_i, num2_i) with
      | Some i1, Some i2 ->
          if i1 < i2 then validate_update tail update else false
      | _ -> validate_update tail update)

let rec count_valid_middles acc rules updates =
  match updates with
  | [] -> acc
  | head :: tail ->
      if validate_update rules head then
        count_valid_middles
          (acc + List.nth head (List.length head / 2))
          rules tail
      else count_valid_middles acc rules tail

let () =
  let rules = read_row_of_ints '|' in
  let updates = read_row_of_ints ',' in
  let output = count_valid_middles 0 rules updates in
  Printf.printf "%d\n" output
