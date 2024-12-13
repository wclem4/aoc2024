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
  | [] -> (true, -1, -1)
  | head :: tail -> (
      let num1_i = List.find_index (fun x -> x = List.nth head 0) update in
      let num2_i = List.find_index (fun x -> x = List.nth head 1) update in

      match (num1_i, num2_i) with
      | Some i1, Some i2 ->
          if i1 < i2 then validate_update tail update else (false, i1, i2)
      | _ -> validate_update tail update)

let swap l a b =
  let index_a = List.nth l a in
  let index_b = List.nth l b in
  let rec rebuild idx = function
    | [] -> []
    | hd :: tl ->
        if idx = a then index_b :: rebuild (idx + 1) tl
        else if idx = b then index_a :: rebuild (idx + 1) tl
        else hd :: rebuild (idx + 1) tl
  in
  rebuild 0 l

let rec fix_order rules update =
  let valid, bad_index1, bad_index2 = validate_update rules update in
  if valid then update
  else
    let reordered_update = swap update bad_index1 bad_index2 in
    fix_order rules reordered_update

let rec count_valid_middles acc rules updates =
  match updates with
  | [] -> acc
  | head :: tail ->
      let valid, _, _ = validate_update rules head in
      if valid then count_valid_middles acc rules tail
      else
        let reordered_update = fix_order rules head in
        count_valid_middles
          (acc + List.nth reordered_update (List.length reordered_update / 2))
          rules tail

let () =
  let rules = read_row_of_ints '|' in
  let updates = read_row_of_ints ',' in
  let output = count_valid_middles 0 rules updates in
  Printf.printf "%d\n" output
