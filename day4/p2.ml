let find_xmas l1 l2 l3 =
  let length = String.length l2 in
  let rec check_a_start starting_index acc =
    try
      let a_index = Str.search_forward (Str.regexp "A") l2 starting_index in

      (* ..S.S.. *)
      (* ...A... *)
      (* ..M.M.. *)
      let check_mas_up i =
        if
          i >= 1
          && i < length - 1
          && String.sub l1 (i - 1) 1 = "S"
          && String.sub l1 (i + 1) 1 = "S"
          && String.sub l3 (i - 1) 1 = "M"
          && String.sub l3 (i + 1) 1 = "M"
        then 1
        else 0
      in

      (* ..M.M.. *)
      (* ...A... *)
      (* ..S.S.. *)
      let check_mas_down i =
        if
          i >= 1
          && i < length - 1
          && String.sub l1 (i - 1) 1 = "M"
          && String.sub l1 (i + 1) 1 = "M"
          && String.sub l3 (i - 1) 1 = "S"
          && String.sub l3 (i + 1) 1 = "S"
        then 1
        else 0
      in

      (* ..M.S.. *)
      (* ...A... *)
      (* ..M.S.. *)
      let check_mas_right i =
        if
          i >= 1
          && i < length - 1
          && String.sub l1 (i - 1) 1 = "M"
          && String.sub l1 (i + 1) 1 = "S"
          && String.sub l3 (i - 1) 1 = "M"
          && String.sub l3 (i + 1) 1 = "S"
        then 1
        else 0
      in

      (* ..S.M.. *)
      (* ...A... *)
      (* ..S.M.. *)
      let check_mas_left i =
        if
          i >= 1
          && i < length - 1
          && String.sub l1 (i - 1) 1 = "S"
          && String.sub l1 (i + 1) 1 = "M"
          && String.sub l3 (i - 1) 1 = "S"
          && String.sub l3 (i + 1) 1 = "M"
        then 1
        else 0
      in

      check_a_start (a_index + 1)
        (acc + check_mas_up a_index + check_mas_down a_index
       + check_mas_right a_index + check_mas_left a_index)
    with Not_found -> acc
  in
  check_a_start 0 0

let rec count_xmas acc l1 l2 l3 =
  try
    let line = input_line stdin in
    if l1 = "" then count_xmas acc line "" ""
    else if l2 = "" then count_xmas acc l1 line ""
    else if l3 = "" then
      let count = find_xmas l1 l2 line in
      count_xmas (acc + count) l1 l2 line
    else
      let count = find_xmas l2 l3 line in
      count_xmas (acc + count) l2 l3 line
  with End_of_file -> acc

let () =
  let output = count_xmas 0 "" "" "" in
  Printf.printf "%d\n" output
