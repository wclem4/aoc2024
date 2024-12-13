let find_xmas l1 l2 l3 l4 =
  let length = String.length l1 in
  let rec check_x_start starting_index acc =
    (* ...XMAS *)
    (* ..MMM.. *)
    (* .A.A.A. *)
    (* S..S..S *)
    try
      let x_index = Str.search_forward (Str.regexp "X") l1 starting_index in

      let check_down i =
        if
          String.sub l2 i 1 = "M"
          && String.sub l3 i 1 = "A"
          && String.sub l4 i 1 = "S"
        then 1
        else 0
      in
      let check_right i =
        if i <= length - 4 && String.sub l1 (i + 1) 3 = "MAS" then 1 else 0
      in
      let check_right_diagonal i =
        if
          i <= length - 4
          && String.sub l2 (i + 1) 1 = "M"
          && String.sub l3 (i + 2) 1 = "A"
          && String.sub l4 (i + 3) 1 = "S"
        then 1
        else 0
      in
      let check_left_diagonal i =
        if
          i >= 3
          && String.sub l2 (i - 1) 1 = "M"
          && String.sub l3 (i - 2) 1 = "A"
          && String.sub l4 (i - 3) 1 = "S"
        then 1
        else 0
      in
      if l2 = "" || l3 = "" || l4 = "" then
        check_x_start (x_index + 1) (acc + check_right x_index)
      else
        check_x_start (x_index + 1)
          (acc + check_down x_index + check_right x_index
          + check_right_diagonal x_index
          + check_left_diagonal x_index)
    with Not_found -> acc
  in
  let rec check_s_start starting_index acc =
    (* ...SAMX *)
    (* ..AAA.. *)
    (* .M.M.M. *)
    (* X..X..X *)
    try
      let s_index = Str.search_forward (Str.regexp "S") l1 starting_index in

      let check_down i =
        if
          String.sub l2 i 1 = "A"
          && String.sub l3 i 1 = "M"
          && String.sub l4 i 1 = "X"
        then 1
        else 0
      in
      let check_right i =
        if i <= length - 4 && String.sub l1 (i + 1) 3 = "AMX" then 1 else 0
      in
      let check_right_diagonal i =
        if
          i <= length - 4
          && String.sub l2 (i + 1) 1 = "A"
          && String.sub l3 (i + 2) 1 = "M"
          && String.sub l4 (i + 3) 1 = "X"
        then 1
        else 0
      in
      let check_left_diagonal i =
        if
          i >= 3
          && String.sub l2 (i - 1) 1 = "A"
          && String.sub l3 (i - 2) 1 = "M"
          && String.sub l4 (i - 3) 1 = "X"
        then 1
        else 0
      in
      if l2 = "" || l3 = "" || l4 = "" then
        check_s_start (s_index + 1) (acc + check_right s_index)
      else
        check_s_start (s_index + 1)
          (acc + check_down s_index + check_right s_index
          + check_right_diagonal s_index
          + check_left_diagonal s_index)
    with Not_found -> acc
  in
  check_x_start 0 0 + check_s_start 0 0

let rec count_xmas acc l1 l2 l3 l4 =
  try
    let line = input_line stdin in
    if l1 = "" then count_xmas acc line "" "" ""
    else if l2 = "" then count_xmas acc l1 line "" ""
    else if l3 = "" then count_xmas acc l1 l2 line ""
    else if l4 = "" then
      let count = find_xmas l1 l2 l3 line in
      count_xmas (acc + count) l1 l2 l3 line
    else
      let count = find_xmas l2 l3 l4 line in
      count_xmas (acc + count) l2 l3 l4 line
  with End_of_file ->
    let count_last =
      find_xmas l2 l3 l4 "" + find_xmas l3 l4 "" "" + find_xmas l4 "" "" ""
    in
    acc + count_last

let () =
  let output = count_xmas 0 "" "" "" "" in
  Printf.printf "%d\n" output
