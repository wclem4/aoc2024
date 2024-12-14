let build_input_array () =
  let rec read_lines acc =
    try
      let line = read_line () in
      let row = Array.of_seq (String.to_seq line) in
      read_lines (row :: acc)
    with End_of_file -> List.rev acc
  in
  let rows = read_lines [] in
  match rows with [] -> [||] | _ -> Array.of_list rows

let rec find_guard_position input_array row_num =
  match input_array with
  | [] -> (0, 0, ' ')
  | head :: tail -> (
      let guard =
        ( Array.find_index
            (fun x -> x = '^' || x = '>' || x = '<' || x = 'v')
            head,
          Array.find_opt
            (fun x -> x = '^' || x = '>' || x = '<' || x = 'v')
            head )
      in
      match guard with
      | Some col_num, Some direction -> (col_num, row_num, direction)
      | _ -> find_guard_position tail (row_num + 1))

let is_obstacle c = match c with '#' | 'O' -> true | _ -> false

let check_stuck direction input_array guard_x guard_y visited =
  let rec walk direction input_array guard_x guard_y =
    if direction = '^' then (
      if guard_y = 0 then false
      else if is_obstacle input_array.(guard_y - 1).(guard_x) then (
        input_array.(guard_y).(guard_x) <- '>';
        walk '>' input_array guard_x guard_y)
      else
        let key = (guard_x, guard_y, 'N') in
        input_array.(guard_y).(guard_x) <- 'N';
        input_array.(guard_y - 1).(guard_x) <- '^';
        if Hashtbl.mem visited key then true
        else (
          Hashtbl.add visited (guard_x, guard_y, 'N') true;
          walk direction input_array guard_x (guard_y - 1)))
    else if direction = 'v' then (
      if guard_y = Array.length input_array - 1 then false
      else if is_obstacle input_array.(guard_y + 1).(guard_x) then (
        input_array.(guard_y).(guard_x) <- '<';
        walk '<' input_array guard_x guard_y)
      else
        let key = (guard_x, guard_y, 'S') in
        input_array.(guard_y).(guard_x) <- 'S';
        input_array.(guard_y + 1).(guard_x) <- 'v';
        if Hashtbl.mem visited key then true
        else (
          Hashtbl.add visited (guard_x, guard_y, 'S') true;
          walk direction input_array guard_x (guard_y + 1)))
    else if direction = '>' then (
      if guard_x = Array.length input_array.(0) - 1 then false
      else if is_obstacle input_array.(guard_y).(guard_x + 1) then (
        input_array.(guard_y).(guard_x) <- 'v';
        walk 'v' input_array guard_x guard_y)
      else
        let key = (guard_x, guard_y, 'E') in
        input_array.(guard_y).(guard_x + 1) <- '>';
        input_array.(guard_y).(guard_x) <- 'E';
        if Hashtbl.mem visited key then true
        else (
          Hashtbl.add visited (guard_x, guard_y, 'E') true;
          walk direction input_array (guard_x + 1) guard_y))
    else if direction = '<' then (
      if guard_x = 0 then false
      else if is_obstacle input_array.(guard_y).(guard_x - 1) then (
        input_array.(guard_y).(guard_x) <- '^';
        walk '^' input_array guard_x guard_y)
      else
        let key = (guard_x, guard_y, 'W') in
        input_array.(guard_y).(guard_x - 1) <- '<';
        input_array.(guard_y).(guard_x) <- 'W';
        if Hashtbl.mem visited key then true
        else (
          Hashtbl.add visited (guard_x, guard_y, 'W') true;
          walk direction input_array (guard_x - 1) guard_y))
    else false
  in
  walk direction input_array guard_x guard_y

let add_obstacles input_array direction guard_x guard_y =
  let rows = Array.length input_array in
  let cols = if rows > 0 then Array.length input_array.(0) else 0 in

  let update_array arr i j =
    let copy = Array.map Array.copy arr in
    copy.(i).(j) <- 'O';
    copy
  in

  let rec loop_cols i j acc =
    if j >= cols then acc
    else
      let modified = update_array input_array i j in
      let visited = Hashtbl.create 10000 in
      let is_stuck = check_stuck direction modified guard_x guard_y visited in
      if is_stuck then loop_cols i (j + 1) (acc + 1)
      else loop_cols i (j + 1) acc
  in

  let rec loop_rows i acc =
    if i >= rows then acc
    else
      let row_sum = loop_cols i 0 0 in
      loop_rows (i + 1) (acc + row_sum)
  in

  loop_rows 0 0

let () =
  let input_array = build_input_array () in
  let guard_x, guard_y, direction =
    find_guard_position (Array.to_list input_array) 0
  in

  let output = add_obstacles input_array direction guard_x guard_y in
  Printf.printf "%d\n" output
