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

let rec walk acc direction input_array guard_x guard_y =
  if direction = '^' then
    if guard_y = 0 then acc + 1
    else if input_array.(guard_y - 1).(guard_x) = '#' then (
      input_array.(guard_y).(guard_x) <- '>';
      walk acc '>' input_array guard_x guard_y)
    else if input_array.(guard_y - 1).(guard_x) = 'X' then (
      input_array.(guard_y - 1).(guard_x) <- '^';
      input_array.(guard_y).(guard_x) <- 'X';
      walk acc direction input_array guard_x (guard_y - 1))
    else (
      input_array.(guard_y - 1).(guard_x) <- '^';
      input_array.(guard_y).(guard_x) <- 'X';
      walk (acc + 1) direction input_array guard_x (guard_y - 1))
  else if direction = 'v' then
    if guard_y = Array.length input_array - 1 then acc + 1
    else if input_array.(guard_y + 1).(guard_x) = '#' then (
      input_array.(guard_y).(guard_x) <- '<';
      walk acc '<' input_array guard_x guard_y)
    else if input_array.(guard_y + 1).(guard_x) = 'X' then (
      input_array.(guard_y + 1).(guard_x) <- 'v';
      input_array.(guard_y).(guard_x) <- 'X';
      walk acc direction input_array guard_x (guard_y + 1))
    else (
      input_array.(guard_y + 1).(guard_x) <- 'v';
      input_array.(guard_y).(guard_x) <- 'X';
      walk (acc + 1) direction input_array guard_x (guard_y + 1))
  else if direction = '>' then
    if guard_x = Array.length input_array.(0) - 1 then acc + 1
    else if input_array.(guard_y).(guard_x + 1) = '#' then (
      input_array.(guard_y).(guard_x) <- 'v';
      walk acc 'v' input_array guard_x guard_y)
    else if input_array.(guard_y).(guard_x + 1) = 'X' then (
      input_array.(guard_y).(guard_x + 1) <- '>';
      input_array.(guard_y).(guard_x) <- 'X';
      walk acc direction input_array (guard_x + 1) guard_y)
    else (
      input_array.(guard_y).(guard_x + 1) <- '>';
      input_array.(guard_y).(guard_x) <- 'X';
      walk (acc + 1) direction input_array (guard_x + 1) guard_y)
  else if direction = '<' then
    if guard_x = 0 then acc + 1
    else if input_array.(guard_y).(guard_x - 1) = '#' then (
      input_array.(guard_y).(guard_x) <- '^';
      walk acc '^' input_array guard_x guard_y)
    else if input_array.(guard_y).(guard_x - 1) = 'X' then (
      input_array.(guard_y).(guard_x - 1) <- '<';
      input_array.(guard_y).(guard_x) <- 'X';
      walk acc direction input_array (guard_x - 1) guard_y)
    else (
      input_array.(guard_y).(guard_x - 1) <- '<';
      input_array.(guard_y).(guard_x) <- 'X';
      walk (acc + 1) direction input_array (guard_x - 1) guard_y)
  else acc

let () =
  let input_array = build_input_array () in
  let guard_x, guard_y, direction =
    find_guard_position (Array.to_list input_array) 0
  in

  let output = walk 0 direction input_array guard_x guard_y in
  Printf.printf "%d\n" output
