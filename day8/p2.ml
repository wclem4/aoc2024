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

let create_freq_map input_array =
  let freq_map = Hashtbl.create 10 in
  Array.iteri
    (fun i row ->
      Array.iteri
        (fun j char ->
          let coord = (j, i) in
          if char <> '.' then (
            input_array.(i).(j) <- '#';
            match Hashtbl.find_opt freq_map char with
            | Some positions ->
                Hashtbl.replace freq_map char (coord :: positions)
            | None -> Hashtbl.add freq_map char [ coord ]))
        row)
    input_array;
  freq_map

let in_bounds (x, y) max_x max_y = x >= 0 && x < max_x && y >= 0 && y < max_y

let add_backward_antinodes (x, y) (fx, fy) max_x max_y input_array =
  let dx = fx - x in
  let dy = fy - y in
  let before_x = x - dx in
  let before_y = y - dy in
  let rec loop acc x y =
    if in_bounds (x, y) max_x max_y then
      if input_array.(y).(x) <> '#' then (
        input_array.(y).(x) <- '#';
        loop (acc + 1) (x - dx) (y - dy))
      else loop acc (x - dx) (y - dy)
    else acc
  in
  loop 0 before_x before_y

let add_forward_antinodes (x, y) (fx, fy) max_x max_y input_array =
  let dx = fx - x in
  let dy = fy - y in
  let after_x = fx + dx in
  let after_y = fy + dy in
  let rec loop acc x y =
    if in_bounds (x, y) max_x max_y then
      if input_array.(y).(x) <> '#' then (
        input_array.(y).(x) <- '#';
        loop (acc + 1) (x + dx) (y + dy))
      else loop acc (x + dx) (y + dy)
    else acc
  in
  loop 0 after_x after_y

let count_antinodes input_array positions =
  let max_x = Array.length input_array.(0) in
  let max_y = Array.length input_array in
  let rec loop positions acc =
    match positions with
    | [] -> acc
    | (x, y) :: tail ->
        let count =
          List.fold_left
            (fun count (fx, fy) ->
              count
              + add_forward_antinodes (x, y) (fx, fy) max_x max_y input_array
              + add_backward_antinodes (x, y) (fx, fy) max_x max_y input_array)
            1 tail
        in
        loop tail (count + acc)
  in
  loop positions 0

let () =
  let input_array = build_input_array () in
  let table = create_freq_map input_array in

  let output =
    Hashtbl.fold
      (fun _ positions acc -> acc + count_antinodes input_array positions)
      table 0
  in
  Printf.printf "%d\n" output
