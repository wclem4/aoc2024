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
          if char <> '.' then
            match Hashtbl.find_opt freq_map char with
            | Some positions ->
                Hashtbl.replace freq_map char (coord :: positions)
            | None -> Hashtbl.add freq_map char [ coord ])
        row)
    input_array;
  freq_map

let outer_points (x, y) (fx, fy) =
  let dx = fx - x in
  let dy = fy - y in
  let before = (x - dx, y - dy) in
  let after = (fx + dx, fy + dy) in
  (before, after)

let in_bounds (x, y) max_x max_y = x >= 0 && x < max_x && y >= 0 && y < max_y

let count_antinodes input_array positions =
  let max_x = Array.length input_array.(0) in
  let max_y = Array.length input_array in

  let try_add_antinode (x, y) =
    if in_bounds (x, y) max_x max_y && input_array.(y).(x) <> '#' then (
      input_array.(y).(x) <- '#';
      1)
    else 0
  in

  let rec loop positions acc =
    match positions with
    | [] -> acc
    | (x, y) :: tail ->
        let count =
          List.fold_left
            (fun count (fx, fy) ->
              let (before_x, before_y), (after_x, after_y) =
                outer_points (x, y) (fx, fy)
              in
              count
              + try_add_antinode (before_x, before_y)
              + try_add_antinode (after_x, after_y))
            0 tail
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
