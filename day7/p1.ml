let extract_numbers line =
  let regex = Str.regexp "[0-9]+" in
  let rec extract acc pos =
    try
      let _ = Str.search_forward regex line pos in
      let number = Str.matched_string line in
      extract (int_of_string number :: acc) (Str.match_end ())
    with Not_found -> List.rev acc
  in
  extract [] 0

type operator = Add | Mul

let eval op a b = match op with Add -> a + b | Mul -> a * b

let rec matches_sum numbers operators sum =
  match numbers with
  | [] -> false
  | [ n ] -> n = sum
  | head :: rest ->
      List.exists
        (fun op ->
          let next = List.hd rest in
          let first_result = eval op head next in
          let new_rest = List.tl rest in
          matches_sum (first_result :: new_rest) operators sum)
        operators

let get_first lst =
  match lst with [] -> None | head :: tail -> Some (head, tail)

let rec count_sums acc =
  try
    let line = input_line stdin in
    let nums = extract_numbers line in
    let operators = [ Add; Mul ] in

    match get_first nums with
    | None -> count_sums acc
    | Some (head, tail) ->
        let matches = matches_sum tail operators head in
        if matches then count_sums (acc + head) else count_sums acc
  with End_of_file -> acc

let () =
  let output = count_sums 0 in
  Printf.printf "%d\n" output
