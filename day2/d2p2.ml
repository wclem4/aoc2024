type order = Asc | Desc | Unknown

let rec is_safe lst prev order =
  let max_jump = 3 in
  match lst with
  | [] -> true
  | head :: tail -> (
      match order with
      | Asc ->
          if head > prev && head - prev <= max_jump then is_safe tail head order
          else false
      | Desc ->
          if head < prev && prev - head <= max_jump then is_safe tail head order
          else false
      | Unknown ->
          if head > prev && head - prev <= max_jump then is_safe tail head Asc
          else if head < prev && prev - head <= max_jump then
            is_safe tail head Desc
          else false)

let rec count_safes acc =
  try
    let line = input_line stdin in
    let nums = line |> String.split_on_char ' ' |> List.map int_of_string in

    let steal_first list =
      match list with [] -> None | head :: tail -> Some (head, tail)
    in

    let rec brute_force n =
      if n = -1 then false
      else
        let rec remove_at r = function
          | [] -> []
          | _ :: tail when r = 0 -> tail
          | head :: tail -> head :: remove_at (r - 1) tail
        in
        let cut_nums = remove_at n nums in
        match steal_first cut_nums with
        | None -> false
        | Some (head, tail) ->
            let safe = is_safe tail head Unknown in
            if safe then true else brute_force (n - 1)
    in

    if brute_force (List.length nums) then count_safes acc + 1
    else count_safes acc
  with End_of_file -> acc

let () =
  let total_safe = count_safes 0 in
  Printf.printf "%d\n" total_safe
