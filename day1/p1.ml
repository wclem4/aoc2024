let rec build_lists l1 l2 =
  try
    let line = input_line stdin in
    let num1, num2 = Scanf.sscanf line "%d %d" (fun x y -> (x, y)) in
    let l1 = num1 :: l1 and l2 = num2 :: l2 in
    build_lists l1 l2
  with End_of_file -> (l1, l2)

let l1, l2 = build_lists [] []

let sort_acc_print () =
  let sl1 = List.sort compare l1 and sl2 = List.sort compare l2 in
  let total = List.fold_left2 (fun acc i j -> acc + abs (i - j)) 0 sl1 sl2 in
  Printf.printf "%d\n" total

let () = sort_acc_print ()
