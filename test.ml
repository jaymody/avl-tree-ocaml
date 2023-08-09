open Lib.AVL

(* basic tests *)
let () =
  let tree = empty |> add 5 "banana" |> add 1 "apple" |> add 10 "dog" |> add 7 "cat" in
  assert (find 1 tree = Some "apple");
  assert (find 5 tree = Some "banana");
  assert (find 7 tree = Some "cat");
  assert (find 10 tree = Some "dog");
  assert (find 2 tree = None);
  let res, tree = remove 5 tree in
  assert (res = Some "banana");
  assert (find 5 tree = None);
  let res, tree = remove 5 tree in
  assert (res = None);
  assert (find 10 tree = Some "dog");
  let tree = add 1 "apricot" tree in
  assert (find 1 tree = Some "apricot")
;;

(* some more basic tests *)
let () =
  let tree =
    empty
    |> add 5 (-5)
    |> add 2 (-2)
    |> add 3 100000
    |> add 1 (-1)
    |> add 0 0
    |> add 10 (-10)
    |> add 11 (-11)
    |> add 12 (-12)
    |> add 8 (-8)
    |> add 7 (-7)
    |> add 3 (-3)
    |> add 4 (-4)
    |> add 6 (-6)
    |> add 9 (-9)
  in
  assert (to_list tree |> List.map fst = [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12 ]);
  assert (find 12 tree = Some (-12));
  let res, tree = remove 12 tree in
  assert (res = Some (-12));
  assert (find 12 tree = None);
  let res, tree = remove 12 tree in
  assert (res = None);
  let res, tree = remove 5 tree in
  assert (res = Some (-5));
  let res, tree = remove 8 tree in
  assert (res = Some (-8));
  let res, tree = remove 0 tree in
  assert (res = Some 0);
  let res, tree = remove 3 tree in
  assert (res = Some (-3));
  assert (to_list tree |> List.map fst = [ 1; 2; 4; 6; 7; 9; 10; 11 ])
;;

(* tests to make sure the tree works at scale *)
let () =
  let size = 10000 in
  let list = List.init size (fun x -> x) in
  let tree = List.fold_left (fun tree num -> add num (-num) tree) empty list in
  assert (to_list tree |> List.map fst = list);
  let keep_list = List.filter (fun x -> x mod 2 = 0) list in
  let remove_list = List.filter (fun x -> x mod 2 = 1) list in
  let tree = List.fold_left (fun tree num -> snd (remove num tree)) tree remove_list in
  assert (to_list tree |> List.map fst = keep_list)
;;

(* calculates the height of the tree *)
let rec calc_height = function
  | Empty -> 0
  | Node n -> 1 + max (calc_height n.l) (calc_height n.r)
;;

let rec is_height_balanced = function
  | Empty -> true
  | Node n ->
    is_height_balanced n.l
    && is_height_balanced n.r
    && abs (calc_height n.l - calc_height n.r) < 2
    && n.h = 1 + max (calc_height n.l) (calc_height n.r)
;;

let () =
  let size = 10000 in
  let list = List.init size (fun x -> x) in
  let tree = List.fold_left (fun tree num -> add num (-num) tree) empty list in
  Printf.printf "Height after %d insertions: %d\n" size (calc_height tree);
  assert (is_height_balanced tree);
  let tree =
    List.fold_left
      (fun tree num -> snd (remove num tree))
      tree
      (List.init (size / 2) (fun x -> x))
  in
  Printf.printf "Height after %d deletions: %d\n" (size / 2) (calc_height tree);
  assert (is_height_balanced tree)
;;

Printf.printf "Tests passed\n"
