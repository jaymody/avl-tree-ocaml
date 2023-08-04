type comparison =
  | Eq
  | Gt
  | Lt

let cmp a b = if a - b < 0 then Lt else if a - b > 0 then Gt else Eq

module AVL = struct
  type 'a tree =
    | Empty
    | Node of 'a tree * int * 'a * 'a tree * int

  let empty = Empty

  let rec height = function
    | Empty -> 0
    | Node (_, _, _, _, h) -> h
  ;;

  let create l k v r =
    let make (l, k, v, r) = Node (l, k, v, r, 1 + max (height l) (height r)) in
    let rotate_left (k, v, l) (rl, rk, rv, rr) = make (l, k, v, rl), rk, rv, rr in
    let rotate_right (k, v, r) (ll, lk, lv, lr) = ll, lk, lv, make (lr, k, v, r) in
    make
      (match l, r with
       | _, Node (rl, rk, rv, rr, _) when height rr > height l ->
         rotate_left (k, v, l) (rl, rk, rv, rr)
       | _, Node ((Node (rll, rlk, rlv, rlr, _) as rl), rk, rv, rr, _)
         when height rl > height l ->
         rotate_right (rk, rv, rr) (rll, rlk, rlv, rlr) |> rotate_left (k, v, l)
       | Node (ll, lk, lv, (Node (lrl, lrk, lrv, lrr, _) as lr), _), _
         when height lr > height r ->
         rotate_left (lk, lv, ll) (lrl, lrk, lrv, lrr) |> rotate_right (k, v, r)
       | Node (ll, lk, lv, lr, _), _ when height ll > height r ->
         rotate_right (k, v, r) (ll, lk, lv, lr)
       | _ -> l, k, v, r)
  ;;

  let add k' v' tree =
    let rec aux = function
      | Empty -> create Empty k' v' Empty
      | Node (l, k, v, r, _) ->
        (match cmp k' k with
         | Eq -> create l k' v' r
         | Lt -> create (aux l) k v r
         | Gt -> create l k v (aux r))
    in
    aux tree
  ;;

  let find k' tree =
    let rec aux = function
      | Empty -> None
      | Node (l, k, v, r, _) ->
        (match cmp k' k with
         | Eq -> Some v
         | Lt -> aux l
         | Gt -> aux r)
    in
    aux tree
  ;;

  let remove k' tree =
    let rec pop_successor l k v r =
      match l with
      | Empty -> (k, v), r
      | Node (ll, lk, lv, lr, _) ->
        let succesor, l = pop_successor ll lk lv lr in
        succesor, create l k v r
    in
    let rec aux = function
      | Empty -> None, Empty
      | Node (l, k, v, r, _) ->
        (match cmp k' k with
         | Eq ->
           ( Some v
           , (match r with
              | Empty -> l
              | Node (rl, rk, rv, rr, _) ->
                let (k, v), r = pop_successor rl rk rv rr in
                create l k v r) )
         | Lt ->
           let e, l = aux l in
           e, create l k v r
         | Gt ->
           let e, r = aux r in
           e, create l k v r)
    in
    aux tree
  ;;

  let to_list tree =
    let rec aux acc = function
      | Empty -> acc
      | Node (l, k, v, r, _) -> aux ((k, v) :: aux acc r) l
    in
    aux [] tree
  ;;
end
