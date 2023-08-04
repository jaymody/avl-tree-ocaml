type comparison =
  | Eq
  | Gt
  | Lt

let cmp a b = if a - b < 0 then Lt else if a - b > 0 then Gt else Eq

module BST = struct
  type 'a tree =
    | Empty
    | Node of 'a tree * int * 'a * 'a tree

  let empty = Empty
  let create l k v r = Node (l, k, v, r)

  let add k' v' tree =
    let rec aux = function
      | Empty -> create Empty k' v' Empty
      | Node (l, k, v, r) ->
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
      | Node (l, k, v, r) ->
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
      | Node (ll, lk, lv, lr) ->
        let succesor, l = pop_successor ll lk lv lr in
        succesor, create l k v r
    in
    let rec aux = function
      | Empty -> None, Empty
      | Node (l, k, v, r) ->
        (match cmp k' k with
         | Eq ->
           ( Some v
           , (match r with
              | Empty -> l
              | Node (rl, rk, rv, rr) ->
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
      | Node (l, k, v, r) -> aux ((k, v) :: aux acc r) l
    in
    aux [] tree
  ;;
end
