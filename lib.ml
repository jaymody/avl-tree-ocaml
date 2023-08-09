type comparison =
  | Eq
  | Gt
  | Lt

let cmp a b = if a - b < 0 then Lt else if a - b > 0 then Gt else Eq

module AVL = struct
  type 'a node =
    { l : 'a tree
    ; k : int
    ; v : 'a
    ; r : 'a tree
    ; h : int
    }

  and 'a tree =
    | Empty
    | Node of 'a node

  let empty = Empty

  let rec height = function
    | Empty -> 0
    | Node { h } -> h
  ;;

  let create node =
    let make n = { n with h = 1 + max (height n.l) (height n.r) } in
    let rotl n r = make { r with l = Node (make { n with r = r.l }) } in
    let rotr n l = make { l with r = Node (make { n with l = l.r }) } in
    Node
      (make
         (match node.l, node.r with
          | l, Node ({ r = Node rr } as r) when rr.h > height l -> rotl node r
          | l, Node ({ l = Node rl } as r) when rl.h > height l -> rotl node (rotr r rl)
          | Node ({ r = Node lr } as l), r when lr.h > height r -> rotr node (rotl l lr)
          | Node ({ l = Node ll } as l), r when ll.h > height r -> rotr node l
          | _ -> node))
  ;;

  let add k v tree =
    let rec aux = function
      | Empty -> Node { l = Empty; k; v; r = Empty; h = 1 }
      | Node n ->
        (match cmp k n.k with
         | Eq -> create { n with v }
         | Lt -> create { n with l = aux n.l }
         | Gt -> create { n with r = aux n.r })
    in
    aux tree
  ;;

  let find k tree =
    let rec aux = function
      | Empty -> None
      | Node n ->
        (match cmp k n.k with
         | Eq -> Some n.v
         | Lt -> aux n.l
         | Gt -> aux n.r)
    in
    aux tree
  ;;

  let remove k tree =
    let rec pop_successor n =
      match n.l with
      | Empty -> (n.k, n.v), n.r
      | Node l ->
        let succesor, l = pop_successor l in
        succesor, create { n with l }
    in
    let rec aux = function
      | Empty -> None, Empty
      | Node n ->
        (match cmp k n.k with
         | Eq ->
           ( Some n.v
           , (match n.r with
              | Empty -> n.l
              | Node r ->
                let (k, v), r = pop_successor r in
                create { n with k; v; r }) )
         | Lt ->
           let e, l = aux n.l in
           e, create { n with l }
         | Gt ->
           let e, r = aux n.r in
           e, create { n with r })
    in
    aux tree
  ;;

  let to_list tree =
    let rec aux acc = function
      | Empty -> acc
      | Node { k; v; r; l } -> aux ((k, v) :: aux acc r) l
    in
    aux [] tree
  ;;
end
