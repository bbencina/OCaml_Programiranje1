(* ===== Exercise 4: Search Tree  ===== *)

type 'a tree =
  | Empty
  | Node of 'a tree * 'a * 'a tree

(* Standard test example.
          5
         / \
        2   7
       /   / \
      0   6   11
   *)

let leaf x = Node(Empty, x, Empty) (* Function to shorten tree creation. *)

let test_tree = Node( Node(leaf 0, 2, Empty), 5, Node(leaf 6, 7, leaf 11))

(* The function "mirror t" returns a mirrored tree. On the test_tree example:
       5
      / \
     7   2
    / \   \
   11  6   0
   ----------
   # mirror test_tree ;;
   - : int tree =
   Node (Node (Node (Empty, 11, Empty), 7, Node (Empty, 6, Empty)), 5,
   Node (Empty, 2, Node (Empty, 0, Empty)))
   ---------- *)

let rec mirror t =
	match t with
	| Empty -> Empty
	| Node(tree1, x, tree2) -> Node(mirror tree2, x, mirror tree1)

(* The function "height t" returns the height (or depth) of the tree and
   the function "size t" returns the number of all tree nodes.
   ----------
   # height test_tree;;
   - : int = 3
   # size test_tree;;
   - : int = 6
   ---------- *)

let rec height t =
	match t with
	| Empty -> 0
	| Node(tree1, x, tree2) -> 1 + (max (height tree1) (height tree2))

let rec size t =
	match t with
	| Empty -> 0
	| Node(tree1, x, tree2) -> 1 + size tree1 + size tree2

(* The function "follow directions t" [direction list -> 'a tree -> 'a option]
   takes as input a list of directions for traversing the tree. Because the
   directions might not lead to a node we use the option type.
   ----------
   # follow [Right;Left] test_tree;;
   - : int option = Some 6
   # follow [Right;Left;Right;Right] test_tree;;
   - : int option = None
   ---------- *)

type direction = Left | Right

let rec follow directions t =
	match directions, t with
	| (_, Empty) -> None
	| ([], Node(_, x, _)) -> Some x
	| (hd::tl, Node(tree1, x, tree2)) -> if hd = Right
		then follow tl tree2
		else follow tl tree1

(* The function "prune directions t" [direction list -> 'a tree -> 'a tree option]
   finds the node determined by the directions and deletes the subtree rooted
   in the selected node.
   Warning: using Some Node(l, x, r) causes an error in OCaml because it is
   read as (Some Node)(l, x, r). Use appropriate paranthesis.
   ----------
   # prune [Right] test_tree;;
   - : int tree option =
   Some (Node (Node (Node (Empty, 0, Empty), 2, Empty), 5, Empty))
   ---------- *)

let rec prune directions t =
	match directions, t with
	| (_, Empty) -> None
	| ([], Node(_, _, _)) -> Some Empty
	| ([dir], Node(t1, x, t2)) -> if dir = Right
		then Some (Node(t1, x, Empty))
		else Some (Node(Empty, x, t2))
	| (hd::tl, Node(tree1, x, tree2)) -> if hd = Right
		then prune tl tree2
		else prune tl tree1

(* The function "map_tree f t"  [('a -> 'b) -> 'a tree -> 'b tree] maps the
   nodes of the tree t with the function f.
   ----------
   # map_tree ((<)3) test_tree;;
   - : bool tree =
   Node (Node (Node (Empty, false, Empty), false, Empty), true,
   Node (Node (Empty, true, Empty), true, Node (Empty, true, Empty)))
   ---------- *)

let rec map_tree f t =
	match t with
	| Empty -> Empty
	| Node(tree1, x, tree2) -> Node(map_tree f tree1, f x, map_tree f tree2)

(* The function "list_of_tree t" ['a tree -> 'a list] maps the data of the tree
   into a list. If the tree is a binary search tree the returned list should be
   ordered.
   ----------
   # list_of_tree test_tree;;
   - : int list = [0; 2; 5; 6; 7; 11]
   ---------- *)

let rec list_of_tree t =
	match t with
	| Empty -> []
	| Node(tree1, x, tree2) -> (list_of_tree tree1)@[x]@(list_of_tree tree2)

(* The function "is_bst t" ['a tree -> bool] checks wheter a tree is a
   binary search tree (BST). Assume that a tree has no repetitions (a tree
   Node(leaf 1, 1, leaf 2) is not allowed).
   An empty tree is a BST.
   ----------
   # is_bst test_tree;;
   - : bool = true
   # test_tree |> mirror |> is_bst;;
   - : bool = false
   ---------- *)

let rec is_bst t =
	match t with
	| Empty -> true
	| Node(t1, x, t2) ->
		(match t1, t2 with
		| (Node(_, x1, _), Node(_, x2, _)) -> if x1 < x && x < x2
			then is_bst t1 && is_bst t2
			else false
		| (Empty, Node(_, x2, _)) -> if x < x2
			then is_bst t2
			else false
		| (Node(_, x1, _), Empty) -> if x1 < x
			then is_bst t1
			else false
		| (Empty, Empty) -> true
		)

(*------------------------------------------------------------------------------
   In the remaining exercises the variable name bst assumes a BST input.
  ----------------------------------------------------------------------------*)

(* The function "insert x bst" ['a -> 'a tree -> 'a tree] inserts the element x
   into the bst. The function "member x bst" ['a -> 'a tree -> bool] checks
   wheter an element is present in the bst.
   ----------
   # insert 2 (leaf 4);;
   - : int tree = Node (Node (Empty, 2, Empty), 4, Empty)
   # member 3 test_tree;;
   - : bool = false
   ---------- *)

let rec insert x bst =
	match bst with
	| Empty -> leaf x
	| Node(t1, root, t2) -> if x = root
		then bst
		else if x < root
		then Node(insert x t1, root, t2)
		else Node(t1, root, insert x t2)

let rec member x bst =
	match bst with
	| Empty -> false
	| Node(t1, root, t2) -> if x = root
		then true
		else (member x t1) || (member x t2)

(* Write the function "member2", where you do not assume a BST structure.
   Think about the differences of time complexity for "member" and "member2"
   if you assume that the tree has n nodes and a depth of log(n). *)

let rec member2 x t = ()

(* The function "bst_of_list l" ['a list -> 'a tree] forms a bst from a list.
   Hint: in lectures the professor first defined the function "insert".
   ----------
   # [11;6;7;0;2;5] |> bst_of_list |> is_bst;;
   - : bool = true
   ---------- *)

let bst_of_list l =
	let rec listing lst acc =
		match (lst, acc) with
		| ([], _) -> acc
		| (hd::tl, Empty) -> listing tl (leaf hd)
		| (hd::tl, Node(t1, x, t2)) -> listing tl (insert hd acc)
	in listing l Empty

(* Create a function "tree_sort l" ['a list -> 'a list] that sorts the list l
   by combining previously defining functions.
   ----------
   # tree_sort ["a";"c";"f";"b";"e";"d"];;
   - : string list = ["a"; "b"; "c"; "d"; "e"; "f"]
   ---------- *)

let tree_sort l = l |> bst_of_list |> list_of_tree

(* The function "succ bst" ['a tree -> 'a option] returns the succesor of the
   tree root, if it exists. For instance, for bst = Node(l, x, r) it returns
   the smallest element larger than x.
   The function "pred bst" ['a tree -> 'a option] symetrically returns the
   largest elements smaller tha the root, if it exists.
   ----------
   # succ test_tree;;
   - : int option = Some 6
   # pred (Node(Empty, 5, leaf 7));;
   - : int option = None
   ---------- *)

let succ bst =
  match bst with
  | Empty -> None
  | Node (_, root, Empty) -> None
  | Node (tl, root, Node (t1, rr, t2)) ->
    let rec minning m t =
      match t with
      | Empty -> Some m
      | Node (Empty, x, _) -> if x < m then Some x else Some m
      | Node (t1, x, _) -> minning m t1
    in minning rr t1

let pred bst =
  match bst with
  | Empty -> None
  | Node (Empty, root, _) -> None
  | Node (Node (t1, rr, t2), root, tr) ->
    let rec minning m t =
      match t with
      | Empty -> Some m
      | Node (_, x, Empty) -> if x > m then Some x else Some m
      | Node (_, x, t1) -> minning m t1
    in minning rr t1

(* In lectures you mentioned multiple different algorithms for deletion.
   One uses "succ" and the other "pred".
   Write a function "delete x bst" ['a tree -> 'a tree], that deletes the
   elements x, should it exist in the tree. For practice, you can implement
   both different algorithms.
   ----------
   [For delete defined with "succ".]
   # delete 7 test_tree;;
   - : int tree =
   Node (Node (Node (Empty, 0, Empty), 2, Empty), 5,
   Node (Node (Empty, 6, Empty), 11, Empty))
   ---------- *)

let rec delete x bst = ()

(* An additional option is to change the type of the tree. Define a new tree
   type that additionally contains an information about its state, that can be
   either Exists or Ghost if it is not present in the tree anymore. *)

type state = Exists | Ghost

type 'a phantom_tree = unit

(* The function "phantomize t" ['a tree -> 'a phantom_tree], that maps a regular
   tree into a phantom tree.
   Then write the function "kill x pt" ['a -> 'a phantom_tree -> 'a phantom_tree]
   that removes an element from the tree by changing it's state to Ghost.
   ----------
   # phantomize test_tree;;
   - : int phantom_tree =
   P_Node (P_Node (P_Node (P_Empty, 0, P_Empty, Exists), 2, P_Empty, Exists), 5,
   P_Node (P_Node (P_Empty, 6, P_Empty, Exists), 7,
   P_Node (P_Empty, 11, P_Empty, Exists), Exists),
   Exists)

   # bst_of_list [3;4;2] |> phantomize |> kill 3 |> kill 6;;
   - : int phantom_tree =
   P_Node (P_Empty, 2,
   P_Node (P_Node (P_Empty, 3, P_Empty, Ghost), 4, P_Empty, Exists), Exists)
   ---------- *)

let rec phantomize t = ()

let rec kill x pt = ()

(* The function "unphantomize pt" ['a phantom_tree -> 'a tree] that maps a
   phantom tree to a regular tree, that only contains existing states.
   The order of elements in the tree is not important.
   Hint: you may use a transition to another data structure in between.
   ----------
   # test_tree |> phantomize |> kill 7 |> kill 0 |> kill 5 |> unphantomize;;
   - : int tree = Node (Node (Node (Empty, 2, Empty), 6, Empty), 11, Empty)
   ---------- *)

let unphantomize pt = ()

(*========== Ideas for additional exercises ==========*)
(*
1.) Change the functions "insert" and "member" to work with phantom trees.
2.) Create a more general decision tree, that additionally contains a
    decision function in every node that decides in which subtree an element
    belongs. [Such trees are used in machine learning, where comparing vectors
    only uses a chosen component of the vector.]
3.) By using exercise 2 you can define dictionaries based on trees, where a pair
    (data, key) is saved according to key value. Define a new type of nodes
    that keeps the value of data and an additional key and define some of the
    function "member", "insert", "dict_of_list", ... so that they coincide with
    the expected behaviour on dictionaries.
*)
