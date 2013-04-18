module Okasaki.Sets.UnbalancedSet
    
type UnbalancedSet<'a> =
    | E
    | T of UnbalancedSet<'a> * 'a * UnbalancedSet<'a>

let isEmpty = function
    | E -> true
    | _ -> false

let rec insert x = function
    | E -> T(E, x, E)
    | T(a, y, b) as s -> 
        if x < y then T(insert x a, y, b)
        elif x > y then T(a, y, insert x b)
        else s
    
let rec isMember x = function
    | E -> false
    | T(a, y, b) ->
        if x < y then isMember x a
        elif x > y then isMember x b
        else true

(* This is an implementation of the "delete" function which deletes a given element. This is not
   in the book *)

let rec min = function
    | E -> failwith "Minimum of an empty set"
    | T(E, x, _) -> x
    | T(a, _, _) -> min a

let rec deleteMin = function
    | E -> E
    | T(E, x, b) -> b
    | T(a, x, b) -> T(deleteMin a, x, b)

let join a b =
    match a, b with
    | E, _ -> b
    | _, E -> a
    | a, b -> T(a, min b, deleteMin b)

let rec delete x = function
    | E -> E
    | T(a, y, b) when x < y -> T(delete x a, y, b)
    | T(a, y, b) when x > y -> T(a, y, delete x b)
    | T(E, y, b) -> b
    | T(a, y, E) -> a
    | T(a, y, b) as t -> join a b

(* Helper functions *)
let top = function
    | E -> failwith "Top of empty set"
    | T(_, y, _) -> y
    
let rec flatten = function
    | E -> []
    | T(a, y, b) -> (flatten a) @ y::(flatten b)
    
let fromList xs = xs |> List.fold (fun x s -> insert s x) E

(* Exercise 2.2  In the worst case, "member" performs approximately 2d comparisons, where d is the
   depth of the tree. Rewrite "member" to take no more than d + 1 comparisons by keeping track of a
   candidate element that might be equal to the query element (say, the last element for which <
   returned false or <= returned true) and checking for equality only when you hit the bottom of
   the tree. *)
let isMember2 x s =
    let rec isMember2_ x cand = function
        | E -> cand = x
        | T(a, y, b) ->
            if x <= y then isMember2_ x y a
            else isMember2_ x cand b
    match s with
    | E -> false
    | T(_, y, _) -> isMember2_ x y s

(* Exercise 2.3 Inserting an existing element into a binary search tree copies the entire path
   search even though the copied nodes are indistinguishable from the originals. Rewrite "insert"
   using exeptions to avoid this copying. Establish only one handler per insertion rather than one
   habdler per iteration. *)
exception ExistingElement   

let insert2 x s =
    let rec insert2_ x = function
        | E -> T(E, x, E)
        | T(a, y, b) as s -> 
            if x < y then T(insert2_ x a, y, b)
            elif x > y then T(a, y, insert2_ x b)
            else raise ExistingElement
    try
        insert2_ x s
    with
        | ExistingElement -> s

(* Exercise 2.4 Combine the ideas of the previous two exercise to obtain a version of insert that
   performs no unnecessary copying and uses no more than d + 1 comparisons. *)
let insert3 x s =
    let rec insert3_ x cand = function
        | E ->
            if x = cand then raise ExistingElement
            else T(E, x, E)
        | T(a, y, b) as s -> 
            if x <= y then T(insert3_ x y a, y, b)
            else T(a, y, insert3_ x cand b)
    try
        match s with
        | E -> T(E, x, E)
        | T(_, y, _) -> insert3_ x y s
    with
        | ExistingElement -> s
        
(* Exercise 2.5 Sharing can also be useful within a single object, not just between objects. For
   example, if the two subtrrees of a given node are identical, then they can be represented by
   the same tree. 
   
   (a) Using this idea, write a function "complete" of type Elem x Int -> Tree where
   "complete(x, d)" creates a complete binary tree of depth d with x stored in every node. (Of
   course, this function makes no sense for the set abstraction, but it can be useful as an
   auxiliary function for other abstraction, such as bags.) This function should run in O(d)
   time. *)
let complete x d =
    let rec complete_ x d aux =
        match d with
        | 0 -> aux
        | _ -> complete_ x (d - 1) (T(aux, x, aux))
    in complete_ x d E

(* (b) Extend this function to create balanced trees of arbitrary size. These trees will not 
   always be complete binary trees, but should be as balanced as possible: for any given node, the
   two subtrees should differ in size by at most one. This function should run in O(log n) time.
   (Hint: use a helper function "create2" that, given a size m, creates a pair of trees, one of
   size m and one of size m + 1. *)
let rec create x n =
    match n with
    | 0 -> E
    | 1 -> T(E, x, E)
    | n when (n % 2 = 1) -> let aux = create x (n / 2) in T(aux, x, aux)
    | n -> T(create x (n / 2), x, create x ((n / 2) - 1))
    
(* Exercise 2.6 Adapt the UnbalancedSet functor to support finite maps rather than sets. Figure
   2.10 gives a minimal signature for finite maps. (Note that the NotFound exception is not
   predefined in Standard ML - you will have to define it yourself. Although this exception could
   be made part of the FiniteMap signature, with every implementation defining its own NotFound
   exception, it is convenient for all finite maps to use the same exception. *)

type finiteMap<'a, 'b> = UnbalancedSet<('a * 'b)>

exception NotFound

let emptyFiniteMap = finiteMap.E

let isEmptyFM = function
    | E -> true
    | _ -> false

let bind key a fm = insert3 (key, a) fm

let rec lookup key fm =
    let rec lookup_ key cand fm =
        match fm, cand with
        | E, (ck, a) -> if ck = key then a else raise NotFound
        | T(a, (yk, yo), b), _ ->
            if key <= yk then lookup_ key (yk, yo) a
            else lookup_ key cand b
    match fm with
    | E -> raise NotFound
    | T(_, y, _) -> lookup_ key y fm
