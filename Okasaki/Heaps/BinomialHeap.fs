module Okasaki.Heaps.BinomialHeap

type Tree<'a> = Node of int * 'a * list<Tree<'a>>

type BinomialHeap<'a> = list<Tree<'a>>

let empty: BinomialHeap<'a> = []

let rank = function Node(r, x, c) -> r

let root = function Node(r, x, c) -> x

let link t1 t2 = 
    match t1, t2 with
    | Node(r, x1, c1) , Node(_, x2, c2) ->
        if x1 <= x2 then
            Node(r + 1, x1, t2::c1)
        else
            Node(r + 1, x2, t1::c2)
            
let rec insTree t (bh: BinomialHeap<'a>): BinomialHeap<'a> =
    match bh with
    | [] -> [t]
    | t'::bh' ->
        if rank t < rank t' then
            t::bh
        else
            insTree (link t t') bh'
            
let insert x bh = (insTree (Node(0, x, [])) bh)

let rec merge (bh1: BinomialHeap<'a>) (bh2: BinomialHeap<'a>) =
    match bh1, bh2 with
    | _, [] -> bh1
    | [], _ -> bh2
    | (t1 :: bh1'), (t2 :: bh2') ->
        if rank t1 < rank t2 then
            t1 :: (merge bh1' bh2)
        elif rank t1 > rank t2 then
            t2 :: (merge bh1 bh2')
        else
            insTree (link t1 t2) (merge bh1' bh2')
            
let rec removeMinTree (t: BinomialHeap<'a>): (Tree<'a> * BinomialHeap<'a>) =
    match t with
    | [] -> failwith "Empty BinomialHeap"
    | [t] -> (t, [])
    | t::bh ->
        let (t', bh') = removeMinTree bh
        in if root t <= root t' then (t, bh) else (t', t::bh')

let findMin (bh: BinomialHeap<'a>) =
    match removeMinTree bh with (t, _) -> root t
    
let deleteMin (bh: BinomialHeap<'a>) =
    match removeMinTree bh with (Node(_, x, bh1), bh2) -> merge (List.rev bh1) bh2

let fromList xs = xs |> List.fold (fun bh x -> insert x bh) empty

let toList (bh: BinomialHeap<'a>) =
    let rec toList_ acc = function
        | [] -> List.rev acc
        | bh -> toList_ ((findMin bh) :: acc) (deleteMin bh)
    in toList_ [] bh

(* Exercise 3.5 Define "findMin" directly rather than via a call to "removeMinTree" *)
let findMin2 (bh: BinomialHeap<'a>) =
    bh |> List.minBy root |> root

(* Exercise 3.6 Most of the rank annotations in this representation of binomial heaps are redundant
   because we know that the children of a node of rank r have ranks r-1, ..., 0. Thus, we can
   remove the rank annotations from each node and instead pair each tree ar the top-level with ibh
   rank, i.e.
   datatype Tree = Node of Elem x Tree list
   type Heap = (int x Tree) list
   Reimplement binomial heaps with this new representation *)

type Tree_<'a> = Node_ of 'a * list<Tree_<'a>>

type BinomialHeap_<'a> = list<int * Tree_<'a>>

let empty_: BinomialHeap_<'a> = [] 

let root_ = function Node_(x, _) -> x

let rec rank_ = function
    | Node_(_, []) -> 0
    | Node_(_, t :: _) -> 1 + rank_ t

let link_ t1 t2 = 
    match t1, t2 with
    | Node_(x1, c1) , Node_(x2, c2) ->
        if x1 <= x2 then
            Node_(x1, t2::c1)
        else
            Node_(x2, t1::c2)

let insTree_ t (bh: BinomialHeap_<'a>): BinomialHeap_<'a> =
    let rec insTree__ t r (bh: BinomialHeap_<'a>) =
        match bh with
        | [] -> [(r, t)]
        | (r', t') :: bh' ->
            if r < r' then
                (r, t) :: bh
            else
                insTree__ (link_ t t') (r + 1) bh'
    in insTree__ t (rank_ t) bh

let insert_ x (bh: BinomialHeap_<'a>) = insTree_ (Node_(x, [])) bh

let rec merge_ (bh1: BinomialHeap_<'a>) (bh2: BinomialHeap_<'a>) =
    match bh1, bh2 with
    | _, [] -> bh1
    | [], _ -> bh2
    | (r1, t1) :: bh1', (r2, t2) :: bh2' ->
        if r1 < r2 then
            (r1, t1) :: bh2
        elif r1 > r2 then
            (r2, t2) :: bh1
        else 
            insTree_ (link_ t1 t2) (merge_ bh1' bh2')

let rec removeMinTree_ (bh: BinomialHeap_<'a>): (Tree_<'a> * BinomialHeap_<'a>) =
    match bh with
    | [] -> failwith "Empty BinomialHeap"
    | [(_, t)] -> (t, [])
    | (r, t)::bh ->
        let (t', bh') = removeMinTree_ bh
        in if root_ t <= root_ t' then (t, bh) else (t', (r, t)::bh')

let findMin_ (bh: BinomialHeap_<'a>) =
    match removeMinTree_ bh with (t, _) -> root_ t
    
let deleteMin_ (bh: BinomialHeap_<'a>) =
    match removeMinTree_ bh with
    (Node_(x, bh1), bh2) ->
        let bh1' = bh1 |> List.rev |> List.map (fun t -> (rank_ t, t))
        in merge_ (List.rev bh1') bh2   
   
let fromList_ xs = xs |> List.fold (fun bh x -> insert_ x bh) empty_

let toList_ (bh: BinomialHeap_<'a>) =
    let rec toList__ acc = function
        | [] -> List.rev acc
        | bh -> toList__ ((findMin_ bh) :: acc) (deleteMin_ bh)
    in toList__ [] bh
