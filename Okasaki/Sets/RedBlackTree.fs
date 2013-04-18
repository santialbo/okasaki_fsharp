module Okasaki.Sets.RedBlackTree

type Color = R | B

type RedBlackTree<'a> =
    | E
    | T of Color * RedBlackTree<'a> * 'a * RedBlackTree<'a>

let rec isMember x = function
    | E -> false
    | T(_, a, y, b) ->
        if x < y then   isMember x a
        elif x > y then isMember x b
        else true
        
let balance = function
    | T(B, T(R, T(R, a, x, b), y, c), z, d) 
    | T(B, T(R, a, x, T(R, b, y, c)), z, d)
    | T(B, a, x, T(R, T(R, b, y, c), z, d))
    | T(B, a, x, T(R, b, y, T(R, c, z, d)))  -> T(R, T(B, a, x, b), y, T(B, c, z, d))  
    | t -> t
    
let black = function
    | E -> E
    | T(_, a, x, b) -> T(B, a, x, b)

let insert x t =
    let rec ins = function
        | E -> T(R, E, x, E)
        | T(c, a, y, b)  as t ->
            if x < y then   balance (T(c, (ins a), y, b))
            elif x > y then balance (T(c, a, y, (ins b)))
            else t
    in black (ins t)
