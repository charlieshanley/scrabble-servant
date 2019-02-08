
module Scrabble.Tile where

data Tile = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z
    deriving (Eq, Show, Read)

points :: Tile -> Int
points t = case t of
    Q -> 10
    Z -> 10

    J -> 8
    X -> 8

    K -> 5

    F -> 4
    H -> 4
    V -> 4
    W -> 4
    Y -> 4

    B -> 3
    C -> 3
    M -> 3
    P -> 3

    D -> 2
    G -> 2

    _ -> 1

