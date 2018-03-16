module HanoiSolver where


type DiskNum  = Integer
type Peg      = String
type Move     = (Peg, Peg)
type DisksState = ([Integer], [Integer], [Integer])

hanoi :: DiskNum -> Peg -> Peg -> Peg -> [Move]
hanoi n from to aux
    | n == 1
    = [(from, to)]
    | n > 1 
    = hanoi (n - 1) from aux to ++ [(from, to)] ++ hanoi (n - 1) aux to from
    | otherwise
    = []

hanoi4 n from to aux1 aux2
    | n > 0
    = hanoi4(n-2) from aux1 aux2 to 
        ++ [(from, aux2), (from, to), (aux2, to)]
    ++ hanoi4 (n-2) aux1 to from aux2
    | otherwise
    = []