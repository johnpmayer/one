
module DiamondSquare where

type Terrain = [TerrainRow]
type TerrainRow = [Float]

avg2 : Float -> Float -> Float
avg2 a b = (a + b) / 2

noise : Float
noise = 7

avg4 : Float -> Float -> Float -> Float -> Float
avg4 a b c d = (a + b + c + d + noise) / 4

truncateRow : TerrainRow -> TerrainRow
truncateRow r = case r of
    [a] -> [a]
    (a::b::rs) -> a :: avg2 a b :: truncateRow (b :: rs)

truncateRowPair : TerrainRow -> TerrainRow -> (TerrainRow, TerrainRow)
truncateRowPair r1 r2 = case (r1,r2) of
    ([a],[b]) -> ([a],[avg2 a b])
    ((a::b::r1s),(c::d::r2s)) -> 
        let (x,y) = truncateRowPair (b::r1s) (d::r2s)
        in  ( a         :: avg2 a b         :: x
            , avg2 a c  :: avg4 a b c d     :: y
            )
    -- _ -> error should never happen

truncate : [TerrainRow] -> [TerrainRow]
truncate t = case t of
    [r] -> [truncateRow r]
    (r1::r2::rs) -> 
        let (x, y) = truncateRowPair r1 r2
        in x :: y :: truncate (r2 :: rs)

seed : (Float,Float,Float,Float) -> Int -> Terrain
seed (a,b,c,d) n = 
    let seedHelp n' = case n' of
            0 -> [[a,b],[c,d]]
            n' -> truncate (seedHelp (n' - 1))
    in seedHelp n

