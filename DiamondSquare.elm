
module DiamondSquare where

import MJS (V3, v3)
import Graphics.WebGL (Triangle, mapTriangle)

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

rowPairTriangles : (Int,Int) -> TerrainRow -> TerrainRow -> [Triangle {pos:V3}]
rowPairTriangles (n',m') r1 r2 = case (r1,r2) of
    ((a::b::r1s),(c::d::r2s)) ->
        let n = toFloat n'
            m = toFloat m'
            t1 = (v3 n m a, v3 n (m+1) b, v3 (n+1) m c)
            t2 = (v3 n (m+1) b, v3 (n+1) m c, v3 (n+1) (m+1) d)
            p = mapTriangle (\v -> { pos = v })
        in p t1 :: p t2 :: rowPairTriangles (n', m' + 1) r1s r2s
    _ -> []

terrainTriangles : Terrain -> [Triangle {pos:V3}]
terrainTriangles t = 
    let helper t' n = case t' of
        (a::b::rs) -> rowPairTriangles (n,0) a b :: helper (b :: rs) (n+1)
        _ -> []
    in concat <| helper t 0
