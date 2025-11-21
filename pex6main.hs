-- pex6.hs 
-- unKnot Haskell

-- name: Gia Sanchirico

{- DOCUMENTATION:

-}

unKnot :: [(Char, Char)] -> String
unKnot tripCode
   | null tripCode = "not a knot"
   | typeIKnot tripCode = unKnot (removeTypeI tripCode)
   | typeIKnot (wrap tripCode) = unKnot (removeTypeI (wrap tripCode))
   | typeIKnot (wrap (wrap tripCode)) = unKnot (removeTypeI (wrap (wrap tripCode)))
   | typeIIKnot tripCode = unKnot (removeTypeII tripCode)
   | typeIIKnot (wrap tripCode) = unKnot (wrap tripCode)
   | typeIIKnot (wrap (wrap tripCode)) = unKnot (removeTypeII (wrap (wrap tripCode)))
   | otherwise = "tangle - resulting trip code: " ++ (show tripCode)

typeIKnot :: [(Char, Char)] -> Bool
typeIKnot tripCode
   | null tripCode = False
   | length tripCode < 2 = False
   | fst(head tripCode) == fst(head(tail tripCode)) = True
   | otherwise = False


typeIIKnot :: [(Char, Char)] -> Bool
typeIIKnot tripCode
   | null tripCode = False
   | length tripCode < 4 = False
   | snd(head tripCode) == snd(head(tail tripCode)) && (partIISearch (fst(head tripCode)) (fst(head(tail tripCode))) (drop 2 tripCode)) = True
   | otherwise = False

partIISearch :: Char -> Char -> [(Char, Char)] -> Bool
partIISearch x y ptc
   | length ptc < 2 = False
   | x == fst(head ptc) && y == fst(head (tail ptc)) = True
   | y == fst(head ptc) && x == fst(head (tail ptc)) = True
   | otherwise = partIISearch x y (tail ptc)

removeTypeI :: [(Char, Char)] -> [(Char, Char)]
removeTypeI tripCode = tail(tail tripCode)


removeTypeII :: [(Char, Char)] -> [(Char, Char)]
removeTypeII tripCode = (removePartII (fst(head tripCode)) (fst(head(tail tripCode)))  (drop 2 tripCode))

removePartII :: Char -> Char -> [(Char, Char)] -> [(Char, Char)]
removePartII x y ptc
   | null ptc = []
   | x == fst(head ptc) = removePartII x y (tail ptc)
   | y == fst(head ptc) = removePartII x y (tail ptc)
   | otherwise = [head ptc] ++ removePartII x y (tail ptc)

wrap :: [(Char, Char)] -> [(Char, Char)]
wrap tripCode = tail tripCode ++ [head tripCode]

main :: IO ()
main = do
   let t01 = [('a','o'),('a','u')]
   print("   test case t01 - tripcode: " )
   print(t01)
   print("   result:" ++ unKnot t01)
