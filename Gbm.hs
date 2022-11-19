module Gbm where

mm "s"       = 1 
mm "A"       = 4
mm "a"       = 1
mm "OTH"     = 0
mm "AL"      = 4
mm "AL10"    = 4
mm "L"       = 4
mm "U"            = 99
mm "U00:00-23:59" = 99
mm "(U:_)"   = 99
mm _         = 0

massert  arg result  =  (== result)(mm arg) 

mmm x
 | x == "s"  = 1
 | otherwise = 99999


leave = ["A","AL","L"]

data Docs =
      ZZZZ
    | RG
    | GM 
    | DB
    | CC
    | MC
    deriving (Enum,Show,Bounded)


fE x = fromEnum x

days x y = enumFromTo x y    

doc1 = days 1 7
doc2 = days 8 14
doc3 = days 15 21

new_row c  = [ 0 | _ <- [1..c]]

block r c  = [ new_row c | _ <- [1..r]]

print_block x = mapM_ print x    -- why does this work

get_column g c = [row !! c | row <- g]


wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

splitcomma = wordsWhen (==',')

--gm_RCV =  (map splitcomma) . lines


gm_RCV fileName = do
    fileText <- readFile fileName
    return  $  ((map splitcomma) . lines)  fileText
    
--commonWords2 file1 file2 =
gm_CSV fileName = do
    fileText <- readFile fileName
    return $ ((map splitcomma) . lines) fileText

rcv array index_size row col = array!!0!!((row-1)*index_size+col-1)
