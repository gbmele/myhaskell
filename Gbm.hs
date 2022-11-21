module Gbm where

import Data.List

assert False x = error "assertion failed!"
assert _     x = x



mm "0"       = 0               --  0 nuffin
mm "A"       = 1               --  1 am
mm "AM"      = 1               --  2 pm 
mm "a"       = 1
mm "p"       = 2
mm "P"       = 2
mm "OTH"     = 0
mm "AL"      = 4
mm "L10"     = 4
mm "L"       = 4
mm "CL"      = 4
mm "LPPA"    = 5
mm "UA"      = 91             -- 91 not AM 
mm "UP"      = 92             -- 92 not PM 
mm "U"       = 99
mm "U00:00-23:59" = 99
mm "(U:_)"   = 99
mm "777"     = 777
mm _         = 0

massert  arg result  =  (== result)(mm arg) 
mmm x
 | x == "s"  = 1
 | otherwise = 99999


leave = ["AL","L","L10","LPPA","CL"]

data Docs =
      ZERODOCS    -- take up the 0 index - dont use
    | RG --1
    | GM --2
    | DB --3
    | CC --4
    | MC --5
    | RM --6 
    | DL --7
    | ML --8
    | DH --9
    deriving (Enum,Show,Bounded)

docset = [RG,GM,DB,CC,MC,RM,DL,ML]

data Shifts = 
     ZEROSHIFT
   | AM
   | PM
   | OFF
   | LEAVE
   deriving (Enum, Show, Bounded)

fE x = fromEnum x

days x y = enumFromTo x y    

di xx = 7

doc1 = days  (0 + 1)      (1*(di 9))
doc2 = days  (1*(di 9)+1) (2*(di 9))
doc3 = days  (2*(di 9)+1) (3*(di 9))
doc4 = days  (3*(di 9)+1) (4*(di 9))
doc5 = days  (4*(di 9)+1) (5*(di 9))
doc6 = days  (5*(di 9)+1) (6*(di 9))
doc7 = days  (6*(di 9)+1) (7*(di 9))
doc8 = days  (7*(di 9)+1) (8*(di 9))
doc9 = days  (8*(di 9)+1) (9*(di 9))

doc10 = days  (9*(di 9)+1)  (10*(di 9))
doc11 = days  (10*(di 9)+1) (11*(di 9))
doc12 = days  (11*(di 9)+1) (12*(di 9))
doc13 = days  (12*(di 9)+1) (13*(di 9))
doc14 = days  (13*(di 9)+1) (14*(di 9))
doc15 = days  (14*(di 9)+1) (15*(di 9))
doc16 = days  (15*(di 9)+1) (16*(di 9))
doc17 = days  (16*(di 9)+1) (17*(di 9))
doc18 = days  (17*(di 9)+1) (18*(di 9))
doc19 = days  (18*(di 9)+1) (19*(di 9))
doc20 = days  (19*(di 9)+1) (20*(di 9))
doc21 = days  (20*(di 9)+1) (21*(di 9))
doc22 = days  (21*(di 9)+1) (22*(di 9))
doc23 = days  (22*(di 9)+1) (23*(di 9))
doc24 = days  (23*(di 9)+1) (25*(di 9))
doc25 = days  (24*(di 9)+1) (26*(di 9))
doc26 = days  (25*(di 9)+1) (27*(di 9))
doc100 = days 1000 1000

-- this array is 0 based - i need to start at 1 index
docs = [[199..199],doc1,doc2,doc3,doc4,doc5,doc6,doc7,doc8]
 --,doc9,doc10,doc11,doc12,doc13,
 --                 doc14,doc15,doc16,doc17,doc18,doc19,doc20,doc21,doc22,doc23,doc24,doc25,doc26]

get_doc arr doc = arr!!(fromEnum doc)

--         (show(length [v | (d,v) <- (zip [1..] (gg!!0)), d `elem` (get_doc docs RG) ,v `elem` leave])),

leave_count arr doc = (show(length [v | (d,v) <- (zip [1..] (arr!!0)), d `elem` (get_doc docs doc) ,v `elem` leave]))

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


--writeNestedList :: Show a => FileName -> [[a]] -> IO ()
--writeNestedList fileName xss =
--    do
--       fh <- openFile  fileName  WriteMode  -- get a file handle
--       mapM_  ((hPutStrLn fh) . show)  xss
--       hClose fh


new_row c  = [ 0 | _ <- [1..c]]

block r c  = [ new_row c | _ <- [1..r]]

print_block x = mapM_ print x    -- why does this work

get_column g c = [row !! c | row <- g]


