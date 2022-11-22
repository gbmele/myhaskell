import Gbm
import Data.List
import Debug.Trace
import Text.Printf

assert False x = error x ++ "assertion failed!"
assert _     x = x

blassert False x = error x
blassert _     x = True

identityCheck name
  | name == "Walter" = True
  | name == "Krystal" = True
  | otherwise = False

main = do

{-
 print $ massert "OTH" 0
 print $ massert "AL10" 4
 print $ massert "U" 99
 print $ (==1) (mmm "s")
 print $ massert "U00:00-23:59" 99
 print $ massert "L" 4
-}


 gg<- gm_CSV "requests7.txt"
 print $ gg!!0
 let requests = gg!!0

 writeFile "data.txt" "quick,OTH,AL10,LSL"

 let ll = length [v | (d,v) <- (zip [1..] requests), d `elem` doc1 ,v `elem` leave]
 print $ length [v | (d,v) <- (zip [1..] (gg!!0)), d `elem` doc2, v `elem` leave]
 print $ length [v | (d,v) <- (zip [1..] (gg!!0)), d `elem` doc3, v `elem` leave]

 --print $ map mm [v |(d,v)<-  (zip [1..] (gg!!0)), d `elem` [1..21]]

 let dump = "requests.dzn"
 let ggfile = "gg.txt"
 let newline = "\n"
 let quotes = ""
 let requestarray = (map mm [v |(d,v)<-  (zip [1..] (gg!!0))] )    

 writeFile dump $  intercalate newline
    ["%%writing Requests",
     quotes ++ "array[docs,1..days]  of var int: REQUESTS = array2d(docs,1..days," ++ quotes,
     --quotes ++ show(requestarray) ++ quotes,
     quotes ++ show(map mm [v |(d,v)<-  (zip [1..] requests)])                      ++ quotes,
     "); % end of array dump \n\n"]


 appendFile dump $ intercalate newline
                ["constraint forall(doc in docs, day in 1..days)",
                 "" ++ "(  if    REQUESTS[ doc, day ] = 4  then roster[ doc, day ]   =  l" ++ "",  -- 4 annual leave
                 ""++  "  elseif REQUESTS[ doc, day ] = 5  then roster[ doc, day ]   =  l" ++ "",  -- 5 conference leave
                 ""++  "  elseif REQUESTS[ doc, day ] = 6  then roster[ doc, day ]   =  l" ++ "",  -- 6 LPPA leave
                 ""++  "  elseif REQUESTS[ doc, day ] = 1  then roster[ doc, day ]   =  a" ++ "",
                 ""++  "  elseif REQUESTS[ doc, day ] = 2  then roster[ doc, day ]   =  p" ++ "",
                 ""++  "  elseif REQUESTS[ doc, day ] = 91 then roster[ doc, day ]  !=  a" ++ "",
                 ""++  "  elseif REQUESTS[ doc, day ] = 92 then roster[ doc, day ]  !=  p" ++ "",
                 ""++  "  elseif REQUESTS[ doc, day ] = 99 then roster[ doc, day ]   =  o" ++ "",
                 ""++  "  else true" ++"",
                 ""++  "  endif);"++ newline ++ newline]

 appendFile dump $ intercalate newline
     [ "constraint count([" ++ show(doc) ++  ",..],l)="  ++ leave_count gg doc ++ ";" |  doc <- docset]




 print $ show(docset)
 print $ "num of docs is " ++ show(length(docset))
 print $ "num of days is " ++ show(length(map mm [v |(d,v)<-  (zip [1..] (gg!!0))]))
 print $ "length of requestarray is--" ++ show(length(requestarray))

 print $ foldr (&&) True 
    [ blassert (mm "O"  == 0) "failed mm '0' ",
      blassert (mm "L"  == 4) "failed mm 'L' ",
     mm "AL" == 4,
     length(docset) == 8,
     length(requestarray)   == 56,
     doc1 == [1..7],
     doc2 == [8..14],
     doc3 == [15..21],
     blassert (1==1) "1==1"
    ]
 -- sequence = foldr (>>) (return())
 sequence_ [print $ 1, print $ 12, print $ (33 + 34)]
 mapM_  print [1,2,3+44]
