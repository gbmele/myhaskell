import Gbm
import Data.List
import Debug.Trace
--import Text.Printf

assert False x = error "assertion failed!"
assert _     x = x

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

 writeFile "data.txt" "quick,OTH,AL10,LSL"

 let ll = length [v | (d,v) <- (zip [1..] (gg!!0)), d `elem` doc1 ,v `elem` leave]
 print $ length [v | (d,v) <- (zip [1..] (gg!!0)), d `elem` doc2, v `elem` leave]
 print $ length [v | (d,v) <- (zip [1..] (gg!!0)), d `elem` doc3, v `elem` leave]

 --print $ map mm [v |(d,v)<-  (zip [1..] (gg!!0)), d `elem` [1..21]]

 let dump = "requests.dzn"
 let ggfile = "gg.txt"
 let newline = "\n"
 let quotes = ""
 writeFile dump $  intercalate newline
    ["%%Requests",
     quotes ++ "array[docs,1..days]  of var int: REQUESTS = array2d(docs,1..days," ++ quotes,
     quotes ++ show(map mm [v |(d,v)<-  (zip [1..] (gg!!0))])                      ++ quotes,
     "); % end of array dump \n\n"]


 appendFile dump $ intercalate newline
                ["constraint forall(doc in docs, day in 1..days)",
                 "" ++ "(  if    REQUESTS[ doc, day ] = 4  then roster[ doc, day ]   =  l" ++ "",
                 ""++  "  elseif REQUESTS[ doc, day ] = 99 then roster[ doc, day ]   =  o" ++ "",
                 ""++  "  elseif REQUESTS[ doc, day ] = 1  then roster[ doc, day ]   =  a" ++ "",
                 ""++  "  elseif REQUESTS[ doc, day ] = 91 then roster[ doc, day ]  !=  a" ++ "",
                 ""++  "  elseif REQUESTS[ doc, day ] = 2  then roster[ doc, day ]   =  p" ++ "",
                 ""++  "  elseif REQUESTS[ doc, day ] = 92 then roster[ doc, day ]  !=  p" ++ "",
                 ""++  "  else true" ++"",
                 ""++  "  endif);"++ newline ++ newline]

 appendFile dump $ intercalate newline
     [ "constraint count([" ++ show(d) ++  ",..],l)="  ++ leave_count gg d ++ ";" |  d<-docset]

 print $  do 
            x <- [1,2,3]    
            return (x*2)

 print $ show(docset)
 print $ "num of docs is " ++ show(length(docset))
 --print $ length(map mm [v |(d,v)<-  (zip [1..] (gg!!0))])
