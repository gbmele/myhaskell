import Data.Time.Calendar
--import Data.Time
import Data.List   --intercalate
import Data.Foldable 
import Debug.Trace
import Text.Printf
import Gbm
import Control.Monad.State.Lazy


assert False x = error x ++ "assertion failed!"
assert _     x = x

blassert False x = error x
blassert _     x = True

requesttxt = "requests70.txt"
requestdump = "requests.dzn"

main = do

 gg<- gm_CSV requesttxt
 
 let requestsdzn = map mm (gg!!0)
 let dump = requestdump
 
 writeFile dump $  intercalate newline
    ["%%writing Requests",
     quotes ++ "array[docs,1..DAYS]  of var int: REQUESTS = array2d(docs,1..DAYS," ++ quotes,
     quotes ++ show(requestsdzn)                      ++ quotes,
     "); % end of array dump \n\n"]

 appendFile dump $ intercalate newline
                ["constraint forall(doc in docs, day in 1..DAYS)",
                 "" ++ "(  if    REQUESTS[ doc, day ] = 4  then roster[ doc, day ]   =  l" ++ "",  -- 4 annual leave
                 ""++  "  elseif REQUESTS[ doc, day ] = 5  then roster[ doc, day ]   =  l" ++ "",  -- 5 conference leave
                 ""++  "  elseif REQUESTS[ doc, day ] = 6  then roster[ doc, day ]   =  l" ++ "",  -- 6 LPPA leave
                 ""++  "  elseif REQUESTS[ doc, day ] = 1  then roster[ doc, day ]   =  a" ++ "",
                 ""++  "  elseif REQUESTS[ doc, day ] = 2  then roster[ doc, day ]   =  p" ++ "",
                 ""++  "  elseif REQUESTS[ doc, day ] = 3  then (roster[ doc, day ]   =  p \\/ roster[doc,day]= a)" ++ "",
                 ""++  "  elseif REQUESTS[ doc, day ] = 91 then roster[ doc, day ]  !=  a" ++ "",
                 ""++  "  elseif REQUESTS[ doc, day ] = 92 then roster[ doc, day ]  !=  p" ++ "",
                 ""++  "  elseif REQUESTS[ doc, day ] = 93 then (roster[ doc, day ]   =  p \\/ roster[doc,day]= a)" ++ "",
                 ""++  "  elseif REQUESTS[ doc, day ] = 99 then roster[ doc, day ]   =  o" ++ "",
                 ""++  "  else true" ++"",
                 ""++  "  endif);"++ newline ++ newline]

 appendFile dump $ intercalate newline
     [ "constraint count(roster[" ++ show(doc) ++  ",..],l)="  ++ leave_count requestsdzn doc ++ ";" |  doc <- docset]


 --print $ requestsdzn
 print $ show(docset)
 print $ "num of docs is " ++ show(length(docset))
 print $ "num of days is " ++ show(length(map mm [v |(d,v)<-  (countfromone (gg!!0))]))
 print $ "length of requestarray is--" ++ show(length(requestsdzn))

 --TESTS 
 print $ foldr (&&) True 
    [ length(docset) == 26,
      length(requestsdzn)   == 1820,
      doc1 == [1..70],
      newline == "\n"
    ]
 -- sequence = foldr (>>) (return())
 sequence_ [print $ 1, print $ 12, print $ (33 + 34)]
 mapM_  print [1,2,3+44]
-- for_ (filter odd [1..5]) $ \i ->
--      do
--        putStr " "
--        putStr (show i)
--        putStr "\n"
 
 --print $ flip  filter [1..70] (\x -> x `elem` mondays)
 --print $ flip  filter [1..70] (\x -> x `mod` 7 == fromEnum Tuesday)
 --print $ filter (\x -> x `mod` 7 == fromEnum Sunday) [1..70]
 --print $ [(day,val) |(day,val) <- zip [1..100] requestarray]--,  `mod` 7 == fromEnum Monday]
 --print $ [v |(day,v) <- zip [1..100] requestarray, day `elem` mondays]
 
 foldlM (\a b -> 
        putStrLn (show a ++ "+" ++ show b ++
        "=" ++ show (a+b)) >>
        return (a+b)) 0 [1..5]

 mapM_ (\l -> Gbm.when (not $ null l) (putStrLn l))   --this is when :: when p s = if p then s else return ()
  ["","abc","def","","","ghi"]

 print $ arrdd requestsdzn JD 69
 print $ arrdd requestsdzn JD 70
 print $ arrdd requestsdzn JD 1
 print $ arrdd requestsdzn ML 5

 --DATEDZN

 let datedzn = get_dates 26 12 2022 70  --from Gbm.get_dates

 writeFile  "dates.dzn" "function string: get_day(days: d) = \n    if d = 1    then \"26/12/2022\"\n"
 
 appendFile "dates.dzn" $  intercalate newline
               ["    elseif d= "++ show(d) ++ " then \""  ++ Gbm.dmystr s ++ "\" "
                | (d,s)<-datedzn ] 

 appendFile "dates.dzn" $ "\n else \"nuffin\"\n endif; % end of the big if"

--print $ replaceAtIndex 0 99 requestsdzn
 --print $ replaceAtIndex  requestsdzn  (docday RG 1) 99 
 --print $ arrdd requestsdzn ML 5
 output [ "================================================\n"     ++ 
          intercalate  " " ["line ", escquote,vertbar,show n,vertbar,"3434 ","2323 ",show doc]  
          |(n,doc) <- zip[1..] docset]
 
 writeFile "datesarray.mzn"  "array[1..70] of var string: mdates = [\n"
 appendFile "datesarray.mzn" $ intercalate newline [escquote ++ dmystr d  ++ escquote ++ "," | (v,d) <-datedzn]
 appendFile "datesarray.mzn" "\n]; % end of mdates array"

 print $ "testing codes"
 tst <- gm_CSV "testcodes.txt"
 let tsts = map mm (tst!!0)
 print $ tsts
