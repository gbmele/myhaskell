import Data.Time.Calendar
--import Data.Time
import Data.List   --intercalate
import Data.Foldable 
import Debug.Trace
import Data.Monoid
import Text.Printf
import Gbm
import Control.Monad.State.Lazy
import Data.Char (isSpace)
import Data.List
import Control.Applicative (liftA2)
import Zebra

x >$> f = f x  -- = f $ x as well

f >.> g = g . f
f |>  g = g . f

addone x = x + 1

fmt 0 = "  "
fmt x = printf "%02d" x

assert False x = error x ++ "assertion failed!"
assert _     x = x

blassert False x = error x
blassert _     x = True

requesttxt = "requests70.txt"
requestdump = "requests.dzn"

rstrip = reverse . dropWhile isSpace . reverse

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
       "" ++ "(  if    REQUESTS[ doc, day ] = 99  then roster[ doc, day ]   =  o" ++ "", --day off
        ""++  "  elseif REQUESTS[ doc, day ] = "++ show(annual) ++"  then roster[ doc, day ]   =  l % AL" ++ "", -- annual leave
        ""++  "  elseif REQUESTS[ doc, day ] = 42   then roster[ doc, day ]   =  l % CONF" ++ "", -- Conference leave
        ""++  "  elseif REQUESTS[ doc, day ] = 43   then roster[ doc, day ]   =  l % LSL"  ++ "", -- long service leave
        ""++  "  elseif REQUESTS[ doc, day ] = 44   then roster[ doc, day ]   =  l % PAR"  ++ "", -- long service leave
        ""++  "  elseif REQUESTS[ doc, day ] = 4   then roster[ doc, day ]   =  c % csupport" ++ "",
        ""++  "  elseif REQUESTS[ doc, day ] = 1   then roster[ doc, day ]   =  a" ++ "",
        ""++  "  elseif REQUESTS[ doc, day ] = 2   then roster[ doc, day ]   =  p" ++ "",
        ""++  "  elseif REQUESTS[ doc, day ] = 3   then (roster[ doc, day ]   =  p \\/ roster[doc,day]= a)" ++ "",
        ""++  "  elseif REQUESTS[ doc, day ] = 91  then roster[ doc, day ]  !=  a" ++ "",
        ""++  "  elseif REQUESTS[ doc, day ] = 92  then roster[ doc, day ]  !=  p" ++ "",
        ""++  "  elseif REQUESTS[ doc, day ] = 99  then roster[ doc, day ]   =  o" ++ "",
        ""++  "  else true" ++"",
         ""++  "  endif);"++ newline ++ newline]

 appendFile dump $ intercalate newline
     [ "constraint count(roster[" ++ show(doc) ++  ",..],l)="  ++ leave_count requestsdzn doc ++ ";" |  doc <- docset]


 --print $ requestsdzn
 print $ show(docset)
 print $ "num of docs is " ++ show(length(docset))
 print $ "num of days is " ++ show(length(map mm [v |(d,v)<-  (zip[1..] (gg!!0))]))
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

 let datedzn = get_dates 8 5 2023 (13*7)  --from Gbm.get_dates
 let start_date = "08/05/2023"
 writeFile  "dates.dzn" "function string: get_day(days: d) = \n    if d = 1    then \"08/05/2023\"\n"
 
 appendFile "dates.dzn" $  intercalate newline
               ["    elseif d= "++ show(d) ++ " then \""  ++ Gbm.dmystr s ++ "\" "
                | (d,s)<-datedzn, d >= 2 ] 

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

 print $ arrdd requestsdzn DB 1
 print $ "RG" ++ show([requests requestsdzn RG])
 print $ "GM  " ++ show([requests requestsdzn GM])
 --almost ther
 --let ew =  do  
--            ew_1 <- replaceAtIndex (requests requestsdzn EW) 1 66
 --           ew_2 <- replaceAtIndex  ew_1 2 55
 --           return $ ew_1
 --print $ ew

 --let ew_3       = replaceAtIndex (ew_2) 1 66 
 --    ew_2       = replaceAtIndex (ew_1) 5 55
 --    ew_1       = replaceAtIndex (requests requestsdzn EW) 3 77
 --    in print $ ew_3

 -- print $ [ v | (d, v) <- (zip [1 ..] requestsdzn), d `elem` (get_doc docs GM)]
 
 print $ fmap ((+1).(*2)) [1..3]

 let pure_maybe = pure :: a -> Maybe a

 print $ pure_maybe 3
 print $ pure (+1) <*> Just 1 
 print $ [(+1),  (*2)] <*> [1,2,3] -- [2,3,4,10,20,30]
 print $ [(+1) . (*2) . (*3)] <*> [1,2,3]
 print $ (\f->f+1) <$> Just 3
 print $  pure (\f -> f + 1) <*> Just 1
 print $ sequenceA [Just 3, Just 2, Just 1]  
 print $ sequenceA [(+3),(+2),(+1)] 3  
 --print $ sequenceA Just 1 Just 5 Just 9
 print $ liftA2 (+) (Just 5) (Just 6)

 print $ [1,2] >>= (\x -> return (x + 1))

 print $ liftA2 add [1,3] [1,2]
 print $ liftA2 add (Just 1) (Just 5)
 --output [ "================================================\n"     ++ 
 --         intercalate  " " ["line ", escquote,vertbar,show n,vertbar,"3434 ","2323 ",show doc]  
 --         |(n,doc) <- zip[1..] docset]
 

 
 output2 ["array[docs,1..DAYS] of var int: REQUESTS = array2d(docs,1..DAYS,[\n"]
 output2 
    [ intercalate "," [show(v)++ if v<10 then " " else "",       -- just to pad
                   if n `mod` 70 == 0 then "% day 70" ++ " " ++ show(docset!!((n `div` 70)-1)) ++ "\n"  --zero index sucks
                   else
                   if n `mod` 7 == 0 then "% day "++ show(n `mod` 70) ++
                                           " " ++ show(docset!!(n `div` 70)) ++ "\n"
                   else ""] 
           | (n,v)  <- zip [1..] requestsdzn]
 output2 ["]); %end of REQUESTS ARRAY\n"]


 
 writeFile "rr2.dzn" $ intercalate " "
            ["array[docs,1..DAYS] of var int: REQUESTS = array2d(docs,1..DAYS,[\n"]
 appendFile "rr2.dzn" $ intercalate " " 
        [ intercalate "," [show(v)++ if v<10 then " " else "", 
                           if n `mod` 70 == 0 then "% day 70" ++ " " ++ show(docset!!((indexset n 70)-1)) ++ "\n\n" 
                           else
                           if n `mod` 7 == 0 then "% day "++ show(n `mod` 70) ++
                           " " ++ show(docset!!(indexset n 70)) ++ "\n"
                            else ""] 
           | (n,v)  <- zip [1..] requestsdzn]
 appendFile "rr2.dzn" $ intercalate " "  
          ["]); %end of REQUESTS ARRAY\n"]
 
 appendFile "rr2.dzn" $ intercalate newline
     [ "constraint count(roster[" ++ show(doc) ++  ",..],l)="  ++ leave_count requestsdzn doc ++ ";" |  doc <- docset]

 --print $ bind( bind (ret 4) inc) inc

 

 print $ fmap (+1) [1,2,3]
 print $ [1,2,3] >>= return . (+1)
 print $ mappend (Sum 1) (Sum 2)
 print $ mappend (Product 3)(Product 4)

 print $ (Unit 3) 
    >>=  (\x -> Unit (x + 2))  
    >>= pure

 print $   ap (Just (2+)) (Just 3)
 print $   (Just(2+)) <*> (Just 3)
 print $ ap [(+1)][1,2,3]
 print $ fmap (+1) [1,2,3]
 print $ (+11)<$>[1,2,4]    -- <$> is the infix of fmap and therefore ilftM
 print $ (\x -> x + 1) <$> [5,6,7]
 print $ ap ((+) . minimum) maximum [1,2,3]
 print $ liftM2 (+) minimum maximum [1,2,3]
 print $ liftM (1+) [1,2,3]
 print $ liftM (\x-> x + 1)[1,2,3]
 print$ liftA2 (++) ["33 "]["444","ff"]
 print $ (+) <$> [1,4] <*> [1,2,3]
 print $ (++) <$> ["Hi there","and"] <*> ["greg","leo"]

-- output3 [ show(v)  | v  <-  requestsdzn] --learn haskell in minizinc!!

-- output2 [ intercalate "," [show(requests requestsdzn doc) ++ "\n"]  | doc <- docset]

 --output3 [ intercalate "," [show(doc,day,shift) | (day,shift) <- zip[1..] (requests requestsdzn doc)]  | doc <- docset]

 --print $ [[(doc,day,shift) | (day,shift) <- zip[1..] (requests requestsdzn doc)] | doc <- docset]

 Gbm.log "gere\nrerer\n"
 print $ 33
 print $ [3] >>= k
 print $ 1 >$> (\x -> x + 1)

 --let xx =  (\y -> y + 2) >.> (\x -> x + 1) in print $ xx 44

-- print $ xx 44 where 
--       xx =  add4 . add11 . (+21)
--       add4  = (+4)
--       add11 = (\x -> x + 11)
 print $ map ((+1) |> (2*)) [1,3,4]
 
-- let   xx = add1 |> times2 |> (+3)
--       add1 = (1+)
--       times2 = (2*)
--       in xx 1
 print $ [1,2,3] >>= k
 print $ Left "boom" >>= \x -> return (x+1)  

-- print $ apply(pure(add1))([1,2,3])
 print $  (\x  -> x + 1) <$>  [1,2,3]
 print $  (pure (\x  -> x + 1)) <*>  [1,2,3]
 print $ "wow it jap is bind"
 print $   join(ap (pure (\x  -> [x + 1]))   [1,2,3])
 print $   fmap  (\x  -> x + 1)   [1,2,3]
 print $    map  (\x  -> x + 1)   [1,2,3]
 let ifte x y z = if x then y else z

 print $ ifte (1==1) "yes" "no"
 print  $ pure(\x -> x + 1)("Ignore this?")(1) -- pure is the K konstant constant from SKI calculus!!!! WTF

 print $ pure ((\x -> x + 1) . (\y -> y + 1)) <*> [1,2,3]
 print $ (pure((\x -> x + 1) . (\y -> y + 1))) `ap` [1,2,3]

-- print $  [addone . (\y -> y +11)] <*> [1,2,3] where addone = (1+)

 print $ map ((\x -> x + 1) |> (\y -> 2 * y)) [10,12]

 --mapM_ ((1+) |> (2*) |> (\x -> dumpme (show(x) ++ "\n")))  [1,2]  where dumpme = appendFile "ggggg.txt"

--doc1 = days (0 * ind + 1) (1 * ind)
 let num = 15
 let idx = 5
 print $ [x | (i,x)<- zip [1..] [1..50]  ]

 let xxx =  [1,32,4] >>= (\x -> return (addone x)) >>= (\xx -> return (xx + 3)) in print $ xxx

 print $ join $ Just (Just 4)
 print $ Debuggable(15,[]) >>= inc >>= inc >>= return
 
 let x = head $ permutations [1..5]
 print $ x
 print $ solve_zebra

 let x = do
          d1 <- [ (x,y) |  x <-[1..7], y <- [0..2]]
          --guard $ d1 
          return $ d1 
 print $ x


 print $ length(comb 14 8)

 let wk1 = Week PM AM AM PM OFF OFF OFF
 let wk2 = Week OFF PM AM AM OFF AM AM
 let wk3 = Week AM  PM OFF AM AM AM AM

 let wks = [wk1,wk2]
 let rr = permutations [wk1,wk2]
 print $ wk1
 print $ rule1pm wk1
 print $ rule1pm wk2

 print $ count AM wks
 print $ count_days sun PM wks
-- print $  (permutations [wk1,wk2,wk3])!!0

 --filter (liftM2 (&&) odd (> 100)) [1..200]
 print $ filter (liftM2 (||) (==PM)(==AM) )      $ ( [mon,tue,wed] <*>) wks
 
 -- this is probably the easiest
 print $ filter(\x -> x==PM || x == AM || x ==OFF) $ ( [mon,tue,wed] <*>) wks

 print $ filter(or <$> sequence [(==PM),(==AM), (==OFF)]) $ ( [mon,tue,wed] <*>) wks

--same
--but beloe no good -- have to nest the .||. 
 print $ filter ((==PM) .||. ((==AM) .||. (==OFF)))  $ ( [mon,tue,wed] <*>) wks

--(.&&.) f g a = (f a) && (g a)
--(.||.) f g a = (f a) || (g a)


 
 print $ filter(==PM) $ ( [mon,tue,wed] <*>) wks
 print $  length . filter(==PM) $ ( ap [mon,tue,wed]) wks
 print $ "mm"
 forM_ (permutations [wk1,wk2]) $ \x -> do
    print $  length . filter(==PM) $ ( ap [mon,tue,wed]) x

-- print $ [ (length . filter (==PM) $ (ap [mon,tue,wed]) x, y)  | x<- permutations [wk1,wk2], let y = wk1]   

 print $ [  (a,b,c,d,e)  
           | a <- [0..5] 
           , b <- [0..5]
           , c <- [0..5]
           , d <- [0..5]
           , e <- [0..5]
           , a + b + c + d + e == 2
           ]   
 --print $  [ [ [i ++  j ++ k  | i <- permutations [1..3]] | j <- permutations [4..6]] | k <- permutations [7..9]]
 print $ sum[y | (x,y) <- zip[1..][20..41], monday x  ]
 print $ [y | (x,y) <- zip [1..][20..41], tuesday x  ] 
                             

 print $ [y | (x,y) <- zip[1..][1..400], x `elem` (getindexset (fromEnum RG) 70)]

 print $ getindexset 2 7         