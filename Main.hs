import Data.Char

mm "s"   = 1 
mm "OTH" = 2
mm "_"   = 0
mm "AL"  = 4
mm "AL10" = 5
--mm (a:_) = 5
mm _     = 99


identityCheck name
  | name == "Walter" = True
  | name == "Krystal" = True
  | otherwise = False


mmm x
  | x == "s" = 1
  | otherwise = 99

massert  arg result  =  (== result)(mm arg) 

main = do
 putStrLn "Hello"
 putStrLn "World"
 --print $ mmm "O"
 --print $ mmm "OTH"

 print $ map mm ["1","_","s","aOTH","AL","a99","AL10"]

 print $ massert "s" 1
 print $ massert "OTH" 2
 print $ massert "AL10" 5
 print $ (==1) (mmm "s")

 writeFile "data.txt" "quick,OTH,AL10,LSL"

 ss <- readFile "data.txt"
 ss2 <- words ss
 print $ ss2



 