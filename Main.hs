import Gbm


identityCheck name
  | name == "Walter" = True
  | name == "Krystal" = True
  | otherwise = False


main = do
 print $ massert "s" 1
 print $ massert "OTH" 0
 print $ massert "AL10" 4
 print $ massert "U" 99
 print $ (==1) (mmm "s")
 print $ massert "U00:00-23:59" 99
 print $ massert "L" 4



 gg<- gm_CSV "requests7.txt"
 print $ gg!!0

 writeFile "data.txt" "quick,OTH,AL10,LSL"

 let ll = length [v | (d,v) <- (zip [1..] (gg!!0)), d `elem` doc1 ,v `elem` leave]
 print $ length [v | (d,v) <- (zip [1..] (gg!!0)), d `elem` doc2, v `elem` leave]
 print $ length [v | (d,v) <- (zip [1..] (gg!!0)), d `elem` doc3, v `elem` leave]

 print $ map mm [v |(d,v)<-  (zip [1..] (gg!!0)), d `elem` [1..21]]

 print $ fromEnum RG

 let dump = "requests.dzn"

 writeFile dump "ffff\n"
 appendFile dump "and this is a new line\n"
 appendFile dump "array[docs,1..days] of var int: REQUESTS = array2d(docs,1..days,\n"
 appendFile dump (show(map mm [v |(d,v)<-  (zip [1..] (gg!!0)), d `elem` [1..21]]))

 appendFile dump "\n ); % end of array dump \n"

 appendFile dump "constraint count([RG,..],l)=" 
 appendFile dump  (show(length [v | (d,v) <- (zip [1..] (gg!!0)), d `elem` doc1 ,v `elem` leave]))
 appendFile dump ";\n" 

 appendFile dump  "constraint count([GM,..],l)="
 appendFile dump  (show(length [v | (d,v) <- (zip [1..] (gg!!0)), d `elem` doc2 ,v `elem` leave]))
 appendFile dump ";\n" 

 appendFile dump  "constraint count([DB,..],l)="
 appendFile dump  (show(length [v | (d,v) <- (zip [1..] (gg!!0)), d `elem` doc3 ,v `elem` leave]))
 appendFile dump ";\n" 
