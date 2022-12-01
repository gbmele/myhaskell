module Gbm where

import Data.List
import Data.Time
import Data.Time.Calendar

when p s = if p then s else return ()

countfromone = zip [1 ..]

data REQUESTS
  = Zero
  | AM
  | PM
  | CL
  | AL
  | LPPA
  | OTH
  | U
  | UA
  | UP

mm "0" = 0 --  0 nuffin
mm "A" = 1 --  1 am
mm "AM" = 1
mm "MH0900-1800(9.00)" = 1
mm "MH0800-1730(9.50)" = 1
mm "MH0800-1800(10.00)" = 1
mm "P" = 2
mm "MH1600-2400(8.00)"  = 2
mm "MH1400-2400(10.00)" = 2
mm "A 08.00-23.59" = 3   -- avail am or pm
mm "A08.00-23.59"  = 3   -- avail am or pm

mm "OTH" = 0
mm "AL (0.00)" = 4
mm "AL (10.00)" = 4
mm "AL(10.00)" = 4
mm "AL(0.00)"  = 4

mm "AL" = 4
mm "CL" = 5
mm "CONF (10.00)" = 5
mm "CONF(10.00)"  = 5
mm "CONF (0.00)"  = 5
mm "CONF(0.00)"   = 5
mm "LPPA (10.00)" = 6
mm "LPPA(10.00)"  = 6
mm "LPPA" = 6
mm "UA"              = 91 -- 91 not AM
mm "UP"              = 92 -- 92 not PM


mm "U" = 99 -- just plain unavailable all day
mm "U 00:00-23:59" = 99
mm "U00:00-23:59" = 99

mm "777" = 777
--mm "3"        = 0
mm "99" = 99
mm _ = 0

log x = appendFile "log.txt" x

massert arg result = (== result) (mm arg)

--leave = ["AL","L","L10","LPPA","CL","CONF"]
annual = 4

conference = 5

lppa = 6

leavedzn = [annual, conference, lppa]

--docs   = {RG,GM,DB,CC,MC,SD,ML,HL,DL,RM,GN,EW,LB,DH,SK,BL,CP,RP,AR,LS,AV,LC,KM,DZ,BB,JD};

data Docs
  = DOCZERO -- take up the 0 index - dont use
  | RG --1
  | GM --2
  | DB --3
  | CC --4
  | MC --5
  | SD --6
  | ML --7
  | HL --8
  | DL --9
  | RM
  | GN
  | EW
  | LB
  | DH
  | SK
  | BL
  | CP
  | RP
  | AR
  | LS
  | AV
  | LC
  | KM
  | DZ
  | BB
  | JD
  deriving (Enum, Show, Bounded)

docset = [RG, GM, DB, CC, MC, SD, ML, HL, DL, RM, GN, EW, LB, DH, SK, BL, CP, RP, AR, LS, AV, LC, KM, DZ, BB, JD]

data Days
  = Sunday -- == 0 this is very importatn
  | Monday -- == 1
  | Tuesday -- == 2 etc
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  deriving (Show, Enum)

--arrdd arr doc day = arr!!(((fromEnum doc -1) * 70) + (day-1))

docday doc day = ((fromEnum doc - 1) * ind) + (day -1)

arrdd arr doc day = arr !! (docday doc day)

replaceAtIndex xs i x = take i xs ++ [x] ++ drop (i + 1) xs

--mondays =  [x |x <- [1..70],   x `mod` 7 == fromEnum Monday]
--tuesdays = [x |x <- [1..70],   x `mod` 7 == fromEnum Tuesday]
--wednesdays = [x |x <- [1..70], x `mod` 7 == fromEnum Wednesday]
--thursdays = filter(\x -> x `mod` 7 == fromEnum Thursday) [1..70]

fE x = fromEnum x

days x y = enumFromTo x y

daycount = 70

ind = 70

di xx = 7

doc1 = days 1 (1 * ind)

doc2 = days (1 * ind + 1) (2 * ind)

doc3 = days (2 * ind + 1) (3 * ind)

doc4 = days (3 * ind + 1) (4 * ind)

doc5 = days (4 * ind + 1) (5 * ind)

doc6 = days (5 * ind + 1) (6 * ind)

doc7 = days (6 * ind + 1) (7 * ind)

doc8 = days (7 * ind + 1) (8 * ind)

doc9 = days (8 * ind + 1) (9 * ind)

doc10 = days (9 * ind + 1) (10 * ind)

doc11 = days (10 * ind + 1) (11 * ind)

doc12 = days (11 * ind + 1) (12 * ind)

doc13 = days (12 * ind + 1) (13 * ind)

doc14 = days (13 * ind + 1) (14 * ind)

doc15 = days (14 * ind + 1) (15 * ind)

doc16 = days (15 * ind + 1) (16 * ind)

doc17 = days (16 * ind + 1) (17 * ind)

doc18 = days (17 * ind + 1) (18 * ind)

doc19 = days (18 * ind + 1) (19 * ind)

doc20 = days (19 * ind + 1) (20 * ind)

doc21 = days (20 * ind + 1) (21 * ind)

doc22 = days (21 * ind + 1) (22 * ind)

doc23 = days (22 * ind + 1) (23 * ind)

doc24 = days (23 * ind + 1) (24 * ind)

doc25 = days (24 * ind + 1) (25 * ind)

doc26 = days (25 * ind + 1) (26 * ind)

doc100 = days 1000 1000

-- this array is 0 based - i need to start at 1 index
docs =
  [ [199 .. 199],
    doc1,
    doc2,
    doc3,
    doc4,
    doc5,
    doc6,
    doc7,
    doc8,
    doc9,
    doc10,
    doc11,
    doc12,
    doc13,
    doc14,
    doc15,
    doc16,
    doc17,
    doc18,
    doc19,
    doc20,
    doc21,
    doc22,
    doc23,
    doc24,
    doc25,
    doc26
  ]

get_doc arr doc = arr !! (fromEnum doc)

leave_count arr doc =
  ( show
      ( length
          [ v | (d, v) <- (zip [1 ..] arr), d `elem` (get_doc docs doc), v `elem` leavedzn
          ]
      )
  )

--FILES

wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'

splitcomma = wordsWhen (== ',')

gm_CSV fileName = do
  fileText <- readFile fileName
  return $ ((map splitcomma) . lines) fileText

--FORMATTING
newline = "\n"

quotes = ""

escquote = "\""

vertbar = "|"

output x = putStrLn $ intercalate "\n" x

--DATES
yyyy str = take 4 str

mnth str = take 2 $ drop 5 str

dd str = drop 8 str

--(dd s) ++ "/"++ (mnth s) ++ "/" ++ (yyyy s)
dmystr s = (dd s) ++ "/" ++ (mnth s) ++ "/" ++ (yyyy s)

-- provide a list of [(1,"01012022")....]  from get_dates 1 1 2022 70
get_dates d1 m1 y1 num =
  [(d, s) | (d, s) <- (zip [1 .. num] (map showGregorian [fromGregorian y1 m1 d1 .. fromGregorian 2030 3 5]))]

--OTHER

rcv array index_size row col = array !! 0 !! ((row -1) * index_size + col -1)

new_row c = [0 | _ <- [1 .. c]]

block r c = [new_row c | _ <- [1 .. r]]

print_block x = mapM_ print x -- why does this work

get_column g c = [row !! c | row <- g]
