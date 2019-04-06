--import
import System.Random
import System.IO.Unsafe

randomZeroToX :: Int -> Int
randomZeroToX x= unsafePerformIO (getStdRandom (randomR (0, x)))
--Names

users = [ "A" , "B" , "C" , "D" , "E" ]
items = [ "suit" , "dress" , "shoes" , "T−shirt" , "Jacket" , "skirt" , "shorts" ,
 "shirt" , "trousers" , "scarf" , "mp3 player" , "TV" , "LCD screen" , "headphone" , "laptop" , "keyboard" , "mouse" , "cellphone" , "headphone" , "earphone" , "milk" ,"cheese" , "bread" , "chocolate" , "meat" ,"flour" , "sugar" ,"oil" ,"tomatoes" , "chicken" ,"yogurt" , "cereal" , "beans" , "fool","eggs" ]

purchases = [ ( "A" , [[ "dress" , "shoes" ] , [ "milk" , "cheese" , "eggs"]]) ,
              ("B" , [[ "earphone" , "mouse" , "laptop" ] , [ "mp3 player" ] ] ) ,
              ("C" , [ [ "bread" , "milk" ] , [ "shoes" ] ] ) ,
              ("D" , [ [ "milk" , "meat" , "chicken" , " yogurt" ] , [ "beans" , "cereal" , "flour" , "sugar"] , [ "tomatoes" , "oil" , "chicken" ] ] ) ,
              ("E" , [] )]




getList user ps = if fst (head ps) == user then snd (head ps) else getList user (tail ps)
createEmptyFreqList xs = [ (x,[]) | x <- xs]

countOcc item list  = sum [1 | a <- list ,a == item ]
flattenPurcahes user  = concat (getList user purchases)

removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys

getItemCart _ [] = []
getItemCart item carts = if (elem item (head carts))
                                 then (removeItem item (head carts)) 
                                 else if (length carts == 1) then [] 
                                 else  getItemCart item (tail carts)

getUserStats user = [(item,[(a,countOcc a (getItemCart item (getList user purchases)) ) 
                    | a <- getItemCart item (getList user purchases)]) | item <- items]

getAllUsersStats = [(user,getUserStats user) | user <- users]












