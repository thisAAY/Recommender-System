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
              ("D" , [ [ "milk" , "meat" , "chicken" , "yogurt" ] , [ "beans" , "cereal" , "flour" , "sugar"] , [ "tomatoes" , "oil" , "chicken" ] ] ) ,
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

getAllUsersStats us = [(user,getUserStats user) | user <- us]

removeUser user ps = removeItem (user,getList user ps) ps
getItemsWihtoutUser user ps = [ snd a | a <- (removeUser user ps) ]


getCartStats cart = [(item,[(a,countOcc a (removeItem item cart)) | a <- (removeItem item cart)]) | item<- cart]
getCartsStats carts = [getCartStats cart | cart <- carts , not (null (snd (head (getCartStats cart))))]
purchasesIntersection user = concat [getCartsStats carts | carts <- getItemsWihtoutUser user purchases]

getMaxFreq xs =maximum [snd a  | a <- xs]

getPossipleItems user ps = concat [snd a | a <- concat ( purchasesIntersection user )]

repeateItems [(item,w)] =  replicate w item
repeateItems (x:xs) =  replicate w item ++ repeateItems xs
                        where w = snd x
                              item = fst x
recommendBasedOnUsers' user ps = repeateItems( getPossipleItems user ps)
recommendBasedOnUsers user   =  list !!  randomZeroToX ( length list)
                                where list = recommendBasedOnUsers' user purchases




recommendEmptyCart user =if null (getList user purchases) then recommendBasedOnUsers user
                         else if ((randomZeroToX 1) == 0) then recommendBasedOnUsers user
                         else (concat (getList user purchases)) !!  randomZeroToX (length (concat(getList user purchases)))

getAllIntersectionWithItem user item =concat [snd a | a <- concat (purchasesIntersection user),fst a == item]
getPossipleIntersectionWithItem user item  = repeateItems  (getAllIntersectionWithItem user item)
getPossipleIntersectionWithCart user cart = concat [getPossipleIntersectionWithItem user item | item <- cart]

recommend user cart = list !! randomZeroToX (length list - 1)
                      where list = getPossipleIntersectionWithCart user cart





