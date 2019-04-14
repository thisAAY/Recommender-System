--import
import System.Random
import System.IO.Unsafe

randomZeroToX :: Int -> Int
randomZeroToX x= unsafePerformIO (getStdRandom (randomR (0, x)))
--Names

users = ["user1", "user2", "user3", "user4"] 
items = ["item1", "item2", "item3", "item4", "item5", "item6"] 
purchases = [ ("user1", [["item1", "item2", "item3"] , ["item1", "item2", "item4"]]) , 
              ("user2", [["item2", "item5"] , ["item4", "item5"]]) , 
              ("user3", [["item3", "item2"]]) , 
              ("user4", [])]
-- use for easy debuging
ppp = purchases


getList user ps = if fst (head ps) == user then snd (head ps) else getList user (tail ps)
createEmptyFreqList xs = [ (x,[]) | x <- xs]

countOcc item list  = sum [1 | a <- list ,a == item ]
flattenPurcahes user  = concat (getList user purchases)

removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys
getItemCart item [] = []
getItemCart item (cart:carts) = if elem item cart then removeItem item cart ++  (getItemCart item carts)
                                else getItemCart item carts
removeDubs [] = []
removeDubs (x:xs) = if elem x xs then x : removeDubs (removeItem x xs) else x : removeDubs xs
getUserItems item carts =removeDubs (getItemCart item carts)
--getUserItems user ps = removeDubs (getUserItemsWithDubs user ps)
-- getUserStats user ps = [(item,[(a,countOcc a (getItemCart item (getList user ps)) ) 
--                     | a <- getItemCart item (getList user ps)]) | item <- items]

getItemStats item user ps= [(item2,countOcc item2 (getItemCart item (getList user ps)))| item2 <-  removeItem item (getUserItems item (getList user ps))]

getUserStats user ps = [(item,getItemStats item user ps) | item <- items]


getAllUsersStats ps = [(fst p,getUserStats (fst p) ps) | p <- ps]

removeUser user ps = removeItem (user,getList user ps) ps
getItemsWihtoutUser user ps = [ snd a | a <- (removeUser user ps) ]


getCartStats cart = [(item,[(a,countOcc a (removeItem item cart)) | a <- (removeItem item cart)]) | item<- cart]
getCartsStats carts = [getCartStats cart | cart <- carts , not (null (snd (head (getCartStats cart))))]
purchasesIntersection user ps = [getUserStats u ps | u <- users,u /= user ]

-- getMaxFreq xs =maximum [snd a  | a <- xs]

-- getPossipleItems user ps =concat [ snd b| b <- concat [ snd a | a <-  purchasesIntersection user ps ]]

-- recommendBasedOnUsers' user ps = repeateItems( getPossipleItems user ps)
-- recommendBasedOnUsers user   =  list !!  randomZeroToX ( length list - 1)
--                                 where list = recommendBasedOnUsers' user purchases




-- recommendEmptyCart user =if null (getList user purchases) then recommendBasedOnUsers user
--                          else if ((randomZeroToX 1) == 0) then recommendBasedOnUsers user
--                          else (concat (getList user purchases)) !!  randomZeroToX (length (concat(getList user purchases)))

-- getAllIntersectionWithItem user item =concat [snd a | a <- concat (purchasesIntersection user),fst a == item]
-- getPossipleIntersectionWithItem user item  = repeateItems  (getAllIntersectionWithItem user item)
-- getPossipleIntersectionWithCart user cart = concat [getPossipleIntersectionWithItem user item | item <- cart]

repeateItems [(item,w)] =  replicate w item
repeateItems (x:xs) =  replicate w item ++ repeateItems xs
                        where w = snd x
                              item = fst x

recommend user [] = list !! randomZeroToX (length list - 1)
                      where list = repeateItems (freqListItems user)
                      
getPossipleItems  stats = concat[ snd a | a <- stats]

getFreqForItem item stats = sum [snd a | a <- getPossipleItems  stats, fst a == item]


freqListItems user =[a | a <- allItems , snd a > 0]
                     where allItems =  [(fst item,getFreqForItem (fst item) stats) | item <- stats]
                           stats = getUserStats user purchases
