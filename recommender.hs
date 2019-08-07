--import
import System.Random
import System.IO.Unsafe
import Data.List
randomZeroToX :: Int -> Int
randomZeroToX x = unsafePerformIO (getStdRandom (randomR (0, x)))
--Names

users = ["user1", "user2", "user3", "user4"] 
items = ["item1", "item2", "item3", "item4", "item5", "item6"] 
purchasesHistory = [ ("user1", [["item1", "item2", "item3"] , ["item1", "item2", "item4"]]) , 
              ("user2", [["item2", "item5"] , ["item4", "item5"]]) , 
              ("user3", [["item3", "item2"]]) , 
              ("user4", [])]




getList user ps = if fst (head ps) == user then snd (head ps) else getList user (tail ps)
createEmptyFreqList xs = [ (x,[]) | x <- xs]

countOcc item list  = sum [1 | a <- list ,a == item ]
flattenPurcahes user  = concat (getList user purchasesHistory)

removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys
getItemCart item [] = []
getItemCart item (cart:carts) = if elem item cart then removeItem item cart ++  (getItemCart item carts)
                                else getItemCart item carts
removeDubs [] = []
removeDubs (x:xs) = if elem x xs then x : removeDubs (removeItem x xs) else x : removeDubs xs
getUserItems item carts =removeDubs (getItemCart item carts)

getItemStats item user ps= [(item2,countOcc item2 (getItemCart item (getList user ps)))| item2 <-  removeItem item (getUserItems item (getList user ps))]

getUserStats user ps cart = [(item,getItemStats item user ps) | item <- cart]


getAllUsersStats ps = [(fst p,getUserStats (fst p) ps items) | p <- ps]

removeUser user ps = removeItem (user,getList user ps) ps
getItemsWihtoutUser user ps = [ snd a | a <- (removeUser user ps) ]


getCartStats cart = [(item,[(a,countOcc a (removeItem item cart)) | a <- (removeItem item cart)]) | item<- cart]
getCartsStats carts = [getCartStats cart | cart <- carts , not (null (snd (head (getCartStats cart))))]

repeateItems [] = [""]
repeateItems [(item,w)] =  replicate w item
repeateItems (x:xs) =  replicate w item ++ repeateItems xs
                       where w = snd x
                             item = fst x


getPossipleItems  stats = concat[ snd a | a <- stats]

getFreqForItem item stats =(item,sum [snd a | a <- getPossipleItems  stats, fst a == item])



freqListItems user =[a | a <- allItems , snd a > 0]
                     where allItems =  [ getFreqForItem item stats | item <- items]
                           stats = getUserStats user purchasesHistory items

freqListCart user cart =[a | a <- allItems , snd a > 0]
                        where allItems =  [ getFreqForItem item stats | item <- items]
                              stats = getUserStats user purchasesHistory cart



searchForItem item cart = (fst item,snd item + sum [snd a | a <-cart ,fst a == fst item])

freqListCartAndItems user cart = [searchForItem  item listCart | item <- listItem]
                                where listItem = freqListItems user
                                      listCart = freqListCart user cart


intersection:: (Eq a) => [[a]] -> [a]
intersection = foldr1 intersect
getCommonItems list1 list2 =intersection ( [fst item | item <- list1 ,(snd item) /= []] : [[fst item | item <- list2, (snd item) /= []]])
makeList  list1 list2 commons = [(fst item1,snd item1) | item1 <- list1,fst item1 `elem` commons]
findItem (item,freq) [] = ((item,freq),[])
findItem (item,freq) (x:xs) = if fst x  == item then  ((item,freq + snd x),[x]) else findItem (item,freq) xs
merge list1 [] = list1
merge [] list2 = list2
merge (x:xs) list2 = if snd item /= [] then fst item :  merge xs (removeItem  ((snd item) !! 0) list2)
                     else fst item : merge xs list2
                     where item = findItem x list2
mergeAllItems [] [] = []
mergeAllItems (x:xs) (y:ys) = (fst x,merge (snd x) (snd y)) : mergeAllItems xs ys
getIntersections list1 list2 commons =mergeAllItems (makeList list1 list2 commons)  (makeList list2 list1 commons)
purchasesIntersection list1 lists = [getIntersections list1 (snd list2) (getCommonItems  list1 (snd list2)) | list2 <- lists ,(snd list2) /= list1]

freqListUsers  user = [a | a <- allItems , snd a > 0]
                     where allItems =  [ getFreqForItem item stats | item <- items]
                           stats = concat (purchasesIntersection (getUserStats user purchasesHistory items) (getAllUsersStats purchasesHistory))


getRandomItem x = if list == [] then "" else  list !! randomZeroToX ((length list) - 1)
                   where list = repeateItems x

recommendFromITems = items !! randomZeroToX ((length items) - 1)

recommend user [] = if r == "" then recommendFromITems else r
                    where r = if randomZeroToX 1 == 0 then  recommendEmptyCart user else recommendBasedOnUsers user


recommend user cart= if r == "" then recommendFromITems else r
                     where r = if randomZeroToX 1 == 0 then  recommendBasedOnItemsInCart user cart  else recommendBasedOnUsers user 



recommendEmptyCart user =getRandomItem (freqListItems user)


recommendBasedOnItemsInCart user cart = getRandomItem (freqListCartAndItems user cart)


recommendBasedOnUsers  user = getRandomItem (freqListUsers  user)