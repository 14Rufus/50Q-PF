import Data.List

--1
myEnumFromTo :: Int -> Int -> [Int]
myEnumFromTo a b | a > b = []
                 | a == b = [a]
                 | otherwise = a : myEnumFromTo (a+1) b

--2
myEnumFromThenTo :: Int -> Int -> Int -> [Int]
myEnumFromThenTo a b c | a == b && a >= c = [a]
                       | a == c = [a]
                       | a < b && a < c = a : myEnumFromThenTo b (b+(b-a)) c 
                       | otherwise = [] 

--3
maisMais :: [a] -> [a] -> [a]
maisMais () x = x
maisMais x () = x
maisMais (h:t) l = h : maisMais t l

--4
exclamation :: [a] -> Int -> a
exclamation (h:t) 0 = h
exclamation (h:t) x = exclamation t (x-1)

--5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (h:t) = myReverse t ++ [h]

--6
myTake :: Int -> [a] -> [a]
myTake 0 l = []
myTake x [] = []
myTake x (h:t) = h : myTake (x-1) t

--7
myDrop :: Int -> [a] -> [a]
myDrop 0 l = l
myDrop x [] = []
myDrop x (h:t) = myDrop (x-1) t

--8
myZip :: [a] -> [b] -> [(a,b)]
myZip [] [] = []
myZip (x:xs) (y:ys) = [(x,y):myZip xs ys]

--9
myElem :: Eq a => a -> [a] -> Bool
myElem x [] = False 
myElem x (h:t) | x==h = True
               | otherwise = myElem x t

--10
myReplicate :: Int -> a -> [a]
myReplicate 0 x = []
myReplicate n x = x : (myReplicate (n-1) x)	 

--11
myIntersperse :: a -> [a] -> [a] 			   
myIntersperse x [a] = [a]
myIntersperse x [] = []
myIntersperse x (h:t) = h : x : myIntersperse x t

--12
myGroup :: Eq a => [a] -> [[a]] 	
myGroup [] = []
myGroup l = myTake (aux12 l) l : myGroup (myDrop (aux12 l) l)


aux12 :: Eq a -> [a] -> Int
aux12 [a] = 1
aux12 (a:b:c) | a==b = 1 + aux12 (b:c)
			  | otherwise = 1

--13
myConcat :: [[a]] -> [a]
myConcat [[]] = []
myConcat [[a]] = [a]
myConcat (h:t) = h ++ myConcat t

--14
myInits :: [a] -> [[a]]			  
myInits [] = [[]]
myinits l = myinits (init l) ++ [l]

--15
myTails :: [a] -> [[a]] 
myTails [] = []
myTails l = [l] ++ myTails (tail l)

--16
myIsPrefixOf :: Eq a => [a] -> [a] -> Bool
myIsPrefixOf [] l = True
myIsPrefixOf l [] = False
myIsPrefixOf (x:xs) (y:ys) | x==Y = myIsPrefixOf xs ys
                           | otherwise = False

--17
myIsSuffixOf :: Eq a => [a] -> [a] -> Bool
myIsSuffixOf [] l = True
myIsSuffixOf l [] = False
myIsSuffixOf a b | last a == last b = myIsSuffixOf (init a) (init b)
                 | otherwise = False

--18
myIsSubsequenceOf :: Eq a => [a] -> [a] -> Bool                 
myIsSubsequenceOf l [] = False
myIsSubsequenceOf [] l = True
myIsSubsequenceOf (x:xs) (y:ys) | x==y = myIsSubsequenceOf xs ys
                                | otherwise = myIsSubsequenceOf (x:xs) ys

--19
myElemIndices :: Eq a => a -> [a] -> [Int]                                
myElemIndices x [] = []
myElemIndices x (h:t) = aux19 0 x (h:t)  


aux19 :: Eq a => a -> a -> [a] -> [Int]
aux19 a x [] = []
aux19 a x (h:t) | x==h = a : aux19 (a+1) x t
                | otherwise = aux19 (a+1) x t

--20
myNub :: Eq a => [a] -> [a] 
myNub [] = []
myNub (h:t) | myElem h t == True = myNub t
            | otherwise = h : myElem t

--21
myDelete :: Eq a => a -> [a] -> [a]
myDelete x [] = []
myDelete x (h:t) | x == h = t
                 | otherwise = h : myDelete x t

--22
mySlash:: Eq a => [a] -> [a] -> [a]                 
mySlash l [] = l
mySlash [] l = []
mySlash (x:xs) (y:ys) = mySlash (myDelete y (x:xs)) ys


--23
myUnion :: Eq a => [a] -> [a] -> [a] 
myUnion [] l = l
myUnion l [] = l
myUnion (x:xs) (y:ys) | y==x = x : myUnion xs ys
                      | otherwise = if y<x then y:x: myUnion xs ys
                      	                 else x:y: myUnion xs ys

--24
myIntersect :: Eq a => [a] -> [a] -> [a]                       	                 
myIntersect l [] = []
myIntersect [] l = []
myIntersect (x:xs) (y:ys) | myElem x (y:ys) == True = x : myIntersect xs (y:ys)
                          | otherwise = myIntersect xs (y:ys)


--25
myInsert :: Ord a => a -> [a] -> [a] 
myInsert x [] = [x]
myInsert x (h:t) | x <= h = x : h : t
                 | otherwise = h : myInsert x t

--26
myUnwords :: [String] -> String
myUnwords [a] = [a]
myUnwords (h:t) = h ++ "" ++ myUnwords t

--27
myUnlines :: [String] -> String
myUnlines [a] = [a] ++ "/n"
myUnlines (h:t) = (h ++ "/n") ++ (myUnlines t)


--28
myPMaior :: Ord a => [a] -> Int
myPMaior [a] = 0
myPMaior [] = []
myPMaior (h:t) = aux28 (h:t) 0 


aux28 :: Ord a => [a] -> Int -> Int 
aux28 (h:t) n | h == (maximum (h:t)) = n
              | otherwise = aux28 t (n+1) 

--29
myTemRepetidos :: Eq a => [a] -> Bool 
myTemRepetidos [] = False
myTemRepetidos [a] = False
myTemRepetidos (h:t) | aux29 h t || myTemRepetidos t 

aux29 :: Eq a => a -> [a] -> Bool
aux29 x [] = False
aux29 x (h:t) | x==h = True
              | otherwise = aux29 x t

--30
myAlgarismos :: [Char] -> [Char]               
myAlgarismos [] = []
myAlgarismos (h:t) | h>=0 && h<9 = h:myAlgarismos t 
                   | otherwise = myAlgarismos t

--31
myPosImpares :: [a] -> [a]
myPosImpares [] = []
myPosImpares [a] = []
myPosImpares (h:a:t) = a : myPosImpares t

 --32
myPosPares :: [a] -> [a]  
myPosPares [] = []
myPosPares [a] = [a]
myPosPares (h:a:t) = h : myPosPares t

--33
myIsSorted :: Ord a => [a] -> Bool
myIsSorted [] = True
myIsSorted [a] = True
myIsSorted (h:a:t) | h<=a = myIsSorted (a:t)
                   | otherwise = False

--34
myISort :: Ord a => [a] -> [a]                    
myISort [] = []
myISort [a] = [a]
myISort (h:a:t) | h<=a = h:myISort (a:t)
                | otherwise = insert h (myISort (a:t))

--35
myMenor :: String -> String -> Bool                
myMenor [] [] = False
myMenor [] l = True
myMenor l [] = False
myMenor (x:xs) (y:ys) | x<y = True 
                      | x==y = myMenor xs ys
                      | otherwise = False

--36
myElemMSet :: Eq a => a -> [(a,Int)] -> Bool
myElemMSet x [] = False
myElemMSet x ((a,b):t) | x==a = True
                       | otherwise = myElemMSet x t

--37
myLengthMSet :: [(a,Int)] -> Int                       
myLengthMSet [] = 0
myLengthMSet ((a,b):(c,d):t) = b + d + myLengthMSet t

--38
myConverteMSet :: [(a,Int)] -> [a]
myConverteMSet [] = []
myConverteMSet ((a,b):t) = (myReplicate b a) ++ myConverteMSet t

