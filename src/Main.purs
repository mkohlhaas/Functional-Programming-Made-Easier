module Main where

-- No further imports!

import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Prelude (type (~>), Unit, discard, negate, otherwise, show, (+), (-), (/=), (<), (==), (>), (>=))

-------------
-- 1. flip --
-------------
flip :: ∀ a b c. (a -> b -> c) -> b -> a -> c
flip f b a = f a b

--------------
-- 2. const --
--------------
const :: ∀ a b. a -> b -> a
const a _ = a

--------------
-- 3. apply --
--------------
apply :: ∀ a b. (a -> b) -> a -> b
apply f = f

--------------------------------
-- 4. Define apply operator $ --
--------------------------------
infixr 0 apply as $

---------------------
-- 5. applyFlipped --
---------------------
applyFlipped :: ∀ a b. a -> (a -> b) -> b
applyFlipped a f = f a

---------------------------------------------------------
-- 6. Define applyFlipped operator # with precedence 1 --
---------------------------------------------------------
infixl 1 applyFlipped as #

------------------------------------------------------
-- 7. Define List Cons operator : with precedence 6 --
------------------------------------------------------
infixr 6 Cons as :

------------------
-- 8. singleton --
------------------
singleton :: ∀ a. a -> List a
singleton a = a : Nil

-------------
-- 9. null --
-------------
null :: ∀ a. List a -> Boolean
null Nil = true
null _ = false

--------------
-- 10. snoc --
--------------
snoc :: ∀ a. List a -> a -> List a
snoc Nil a = singleton a
snoc (x : xs) a = x : (snoc xs a)

----------------
-- 11. length --
----------------
length :: ∀ a. List a -> Int
length Nil = 0
length (_ : as) = 1 + length as

--------------
-- 12. head --
--------------
head :: List ~> Maybe
head Nil = Nothing
head (a : _) = Just a

--------------
-- 13. tail --
--------------
tail :: ∀ a. List a -> Maybe (List a)
tail Nil = Nothing
tail (_ : as) = Just as

--------------
-- 14. last --
--------------
last :: List ~> Maybe
last (a : Nil) = Just a
last (_ : as) = last as
last _ = Nothing

--------------
-- 15. init --
--------------
init :: ∀ a. List a -> Maybe (List a)
init Nil = Nothing
init l = Just $ go l
  where
  go Nil = Nil
  go (_ : Nil) = Nil
  go (x : xs) = x : go xs

----------------
-- 16. uncons --
----------------
uncons :: ∀ a. List a -> Maybe { head :: a, tail :: List a }
uncons Nil = Nothing
uncons (a : as) = Just { head: a, tail: as }

---------------
-- 17. index --
---------------
index :: ∀ a. List a -> Int -> Maybe a
index Nil _ = Nothing
index (a : _) 0 = Just a
index (_ : as) n = index as (n - 1)

----------------------------------
-- 18. Define index operator !! --
----------------------------------
infixl 8 index as !!

-------------------
-- 19. findIndex --
-------------------
findIndex :: ∀ a. (a -> Boolean) -> List a -> Maybe Int
findIndex p = go 0
  where
  go _ Nil = Nothing
  go n (a : as)
    | p a = Just n
    | otherwise = go (n + 1) as

-----------------------
-- 20. findLastIndex --
-----------------------
findLastIndex :: ∀ a. (a -> Boolean) -> List a -> Maybe Int
findLastIndex p = go 0 Nothing
  where
  go _ nLast Nil = nLast
  go n nLast (a : as)
    | p a = go (n + 1) (Just n) as
    | otherwise = go (n + 1) nLast as

-----------------
-- 21. reverse --
-----------------
reverse :: List ~> List
reverse = go Nil
  where
  go l Nil = l
  go l (a : as) = go (a : l) as

----------------
-- 22. concat --
----------------
concat :: ∀ a. List (List a) -> List a
concat Nil = Nil
concat (Nil : loas) = concat loas
concat ((a : as) : loas) = a : concat (as : loas)

----------------
-- 23. filter --
----------------
filter :: ∀ a. (a -> Boolean) -> List a -> List a
filter _ Nil = Nil
filter p (a : as)
  | p a = a : filter p as
  | otherwise = filter p as

-------------------
-- 24. catMaybes --
-------------------
catMaybes :: ∀ a. (List (Maybe a)) -> List a
catMaybes Nil = Nil
catMaybes (Nothing : mAs) = catMaybes mAs
catMaybes (Just a : mAs) = a : catMaybes mAs

---------------
-- 25. range --
---------------
range :: Int -> Int -> List Int
range from to
  | from == to = singleton from
  | from < to = from : range (from + 1) to
  | otherwise = from : range (from - 1) to

----------------------------------------------------
-- 26. Define range operator .. with precedence 8 --
----------------------------------------------------
infix 8 range as ..

--------------
-- 27. take --
--------------
take :: ∀ a. Int -> List a -> List a
take _ Nil = Nil
take n (a : as)
  | n > 0 = a : take (n - 1) as
  | otherwise = Nil

--------------
-- 28. drop --
--------------
drop :: ∀ a. Int -> List a -> List a
drop _ Nil = Nil
drop n l@(_ : as)
  | n > 0 = drop (n - 1) as
  | otherwise = l

-------------------
-- 29. takeWhile --
-------------------
takeWhile :: ∀ a. (a -> Boolean) -> List a -> List a
takeWhile _ Nil = Nil
takeWhile p (a : as)
  | p a = a : takeWhile p as
  | otherwise = Nil

-------------------
-- 30. dropWhile --
-------------------
dropWhile :: ∀ a. (a -> Boolean) -> List a -> List a
dropWhile _ Nil = Nil
dropWhile p l@(a : as)
  | p a = dropWhile p as
  | otherwise = l

-----------------
-- 31. takeEnd --
-----------------
takeEnd :: ∀ a. Int -> List a -> List a
takeEnd n l = go Nil n (reverse l)
  where
  go acc _ Nil = acc
  go acc n' (a : as)
    | n' > 0 = go (a : acc) (n' - 1) as
    | otherwise = acc

-----------------
-- 32. dropEnd --
-----------------
dropEnd :: ∀ a. Int -> List a -> List a
dropEnd n l = go n (reverse l)
  where
  go _ Nil = Nil
  go n' l'@(_ : as)
    | n' > 0 = go (n' - 1) as
    | otherwise = reverse l'

-------------
-- 33. zip --
-------------
zip :: ∀ a b. List a -> List b -> List (Tuple a b)
zip Nil _ = Nil
zip _ Nil = Nil
zip (a : as) (b : bs) = Tuple a b : zip as bs

---------------
-- 34. unzip --
---------------
unzip :: ∀ a b. List (Tuple a b) -> Tuple (List a) (List b)
unzip ts = go Nil Nil ts
  where
  go as bs Nil = Tuple (reverse as) (reverse bs)
  go as bs (Tuple a b : ts') = go (a : as) (b : bs) ts'

----------
-- Main --
----------
main :: Effect Unit
main = do
  log "Exercise Chapter 5."
  log (show ((flip const 1 2) == 2))
  log $ show $ (flip const 1 2) == 2
  (flip const 1 2) == 2 # show # log
  log $ show $ (singleton "xyz") == ("xyz" : Nil)
  log $ show $ (null Nil) == true
  log $ show $ (null ("abc" : Nil)) == false
  log $ show $ (snoc (1 : 2 : Nil) 3) == (1 : 2 : 3 : Nil)
  log $ show $ (length $ 1 : 2 : 3 : Nil) == 3
  log $ show $ (head ("abc" : "123" : Nil)) == (Just "abc")
  log $ show $ (head (Nil :: List Boolean)) == Nothing
  log $ show $ (tail (Nil :: List Boolean)) == Nothing
  log $ show $ (tail ("abc" : "123" : Nil)) == (Just ("123" : Nil))
  log $ show $ (last (Nil :: List Boolean)) == Nothing
  log $ show $ (last ("a" : "b" : "c" : Nil)) == (Just "c")
  log $ show $ (last $ "a" : "b" : "c" : Nil) == (Just "c")
  log $ show $ (init (Nil :: List Boolean)) == Nothing
  log $ show $ (init (1 : Nil)) == (Just Nil)
  log $ show $ (init (1 : 2 : Nil)) == (Just (1 : Nil))
  log $ show $ (init (1 : 2 : 3 : Nil)) == (Just (1 : 2 : Nil))
  log $ show $ (uncons (1 : 2 : 3 : Nil)) == (Just { head: 1, tail: (2 : 3 : Nil) })
  log $ show $ (uncons (Nil :: List Boolean)) == Nothing
  log $ show $ (index (1 : Nil) 4) == Nothing
  log $ show $ (index (1 : 2 : 3 : Nil) 1) == (Just 2)
  log $ show $ (index (Nil :: List Boolean) 0) == Nothing
  log $ show $ ((1 : 2 : 3 : Nil) !! 1) == (Just 2)
  log $ show $ (findIndex (_ >= 2) (1 : 2 : 3 : Nil)) == (Just 1)
  log $ show $ (findIndex (_ >= 99) (1 : 2 : 3 : Nil)) == Nothing
  log $ show $ (findIndex (10 /= _) Nil) == Nothing
  log $ show $ (findLastIndex (_ == 10) Nil) == Nothing
  log $ show $ (findLastIndex (_ == 10) (10 : 5 : 10 : -1 : 2 : 10 : Nil)) == (Just 5)
  log $ show $ (findLastIndex (_ == 10) (11 : 12 : Nil)) == Nothing
  log $ show $ (reverse (10 : 20 : 30 : Nil)) == (30 : 20 : 10 : Nil)
  log $ show $ (concat ((1 : 2 : 3 : Nil) : (4 : 5 : Nil) : (6 : Nil) : (Nil) : Nil)) == (1 : 2 : 3 : 4 : 5 : 6 : Nil)
  log $ show $ (filter (4 > _) $ (1 : 2 : 3 : 4 : 5 : 6 : Nil)) == (1 : 2 : 3 : Nil)
  log $ show $ (catMaybes (Just 1 : Nothing : Just 2 : Nothing : Nothing : Just 5 : Nil)) == (1 : 2 : 5 : Nil)
  log $ show $ (range 1 10) == (1 : 2 : 3 : 4 : 5 : 6 : 7 : 8 : 9 : 10 : Nil)
  log $ show $ (range 3 (-3)) == (3 : 2 : 1 : 0 : -1 : -2 : -3 : Nil)
  log $ show $ (1 .. 10) == (1 : 2 : 3 : 4 : 5 : 6 : 7 : 8 : 9 : 10 : Nil)
  log $ show $ (3 .. (-3)) == (3 : 2 : 1 : 0 : -1 : -2 : -3 : Nil)
  log $ show $ (take 5 (12 : 13 : 14 : Nil)) == (12 : 13 : 14 : Nil)
  log $ show $ (take 5 (-7 : 9 : 0 : 12 : -13 : 45 : 976 : -19 : Nil)) == (-7 : 9 : 0 : 12 : -13 : Nil)
  log $ show $ (drop 2 (1 : 2 : 3 : 4 : 5 : 6 : 7 : Nil)) == (3 : 4 : 5 : 6 : 7 : Nil)
  log $ show $ (drop 10 (Nil :: List Boolean)) == Nil
  log $ show $ (takeWhile (_ > 3) (5 : 4 : 3 : 99 : 101 : Nil)) == (5 : 4 : Nil)
  log $ show $ (takeWhile (_ == -17) (1 : 2 : 3 : Nil)) == Nil
  log $ show $ (dropWhile (_ > 3) (5 : 4 : 3 : 99 : 101 : Nil)) == (3 : 99 : 101 : Nil)
  log $ show $ (dropWhile (_ == -17) (1 : 2 : 3 : Nil)) == (1 : 2 : 3 : Nil)
  log $ show $ (takeEnd 3 (1 : 2 : 3 : 4 : 5 : 6 : Nil)) == (4 : 5 : 6 : Nil)
  log $ show $ (takeEnd 10 (1 : Nil)) == (1 : Nil)
  log $ show $ (dropEnd 3 (1 : 2 : 3 : 4 : 5 : 6 : Nil)) == (1 : 2 : 3 : Nil)
  log $ show $ (dropEnd 10 (1 : Nil)) == Nil
  log $ show $ (zip (1 : 2 : 3 : Nil) ("a" : "b" : "c" : "d" : "e" : Nil)) == ((Tuple 1 "a") : (Tuple 2 "b") : (Tuple 3 "c") : Nil)
  log $ show $ (zip ("a" : "b" : "c" : "d" : "e" : Nil) (1 : 2 : 3 : Nil)) == ((Tuple "a" 1) : (Tuple "b" 2) : (Tuple "c" 3) : Nil)
  log $ show $ (zip (Nil :: List Boolean) (1 : 2 : Nil)) == Nil
  log $ show $ (unzip (Tuple 1 "a" : Tuple 2 "b" : Tuple 3 "c" : Nil)) == (Tuple (1 : 2 : 3 : Nil) ("a" : "b" : "c" : Nil))
  log $ show $ (unzip (Tuple "a" 1 : Tuple "b" 2 : Tuple "c" 3 : Nil)) == (Tuple ("a" : "b" : "c" : Nil) (1 : 2 : 3 : Nil))
  log $ show $ (unzip (Nil :: (List (Tuple Boolean Boolean)))) == (Tuple Nil Nil)
