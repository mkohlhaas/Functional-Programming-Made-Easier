module Main where

-- No further imports necessary!

import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Effect.Console (log)
import Prelude (type (~>), Unit, discard, negate, otherwise, show, (+), (-), (/=), (<), (==), (>), (>=), (>>>))

-------------
-- 1. flip --
-------------
flip ∷ ∀ a b c. (a → b → c) → b → a → c
flip f x y = f y x

--------------
-- 2. const --
--------------
const ∷ ∀ a b. a → b → a
const x _ = x

--------------
-- 3. apply --
--------------
apply ∷ ∀ a b. (a → b) → a → b
apply f = f

--------------------------------
-- 4. Define apply operator $ --
--------------------------------
infixr 0 apply as $

---------------------
-- 5. applyFlipped --
---------------------
applyFlipped ∷ ∀ a b. a → (a → b) → b
applyFlipped = flip apply

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
singleton ∷ ∀ a. a → List a
singleton x = x : Nil

-------------
-- 9. null --
-------------
null ∷ ∀ a. List a → Boolean
null Nil = true
null _ = false

--------------
-- 10. snoc --
--------------
snoc ∷ ∀ a. List a → a → List a
snoc Nil el = singleton el
snoc (x : xs) el = x : (snoc xs el)

----------------
-- 11. length --
----------------
length ∷ ∀ a. List a → Int
length Nil = 0
length (_ : xs) = 1 + length xs

--------------
-- 12. head --
--------------
head ∷ List ~> Maybe
head Nil = Nothing
head (x : _) = Just x

--------------
-- 13. tail --
--------------
tail ∷ ∀ a. List a → Maybe (List a)
tail Nil = Nothing
tail (_ : xs) = Just xs

--------------
-- 14. last --
--------------
last ∷ List ~> Maybe
last Nil = Nothing
last (x : Nil) = Just x
last (_ : xs) = last xs

--------------
-- 15. init --
--------------
init ∷ ∀ a. List a → Maybe (List a)
init Nil = Nothing
init l = Just $ go l
  where
  go Nil = Nil
  go (_ : Nil) = Nil
  go (x : xs) = x : go xs

----------------
-- 16. uncons --
----------------
uncons ∷ ∀ a. List a → Maybe { head ∷ a, tail ∷ List a }
uncons Nil = Nothing
uncons (x : xs) = Just { head: x, tail: xs }

---------------
-- 17. index --
---------------
index ∷ ∀ a. List a → Int → Maybe a
index Nil _ = Nothing
index (x : _) 0 = Just x
index (_ : xs) idx = index xs (idx - 1)

----------------------------------------------------
-- 18. Define index operator !! with precedence 8 --
----------------------------------------------------
infixl 8 index as !!

-------------------
-- 19. findIndex --
-------------------
findIndex ∷ ∀ a. (a → Boolean) → List a → Maybe Int
findIndex p l = go l 0
  where
  go Nil _ = Nothing
  go (x : xs) idx
    | p x = Just idx
    | otherwise = go xs (idx + 1)

-----------------------
-- 20. findLastIndex --
-----------------------
findLastIndex ∷ ∀ a. (a → Boolean) → List a → Maybe Int
findLastIndex p l = go l 0 Nothing
  where
  go Nil _ res = res
  go (x : xs) idx res
    | p x = go xs (idx + 1) (Just idx)
    | otherwise = go xs (idx + 1) res

-----------------
-- 21. reverse --
-----------------
reverse ∷ List ~> List
reverse l = go l Nil
  where
  go Nil res = res
  go (x : xs) res = go xs (x : res)

----------------
-- 22. concat --
----------------
concat ∷ ∀ a. List (List a) → List a
concat Nil = Nil
concat (Nil : xss) = concat xss
concat ((x : xs) : xss) = x : concat (xs : xss)

----------------
-- 23. filter --
----------------
filter ∷ ∀ a. (a → Boolean) → List a → List a
filter _ Nil = Nil
filter p (x : xs)
  | p x = x : filter p xs
  | otherwise = filter p xs

-------------------
-- 24. catMaybes --
-------------------
catMaybes ∷ ∀ a. List (Maybe a) → List a
catMaybes Nil = Nil
catMaybes (Nothing : xs) = catMaybes xs
catMaybes (Just x : xs) = x : catMaybes xs

---------------
-- 25. range --
---------------
range ∷ Int → Int → List Int
range from to
  | from < to = from : range (from + 1) to
  | from > to = from : range (from - 1) to
  | otherwise = singleton from

----------------------------------------------------
-- 26. Define range operator .. with precedence 8 --
----------------------------------------------------
infix 8 range as ..

--------------
-- 27. take --
--------------
take ∷ ∀ a. Int → List a → List a
take _ Nil = Nil
take n (x : xs)
  | n > 0 = x : take (n - 1) xs
  | otherwise = Nil

--------------
-- 28. drop --
--------------
drop ∷ ∀ a. Int → List a → List a
drop _ Nil = Nil
drop n l@(_ : xs)
  | n > 0 = drop (n - 1) xs
  | otherwise = l

-------------------
-- 29. takeWhile --
-------------------
takeWhile ∷ ∀ a. (a → Boolean) → List a → List a
takeWhile _ Nil = Nil
takeWhile p (x : xs)
  | p x = x : takeWhile p xs
  | otherwise = Nil

-------------------
-- 30. dropWhile --
-------------------
dropWhile ∷ ∀ a. (a → Boolean) → List a → List a
dropWhile _ Nil = Nil
dropWhile p l@(x : xs)
  | p x = dropWhile p xs
  | otherwise = l

-----------------
-- 31. takeEnd --
-----------------
takeEnd ∷ ∀ a. Int → List a → List a
takeEnd n = go >>> snd
  where
  go Nil = Tuple 0 Nil
  go (x : xs) = case go xs of
    Tuple n' xs'
      | n' < n → Tuple (n' + 1) (x : xs')
      | otherwise → Tuple n' xs'

-----------------
-- 32. dropEnd --
-----------------
dropEnd ∷ ∀ a. Int → List a → List a
dropEnd n = go >>> snd
  where
  go Nil = Tuple 0 Nil
  go (x : xs) = case go xs of
    Tuple n' xs'
      | n' < n → Tuple (n' + 1) xs'
      | otherwise → Tuple n' (x : xs')

-------------
-- 33. zip --
-------------
zip ∷ ∀ a b. List a → List b → List (Tuple a b)
zip Nil _ = Nil
zip _ Nil = Nil
zip (x : xs) (y : ys) = Tuple x y : zip xs ys

---------------
-- 34. unzip --
---------------
unzip ∷ ∀ a b. List (Tuple a b) → Tuple (List a) (List b)
unzip Nil = Tuple Nil Nil
unzip (Tuple x y : ts) = unzip ts # \(Tuple l1 l2) → Tuple (x : l1) (y : l2)

----------
-- Main --
----------
main ∷ Effect Unit
main = do
  log "Exercise Chapter 5."
  log (show ((flip const 1 2) == 2))
  log $ show $ (flip const 1 2) == 2
  (flip const 1 2) == 2 # show # log
  log $ show $ singleton "xyz" == ("xyz" : Nil)
  log $ show $ null Nil == true
  log $ show $ null ("abc" : Nil) == false
  log $ show $ snoc (1 : 2 : Nil) 3 == (1 : 2 : 3 : Nil)
  log $ show $ (length $ 1 : 2 : 3 : Nil) == 3
  log $ show $ head ("abc" : "123" : Nil) == Just "abc"
  log $ show $ head (Nil ∷ List Unit) == Nothing
  log $ show $ tail (Nil ∷ List Unit) == Nothing
  log $ show $ tail ("abc" : "123" : Nil) == Just ("123" : Nil)
  log $ show $ last (Nil ∷ List Unit) == Nothing
  log $ show $ last ("a" : "b" : "c" : Nil) == Just "c"
  log $ show $ (last $ "a" : "b" : "c" : Nil) == Just "c"
  log $ show $ init (Nil ∷ List Unit) == Nothing
  log $ show $ init (1 : Nil) == Just Nil
  log $ show $ init (1 : 2 : Nil) == Just (1 : Nil)
  log $ show $ init (1 : 2 : 3 : Nil) == Just (1 : 2 : Nil)
  log $ show $ uncons (1 : 2 : 3 : Nil) == Just { head: 1, tail: (2 : 3 : Nil) }
  log $ show $ uncons (Nil ∷ List Unit) == Nothing
  log $ show $ index (1 : Nil) 4 == Nothing
  log $ show $ index (1 : 2 : 3 : Nil) 1 == Just 2
  log $ show $ index (Nil ∷ List Unit) 0 == Nothing
  log $ show $ index (1 : 2 : 3 : Nil) (-99) == Nothing
  log $ show $ (1 : 2 : 3 : Nil) !! 1 == Just 2
  log $ show $ findIndex (_ >= 2) (1 : 2 : 3 : Nil) == Just 1
  log $ show $ findIndex (_ >= 99) (1 : 2 : 3 : Nil) == Nothing
  log $ show $ findIndex (10 /= _) Nil == Nothing
  log $ show $ findLastIndex (_ == 10) Nil == Nothing
  log $ show $ findLastIndex (_ == 10) (10 : 5 : 10 : -1 : 2 : 10 : Nil) == Just 5
  log $ show $ findLastIndex (_ == 10) (11 : 12 : Nil) == Nothing
  log $ show $ reverse (10 : 20 : 30 : Nil) == (30 : 20 : 10 : Nil)
  log $ show $ concat ((1 : 2 : 3 : Nil) : (4 : 5 : Nil) : (6 : Nil) : (Nil) : Nil) == (1 : 2 : 3 : 4 : 5 : 6 : Nil)
  log $ show $ filter (4 > _) (1 : 2 : 3 : 4 : 5 : 6 : Nil) == (1 : 2 : 3 : Nil)
  log $ show $ catMaybes (Just 1 : Nothing : Just 2 : Nothing : Nothing : Just 5 : Nil) == (1 : 2 : 5 : Nil)
  log $ show $ range 1 10 == (1 : 2 : 3 : 4 : 5 : 6 : 7 : 8 : 9 : 10 : Nil)
  log $ show $ range 3 (-3) == (3 : 2 : 1 : 0 : -1 : -2 : -3 : Nil)
  log $ show $ 1 .. 10 == (1 : 2 : 3 : 4 : 5 : 6 : 7 : 8 : 9 : 10 : Nil)
  log $ show $ 3 .. (-3) == (3 : 2 : 1 : 0 : -1 : -2 : -3 : Nil)
  log $ show $ take 5 (12 : 13 : 14 : Nil) == (12 : 13 : 14 : Nil)
  log $ show $ take 5 (-7 : 9 : 0 : 12 : -13 : 45 : 976 : -19 : Nil) == (-7 : 9 : 0 : 12 : -13 : Nil)
  log $ show $ drop 2 (1 : 2 : 3 : 4 : 5 : 6 : 7 : Nil) == (3 : 4 : 5 : 6 : 7 : Nil)
  log $ show $ drop 10 (Nil ∷ List Unit) == Nil
  log $ show $ takeWhile (_ > 3) (5 : 4 : 3 : 99 : 101 : Nil) == (5 : 4 : Nil)
  log $ show $ takeWhile (_ == -17) (1 : 2 : 3 : Nil) == Nil
  log $ show $ dropWhile (_ > 3) (5 : 4 : 3 : 99 : 101 : Nil) == (3 : 99 : 101 : Nil)
  log $ show $ dropWhile (_ == -17) (1 : 2 : 3 : Nil) == (1 : 2 : 3 : Nil)
  log $ show $ takeEnd 3 (1 : 2 : 3 : 4 : 5 : 6 : Nil) == (4 : 5 : 6 : Nil)
  log $ show $ takeEnd 10 (1 : Nil) == (1 : Nil)
  log $ show $ dropEnd 3 (1 : 2 : 3 : 4 : 5 : 6 : Nil) == (1 : 2 : 3 : Nil)
  log $ show $ dropEnd 10 (1 : Nil) == Nil
  log $ show $ zip (1 : 2 : 3 : Nil) ("a" : "b" : "c" : "d" : "e" : Nil) == ((Tuple 1 "a") : (Tuple 2 "b") : (Tuple 3 "c") : Nil)
  log $ show $ zip ("a" : "b" : "c" : "d" : "e" : Nil) (1 : 2 : 3 : Nil) == ((Tuple "a" 1) : (Tuple "b" 2) : (Tuple "c" 3) : Nil)
  log $ show $ zip (Nil ∷ List Unit) (1 : 2 : Nil) == Nil
  log $ show $ unzip (Tuple 1 "a" : Tuple 2 "b" : Tuple 3 "c" : Nil) == (Tuple (1 : 2 : 3 : Nil) ("a" : "b" : "c" : Nil))
  log $ show $ unzip (Tuple "a" 1 : Tuple "b" 2 : Tuple "c" 3 : Nil) == (Tuple ("a" : "b" : "c" : Nil) (1 : 2 : 3 : Nil))
  log $ show $ unzip (Nil ∷ (List (Tuple Unit Unit))) == (Tuple Nil Nil)
