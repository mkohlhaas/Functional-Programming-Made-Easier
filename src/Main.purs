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

--------------
-- 2. const --
--------------

--------------
-- 3. apply --
--------------

--------------------------------
-- 4. Define apply operator $ --
--------------------------------

---------------------
-- 5. applyFlipped --
---------------------

---------------------------------------------------------
-- 6. Define applyFlipped operator # with precedence 1 --
---------------------------------------------------------

------------------------------------------------------
-- 7. Define List Cons operator : with precedence 6 --
------------------------------------------------------

------------------
-- 8. singleton --
------------------

-------------
-- 9. null --
-------------

--------------
-- 10. snoc --
--------------

----------------
-- 11. length --
----------------

--------------
-- 12. head --
--------------

--------------
-- 13. tail --
--------------

--------------
-- 14. last --
--------------

--------------
-- 15. init --
--------------

----------------
-- 16. uncons --
----------------

---------------
-- 17. index --
---------------

----------------------------------
-- 18. Define index operator !! --
----------------------------------

-------------------
-- 19. findIndex --
-------------------

-----------------------
-- 20. findLastIndex --
-----------------------

-----------------
-- 21. reverse --
-----------------

----------------
-- 22. concat --
----------------

----------------
-- 23. filter --
----------------

-------------------
-- 24. catMaybes --
-------------------

---------------
-- 25. range --
---------------

----------------------------------------------------
-- 26. Define range operator .. with precedence 8 --
----------------------------------------------------

--------------
-- 27. take --
--------------

--------------
-- 28. drop --
--------------

-------------------
-- 29. takeWhile --
-------------------

-------------------
-- 30. dropWhile --
-------------------

-----------------
-- 31. takeEnd --
-----------------

-----------------
-- 32. dropEnd --
-----------------

-------------
-- 33. zip --
-------------

---------------
-- 34. unzip --
---------------

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
