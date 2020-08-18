module Filtering where
{-
1. Given the above, how might we write a filter function that would give
us all the multiples of 3 out of a list from 1–30?
-}
result = filter (\x -> rem x 3 == 0) [1 .. 30]

{-
2. Recalling what we learned about function composition,
how could we compose the above function with the length function to tell us
how many multiples of 3 there are between 1 and 30?
-}
result' = length . filter (\x -> rem x 3 == 0) $ [1 .. 30]

{-
3. Next, we’re going to work on removing all articles (“the,” “a,” and “an”) from sentences.
You want to get to something that works
like this:

Prelude> myFilter "the brown dog was a goof"
["brown","dog","was","goof"]

You may recall that earlier in this chapter, 
we asked you to write a function that separates a string into a list of strings by separating them at spaces.
That is a standard library function called words.
You may consider starting this exercise by using words (or your own version, of course).
-}
myFilter :: String -> [String]
myFilter s = filter isNotArticle $ words s
 where
  isNotArticle x = case x of
    "the" -> False
    "a"   -> False
    "an"  -> False
    _     -> True
