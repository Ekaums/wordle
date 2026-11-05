module Feedback (Feedback (..), GuessResult (..), checkGuess) where

import Data.List (mapAccumL)
import Data.Map.Strict qualified as M

data GuessResult = Won | Continue [Feedback]

data Feedback = Correct | Misplaced | Absent
  deriving (Show, Eq)

checkGuess :: String -> String -> GuessResult
checkGuess guess soln =
  if guess == soln
    then Won
    else Continue (computeFeedback guess soln)

{-
Algo for computing feedback:
  - Construct a mapping b/w each char in soln and # of occurences
  - Two passes to compute feedback
  First pass:
    For each char in guess:
      if exact match:
        dec count and mark correct
      else:
        mark absent

  Prioritize exact matches first to handle edge case where multiple of same char in guess

  Second pass:
    For each char in guess:
      if marked absent and count > 0:
         mark misplaced (not correct, but still present)
         dec count
-}
computeFeedback :: String -> String -> [Feedback]
computeFeedback guess soln = snd $ mapAccumL pass2Fn pass1Map (zip guess pass1Feedback)
  where
    table = M.fromListWith (+) [(c, 1) | c <- soln]
    (pass1Map, pass1Feedback) = mapAccumL pass1Fn table (zip guess soln)

    pass1Fn t (g, s)
      | g == s = (M.adjust (subtract 1) g t, Correct)
      | otherwise = (t, Absent)

    pass2Fn t (g, f)
      | f == Correct = (t, f)
      | M.findWithDefault 0 g t > 0 = (M.adjust (subtract 1) g t, Misplaced)
      | otherwise = (t, f)
