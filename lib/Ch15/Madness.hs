module Ch15.Madness where

import Data.Monoid

type Adjective = String

type Adverb = String

type Noun = String

type Exclamation = String

madlibbin' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbin' e adv noun adj =
  e <> "! he said"
    <> adv
    <> " as he jumped into his car "
    <> noun
    <> " and drove off with his "
    <> adj
    <> " wife."

madlibbinBetter' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbinBetter' e adv noun adj =
  mconcat [e, "! he said", adv, " as he jumped into his car ", noun, " and drove off with his ", adj, " wife."]

main :: IO ()
main = do
  print $ madlibbin' "exc" "adv" "noun" "adj"
  print $ madlibbinBetter' "exc" "adv" "noun" "adj"