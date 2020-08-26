module Garden where

type Gardener = String

-- non normal form
-- data FlowerType =
--   Gardenia
--   | Daisy
--   | Rose
--   | Lilac
--   deriving Show

-- data Garden = Garden Gardener FlowerType deriving Show

-- normal form
data Garden' =
  Gardenia Gardener
  | Daisy Gardener
  | Rose Gardener
  | Lilac Gardener
  deriving Show
