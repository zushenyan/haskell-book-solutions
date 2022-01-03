module Ch18.TheAnswerIsTheExercise where

import Control.Monad (join)

bind :: Monad m => (a -> m b) -> m a -> m b
bind f ma = join $ f <$> ma