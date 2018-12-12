import           Control.Monad                  ( join )

-- join :: Monad m => m (m a) -> m a
-- write bind in terms of fmap and join
bind :: Monad m => (a -> m b) -> m a -> m b
bind = (join .) . fmap
-- bind f m = join $ fmap f m
