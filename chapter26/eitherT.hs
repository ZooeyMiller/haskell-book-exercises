module MyEitherT where

data EitherT e m a = EitherT { runEither :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  -- I mean it would be nice to write this nicer but also yolo
  -- the logic is - unpack the m (n _ _) from its wrapper
  -- fmap into the outer m structure, with an fmap for
  -- the inner m, then put it back in the structure
  fmap f x = EitherT $ (fmap $ fmap f) $ runEither x 


instance Applicative m => Applicative (EitherT e m) where
  pure = EitherT . pure . pure 
  -- I need to get through the layers
  (<*>) fm xm = EitherT $ (fmap (<*>) f) <*> x
    where f = runEither fm
          -- m (Either e (a -> b)) -> m (Either e a)
          x = runEither xm
