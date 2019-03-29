{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

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

instance Monad m => Monad (EitherT e m) where
    return = pure
--  (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
    (>>=) x f = EitherT $ runEither x >>= g f
      where g _ (Left z)   = pure $ Left z
            g h (Right z) = runEither $ h z


swapEither :: Either e a -> Either a e
swapEither (Left x) = Right x
swapEither (Right x) = Left x


swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT = EitherT . fmap swapEither . runEither 


eitherT :: Monad m =>
           (a -> m c)
        -> (b -> m c)
        -> EitherT a m b
        -> m c
eitherT f g x = runEither x >>= h
    where h (Left y) = f y 
          h (Right y) = g y 

data StateT s m a = StateT { runStateT :: s -> m (a,s) }

instance Functor m => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b 
  fmap f (StateT sma) = StateT $ fg 
    where fg s = fmap (\(x, ss) -> (f x, ss)) (sma s)





        -- sma :: s -> m (a,s) 
     

 

