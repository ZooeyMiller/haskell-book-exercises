{-# LANGUAGE ScopedTypeVariables #-}

import           Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
    [
        DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123)) , DbNumber 9001
            , DbString "Hello, world!"
            , DbDate (UTCTime
                        (fromGregorian 1921 5 1)
                        (secondsToDiffTime 34123))
    ]


filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr go []
    where
        go (DbDate x) l = x : l
        go _  l         = l

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr go []
        where
            go (DbNumber x) l = x : l
            go _ l            = l

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent =  foldr go date . filterDbDate
           where date = UTCTime
                            (fromGregorian 0 0 0 )
                            (secondsToDiffTime 0)
                 go x y = if x > y then x else y

sumDb :: [DatabaseItem] -> Integer
sumDb = foldr (+) 0 . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb l = (/) (fromIntegral $ sumDb l) (fromIntegral $ length $ filterDbNumber l)
