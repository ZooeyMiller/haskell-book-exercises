foldBoolIf :: a -> a -> Bool -> a
foldBoolIf x y bool = if bool then x else y

foldBoolGuard :: a -> a -> Bool -> a
foldBoolGuard x y bool | bool == True  = x
                       | bool == False = y
