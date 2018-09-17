type Name = String
type Age = Integer
data Person = Person Name Age deriving Show
data PersonInvalid =
         NameEmpty
        | AgeTooLow
        | PersonInvalidUnknown String
            deriving (Eq, Show)
mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
    | name /= "" && age > 0 = Right $ Person name age
    | name == "" = Left NameEmpty
    | not (age > 0) = Left AgeTooLow
    | otherwise =
        Left $ PersonInvalidUnknown $
            "Name was: " ++ show name ++ " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
        putStr "Please enter a name: "
        name <- getLine
        putStr "Please enter an age: "
        age <- getLine
        case mkPerson name (read age :: Integer) of
            Right person ->
                putStrLn $ "Yay! Succesfully got a person " ++ (show person)
            Left NameEmpty ->
                putStrLn "ERROR: You gave an empty name."
            Left AgeTooLow ->
                putStrLn "ERROR: The age must be above zero"
            Left (PersonInvalidUnknown str) ->
                putStrLn $ "ERROR: The person you entered was invalid: " ++ str
