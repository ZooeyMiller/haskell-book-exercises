import           Control.Monad
import           Data.Char
import           System.Exit   (exitSuccess)

palindrome :: IO ()
palindrome = forever $ do
            line1 <- getLine
            let
                normalised = removeNonLetters $ fmap toLower line1
                    in
                        case (normalised == reverse normalised) of
                            True  -> putStrLn "It's a palindrome!"
                            False -> do
                                putStrLn "Nope!"
                                exitSuccess

removeNonLetters :: String -> String
removeNonLetters = filter go
                where
                    go char = (cc > 96) && (cc < 122)
                        where cc = ord char


