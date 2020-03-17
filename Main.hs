module Main where

import Control.Applicative
import Data.Char
import qualified Data.Map.Strict as Map

newtype HTMLField = HTMLField String deriving (Show, Eq)
type HTMLProperty = Map.Map String String 
data HTMLValue = HTMLText String 
    | HTMLValue HTMLField [HTMLProperty] [HTMLValue] HTMLValue
    deriving (Show, Eq)

type HTML = Map.Map HTMLField HTMLValue

type ParserInput = String
newtype ParserError = ParserError String deriving (Show, Eq)
data ParserSuccess a = ParserSuccess {
    unparsed :: String,
    parsed :: a 
} deriving (Show, Eq)

newtype Parser a = Parser {
    runParser :: ParserInput -> Either ParserError (ParserSuccess a)
}

instance Functor Parser where
    fmap f (Parser p) = Parser $ \input -> do
       (ParserSuccess unparsed parsed) <- p input
       Right $ ParserSuccess unparsed (f parsed)

instance Applicative Parser where
    pure a = Parser $ \input -> Right $ ParserSuccess input a
    (<*>) (Parser p1) (Parser p2) = Parser $ \input -> do 
        (ParserSuccess unparsed fparsed) <- p1 input
        (ParserSuccess unparsed' parsed') <- p2 unparsed 
        Right $ ParserSuccess unparsed' (fparsed parsed')

instance Alternative Parser where
    empty = Parser $ \_ ->  Left (ParserError "Empty")
    (Parser p1) <|> (Parser p2) = Parser f where f input = p1 input <> p2 input

parseSpan :: (Char -> Bool) -> Parser String
parseSpan f = Parser $ \input ->
    let (parsed, unparsed) = span f input in Right (ParserSuccess unparsed parsed)

parseChar :: Char -> Parser Char
parseChar x = Parser f 
    where 
        f (y:ys)
            | y == x = Right $ ParserSuccess ys y 
            | otherwise = Left $ ParserError [y]
        f [] = Left $ ParserError ""

parseString :: String -> Parser String
parseString s = Parser $ \input -> 
    case runParser (traverse parseChar s) input of
        Left (ParserError err) -> Left $ ParserError $ "Expected: " ++ s ++ " Found: " ++ err  
        result -> result

parseAny :: Parser String
parseAny = parseSpan (/= '"')

htmlString :: Parser HTMLValue
htmlString = fmap HTMLText $ parseChar '"' *> parseAny <* parseChar '"'

htmlText :: Parser HTMLValue
htmlText = undefined

htmlParser :: Parser HTMLValue
htmlParser = htmlString

runHtmlParser :: String -> Either ParserError (ParserSuccess HTMLValue)
runHtmlParser = runParser htmlParser

main :: IO ()
main = do
    input <- getLine
    print $ runHtmlParser input