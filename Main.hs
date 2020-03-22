module Main where

import Control.Applicative
import Data.Char
import qualified Data.Map.Strict as Map

type HTMLAttributes = Map.Map String String

data HTMLValue = HTMLText String
    | HTMLSeq [HTMLValue]
    | HTMLField HTMLAttributes HTMLValue
    deriving (Show, Eq)

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
            | otherwise = Left $ ParserError $ "Expected: " ++ [x] ++ " Found: " ++ [y] 
        f [] = Left $ ParserError ""

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure [] 

ws :: Parser String
ws = parseSpan isSpace

parseString :: String -> Parser String
parseString s = Parser $ \input -> 
    case runParser (traverse parseChar s) input of
        Left (ParserError err) -> Left $ ParserError $ "Expected: " ++ s ++ " Found: " ++ err  
        result -> result

parseAny :: Parser String
parseAny = parseSpan (/= '"')

parseNotIn :: String -> Parser String
parseNotIn = foldl1 (<|>) . parsers
    where parsers = map (\c -> parseSpan (/= c))

stringLiteral :: Parser String
stringLiteral = parseChar '"' *> parseAny <* parseChar '"'

htmlText :: Parser HTMLValue
htmlText = HTMLText <$> stringLiteral

htmlAttributes :: Parser HTMLAttributes
htmlAttributes = Map.fromList <$> sepBy ws parseAttribute
    where
        parseAttribute = liftA2 (,) (stringLiteral <* parseChar '=') stringLiteral

htmlField :: Parser HTMLValue
htmlField = Parser (\input -> 
        case runParser (parseChar '<' *> htmlAttributes <* parseChar '>') input of
            (Left (ParserError e)) -> Left $ ParserError $ "AttributeParser: " ++ e
            (Right (ParserSuccess unparsed as)) -> do
                (ParserSuccess unparsed' children) <- runParser htmlValue unparsed
                Right $ ParserSuccess unparsed' $ HTMLField as children
    )

htmlValue :: Parser HTMLValue
htmlValue = htmlSeq

htmlSeq :: Parser HTMLValue
htmlSeq = HTMLSeq <$> Parser f
    where 
        f input 
            | input == "" = Right $ ParserSuccess "" []
            | otherwise = do
                (ParserSuccess unparsed parsed) <- runParser (many $ htmlField <|> htmlText) input
                case unparsed of 
                    "" -> Right $ ParserSuccess unparsed parsed 
                    _ -> Left $ ParserError $ "Could not parse " ++ unparsed

runHtmlParser :: String -> Either ParserError (ParserSuccess HTMLValue)
runHtmlParser = runParser htmlValue

main :: IO ()
main = do
    input <- getLine
    print $ runHtmlParser input