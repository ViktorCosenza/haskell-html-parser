module Main where

import Control.Applicative
import Data.Char
import qualified Data.Map.Strict as Map

type HTMLAttributes = Map.Map String String

data HTMLValue = HTMLText String
    | HTMLSeq [HTMLValue]
    | HTMLField String HTMLAttributes HTMLValue
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

parseAny :: Parser String
parseAny = parseSpan (/= '"')

notEmpty :: (Eq (f a), Alternative f) => Parser (f a) -> Parser (f a) 
notEmpty p = Parser $ \input -> do
    (ParserSuccess unparsed parsed) <- runParser p input
    if empty == parsed
        then Left $ ParserError "Must not be empty" 
        else Right $ ParserSuccess unparsed parsed 

parseIn :: String -> Parser String
parseIn = foldl1 (<|>) . parsers
    where parsers = map (\c -> parseSpan (== c))
   
parseNotIn :: String -> Parser String
parseNotIn = foldl1 (<|>) . parsers
    where parsers = map (\c -> parseSpan (/= c))

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

parseName :: Parser String
parseName = parseSpan isAlpha

stringLiteral :: Parser String
stringLiteral = parseChar '"' *> parseAny <* parseChar '"'


htmlText :: Parser HTMLValue
htmlText = HTMLText <$> stringLiteral

htmlAttributes :: Parser (String, HTMLAttributes)
htmlAttributes = liftA2 (,) (parseName <* ws) (Map.fromList <$> sepBy ws parseAttribute)
    where
        parseAttribute = liftA2 (,) (ws *> parseName <* ws <* parseChar '=') (ws *> stringLiteral)

htmlField :: Parser HTMLValue
htmlField = Parser (\input -> 
        case runParser (ws *> parseChar '<' *> ws *> ( htmlAttributes <* parseChar '>' )) input of
            (Left (ParserError e)) -> Left $ ParserError $ "AttributeParser: " ++ e
            (Right (ParserSuccess unparsed (name, as))) -> do
                (ParserSuccess unparsed' children) <- runParser htmlValue unparsed
                Right $ ParserSuccess unparsed' $ HTMLField name as children
    )


htmlValue :: Parser HTMLValue
htmlValue = HTMLSeq <$> Parser f
    where 
        f input 
            | input == "" = Right $ ParserSuccess "" []
            | otherwise = do
                (ParserSuccess unparsed parsed) <- runParser ((many (ws *>htmlField <|> htmlText)) <* ws) input
                case unparsed of 
                    "" -> Right $ ParserSuccess unparsed parsed 
                    _ -> Left $ ParserError $ "Parsed: " ++ show parsed ++  " Unparsed: " ++ unparsed

runHtmlParser :: String -> Either ParserError (ParserSuccess HTMLValue)
runHtmlParser = runParser htmlValue

parseFile :: FilePath -> IO ()
parseFile f = do
    content <- readFile f
    print $ runHtmlParser content

main :: IO ()
main = do
    input <- getLine
    print $ runHtmlParser input