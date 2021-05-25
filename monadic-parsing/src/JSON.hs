module JSON 
where
import Control.Applicative (Applicative(..), Alternative(..))
import Control.Monad       (Monad(..), when)
import Data.Char (isDigit, ord, digitToInt )
import Data.List
import Parser (Parser(..), parse, zero)
import Combinators(
  look, 
  sat, 
  char, string, spaces, 
  apply, token, symbol, 
  many0, many1, 
  sepBy0, sepBy1, takeUntil,
  chainl1,
  failure
  )

data Json = JsonNull
  | JsonBool Bool
  | JsonString String
  | JsonInteger Int
  | JsonDouble Double
  | JsonArray [Json]
  | JsonObject [JsonProperty]
  deriving (Eq, Show)

type JsonProperty = (String, Json)

json :: Parser Json
json = 
  jsonObject  <|>
  jsonArray   <|>
  jsonString  <|>
  jsonDouble  <|>
  jsonInteger <|>
  jsonBool    <|>
  jsonNull

jsonObject :: Parser Json
jsonObject = do {
  symbol "{";
  props <- jsonProperty `sepBy0` symbol ",";
  symbol "}";
  return (JsonObject props)
}
  where
  jsonProperty :: Parser JsonProperty
  jsonProperty = do {
    name <- fmap (\(JsonString s) -> s) jsonString;
    symbol ":";
    value <- json;
    return (name, value)
  }

jsonArray :: Parser Json
jsonArray = do {
  symbol "[";
  xs <- json `sepBy0` char ',';
  symbol "]";
  return (JsonArray xs)
}

jsonNull :: Parser Json
jsonNull = do { symbol "null"; return JsonNull }

jsonBool :: Parser Json
jsonBool = true <|> false
  where true = do { symbol "true"; return (JsonBool True) }
        false = do { symbol "false"; return (JsonBool False) }

jsonString :: Parser Json
jsonString = do { 
  symbol "\""; 
  str <- takeUntil '\"';
  symbol "\""; 
  return (JsonString str) 
}

jsonInteger :: Parser Json
jsonInteger = JsonInteger <$> integer

jsonDouble :: Parser Json
jsonDouble = do {
  wholePart <- fmap fromIntegral integer; 
  char '.';
  fractionalPart <- fmap asFracPt (many digit);
  return (JsonDouble (wholePart + fractionalPart))
}
  where asFracPt ds = sum [ fromIntegral d * (10 ** (-p))| (d, p) <- zip ds [1..] ]

digit :: Parser Int
digit = do {
  d <- sat isDigit;
  return (digitToInt d)
}

integer :: Parser Int
integer = do {
  spaces;
  s <- sign;
  d <- digitToInt <$> sat isDigit;
  if d == 0 
    then 
      return 0 
    else 
      do {
        ds <- many0 digit;
        return (s * asInt (d:ds));
      }
}
  where sign :: Parser Int
        sign = do { symbol "+"; return 1; }    <|>
               do { symbol "-"; return (-1); } <|>
               do { return 1; }
        asInt ds = sum [ d * (10^p) | (d, p) <- zip (reverse ds) [0..] ]