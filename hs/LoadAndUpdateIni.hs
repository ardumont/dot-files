module LoadAndUpdateIni(loadAndUpdateExtensionsWith) where

-- original source file in: https://github.com/ardumont/haskell-lab.git

import qualified Data.Map as Map
import Text.ParserCombinators.Parsec
import Data.Maybe
import System.Environment

type IniConfig = Map.Map String [(String, String)]

ident :: Parser String
ident = do c <- letter <|> char '_'
           cs <- many (letter <|> digit <|> char '_' )
           return (c:cs)
        <?> "Identifier"

comment :: Parser ()
comment = do char '#'
             skipMany (noneOf "\r\n")
           <?> "Comment"

eol :: Parser ()
eol = do try (many1 (oneOf "\r\n"))
         return ()
      <?> "eol"

item :: Parser (String, String)
item = do key <- ident
          skipMany space
          char '='
          skipMany space
          value <- manyTill anyChar (try eol <|> try comment <|> eof)
          return (key, rstrip value)
       where rstrip :: String -> String
             rstrip = id

section :: Parser (String, String)
section = do char '['
             value <- manyTill anyChar (char ']')
             return (value, "")
          <?> "Section"

line :: Parser (Maybe (String, String))
line = do skipMany space
          try (item >>= mb) <|> (section >>= mb)
       --    try (comment >> Nothing) <|> (section >>= mb) <|> (item >>= mb)
       where mb = return . Just

fileContent :: Parser [(String, String)]
fileContent = do linesOfFile <- many line
                 (return . catMaybes) linesOfFile

mapify :: [(String, String)] -> IniConfig
mapify xs =
  internalMapify xs "" Map.empty
  where internalMapify :: [(String, String)] -> String -> IniConfig -> IniConfig
        internalMapify [] _ m = m
        internalMapify (e@(key, val) : xss) oldKey m =
          if val == ""
          then internalMapify xss key (Map.insert key [] m)
          else internalMapify xss oldKey (Map.update (\l -> Just (l ++ [e])) oldKey m) -- not performant

stringify :: IniConfig -> String
stringify m = (unlines . map stringifyLine) $ Map.keys m
              where stringifyLine k = (stringifySection k) ++ (stringifyProperties (Map.lookup k m))
                    stringifyProperties Nothing = []
                    stringifyProperties (Just xs) = (unlines . map stringifyProperty) xs
                    stringifyProperty (k, v) = k ++ "=" ++ v
                    stringifySection k = "[" ++ k ++ "]\n"

fromString :: String -> IniConfig
fromString stringToParse =
  case (parse fileContent "" stringToParse) of
    Left _  -> Map.empty
    Right m -> mapify m

fromFilePath :: FilePath -> IO IniConfig
fromFilePath filePath =
  do stringToParse <- readFile filePath
     return $ fromString stringToParse

countProperties :: Ord k => k -> Map.Map k [a] -> Int
countProperties k m = case Map.lookup k m of Just l -> length l

setProperty :: Ord k => k -> a -> Map.Map k [a] -> Map.Map k [a]
setProperty k v m = Map.update (\l -> Just (l ++ [v])) k m

updateNewExtension :: Map.Map String [(String, t)] -> t -> Map.Map String [(String, t)]
updateNewExtension iniProperties newExtensionValue =
  let nbExtensions = countProperties "ExtensionDirs" iniProperties
      newExtension = ("Extension" ++ (show nbExtensions), newExtensionValue)
  in setProperty "ExtensionDirs" newExtension iniProperties

loadAndUpdateExtensionsWith inputFilePath inputExtension =
  fromFilePath inputFilePath >>= \iniProperties -> return $ updateNewExtension iniProperties inputExtension

main :: IO ()
main = do (inputFilePath:inputExtension:_) <- getArgs
          updatedIniProperties <- loadAndUpdateExtensionsWith inputFilePath inputExtension
          putStrLn $ stringify updatedIniProperties
