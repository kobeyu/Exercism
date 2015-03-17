module Sgf(parseSgf) where
import Data.Text(pack,Text)
import Data.Tree(Tree(Node))
import Text.Parsec((<|>),many,many1,oneOf,char,anyChar,noneOf,between,choice,runParser)
import Text.Parsec.Text(Parser)
import Data.Functor((<$>))
import Data.Map(Map,fromList)

type SgfTree = Tree SgfNode
type SgfNode = Map Text [Text]

singleton :: Parser t -> Parser [t]
singleton = fmap (:[])

parens :: Parser t -> Parser t
parens = between (char '(') (char ')')

brackets :: Parser t -> Parser t
brackets = between (char '[') (char ']')

attrParser :: Parser (Text,[Text])
attrParser = do
    name <- many1 $ oneOf ['A'..'Z']
    attrs <- many . brackets . fmap concat . many $ choice
            [ oneOf "\n\t" >> return " "
            , char '\\' >> ((char '\n' >> return "") <|> singleton anyChar)
            , singleton $ noneOf "]"
            ]
    return (pack name,map pack attrs)

treeParser :: Parser SgfTree
treeParser = do
    _ <- char ';'
    props <- many attrParser
    children <- singleton treeParser <|> many (parens treeParser)
    return $ Node (fromList props) children

sgf :: Parser SgfTree
sgf = parens treeParser

parseSgf :: Text -> Maybe (Tree (Map Text [Text]))
parseSgf t = case runParser sgf () "test" t of
    Left _ -> Nothing
    Right x -> Just x


