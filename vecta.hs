module Vecta where
import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language( haskellStyle )
import Text.Parsec.Language (haskellDef)
import Control.Monad.Identity

lexer :: TokenParser ()
lexer = makeTokenParser haskellDef

whiteSpace= P.whiteSpace lexer
lexeme = P.lexeme lexer
symbol = P.symbol lexer
natural = P.natural lexer
parens = P.parens lexer
semi = P.semi lexer
videntifier = P.identifier lexer
vreserved = P.reserved lexer
vreservedOp = P.reservedOp lexer
vcomma = P.comma lexer

langdef :: P.GenLanguageDef String u Identity
langdef = P.LanguageDef
            { P.commentStart = "/*"
            , P.commentEnd = "*/"
            , P.commentLine = "//"
            , P.nestedComments = True
            , P.identStart = lower
            , P.identLetter = alphaNum
            , P.opStart = oneOf ":!#$%&*+./<=>?@\\^|-~"
            , P.opLetter = char '>'
            , P.reservedOpNames = [",", "|"]
            , P.reservedNames = ["fn", "do"]
            , P.caseSensitive = True
            }

vectaFormalParam :: Parser ()
vectaFormalParam = do
                    videntifier
                    char ':'
                    videntifier

vectaFormalParams :: Parser ()
vectaFormalParams = do
                      sepBy vectaFormalParam (vcomma)

vectaFundef :: Parser ()
vectaFundef = do
                vreserved "fn"
                fun_name <- videntifier
                char '('

                char ')'  
                spaces
                vreservedOp "->"
                return ()