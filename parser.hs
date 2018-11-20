import Text.ParserCombinators.Parsec
import Structure

constantExprParse :: GenParser Char st Exp
constantExprParse = 
    do result <- many1 digit;
        return (Constant (read result))

variableExprParse :: GenParser Char st Exp
variableExprParse = 
    do result <- many1 letter;
        return (Variable (result))

sumExprParse :: GenParser Char st Exp
sumExprParse = 
    do expr1 <- termExprParse;
        many space;
        char '+';
        expr2 <- exprParse;
        return (Plus expr1 expr2)

subExprParse :: GenParser Char st Exp
subExprParse = 
    do expr1 <- termExprParse;
        many space;
        char '-';
        expr2 <- exprParse;
        return (Minus expr1 expr2)

mulExprParse :: GenParser Char st Exp
mulExprParse = 
    do expr1 <- factorExprParse;
        many space;
        char '*';
        expr2 <- termExprParse;
        return (Times expr1 expr2)

divExprParse :: GenParser Char st Exp
divExprParse = 
    do expr1 <- factorExprParse;
        many space;
        char '/';
        expr2 <- termExprParse;
        return (Div expr1 expr2)

parenthesisExprParse :: GenParser Char st Exp
parenthesisExprParse = 
    do char '(';
        result <- exprParse;
        many space;
        char ')';
        return result

factorExprParse :: GenParser Char st Exp
factorExprParse = 
    do many space;
        result <- parenthesisExprParse <|> constantExprParse <|> variableExprParse;
        return result

termExprParse :: GenParser Char st Exp
termExprParse = 
    do many space;
        result <- (try mulExprParse) <|> (try divExprParse) <|> factorExprParse;
        return result

exprParse :: GenParser Char st Exp
exprParse = 
    do many space;
        result <- (try sumExprParse) <|> (try subExprParse) <|> termExprParse;
        return result

lesserExprParse :: GenParser Char st Exp
lesserExprParse = 
    do expr1 <- exprParse;
        many space;
        char '<';
        expr2 <- relExprParse;
        return (Lesser expr1 expr2)

equalsExprParse :: GenParser Char st Exp
equalsExprParse = 
    do expr1 <- exprParse;
        many space;
        char '=';
        expr2 <- relExprParse;
        return (Equals expr1 expr2)

greaterExprParse :: GenParser Char st Exp
greaterExprParse = 
    do expr1 <- exprParse;
        many space;
        char '>';
        expr2 <- relExprParse;
        return (Greater expr1 expr2)

relExprParse :: GenParser Char st Exp
relExprParse = 
    do many space;
        result <- (try lesserExprParse) <|> (try equalsExprParse) <|> (try greaterExprParse) <|> exprParse;
        return result

printComParse :: GenParser Char st Com
printComParse = 
    do string "print";
        space;
        exp <- relExprParse;
        many space;
        char ';';
        return (Printe exp);

declareComParse :: GenParser Char st Com
declareComParse = 
    do string "declare";
        many1 space;
        id <- many1 letter;
        many space;
        char '=';
        exp <- relExprParse;
        many1 space;
        string "in";
        space;
        com <- comParse;
        return (Declare id exp com);

assignComParse :: GenParser Char st Com
assignComParse =  
    do id <- many1 letter;
        many space;
        string ":=";
        exp <- relExprParse;
        many space;
        char ';';
        return (Assign id exp);

whileComParse :: GenParser Char st Com
whileComParse = 
    do string "while";
        exp <- relExprParse;
        many1 space;
        string "do";
        space;
        com <- comParse;
        return (While exp com);

ifComParse :: GenParser Char st Com
ifComParse = 
    do string "if";
        exp <- relExprParse;
        many1 space;
        string "then";
        space;
        com1 <- comParse;
        string "else";
        space;
        com2 <- comParse;
        return (If exp com1 com2);

seqParse :: GenParser Char st Com
seqParse =
    do char '{';
        coms <- many comParse;
        many space;
        char '}';
        return (SeqV coms);

comParse :: GenParser Char st Com
comParse =
    do many space;
        result <- (try printComParse) <|> (try declareComParse) <|> (try whileComParse) <|> (try ifComParse) <|> (try seqParse) <|> assignComParse;
        many space;
        return result;

file :: GenParser Char st [Com]
file = 
    do result <- many1 comParse;
       eof;
       return result;

runManyCom :: [Com] -> Stack -> Index -> IO ()
runManyCom [] stack index = do return ()
runManyCom (hd:tl) stack index = do 
    let (stack2, index2, ret) = ((unStOut2 (exec hd)) stack index)
    putStr ret;
    runManyCom tl stack2 index2;

run :: Either ParseError [Com] -> Int -> IO ()
run entry opt
    | opt == 0 = case entry of
            Left err -> do{ putStr "parse error at "
                            ; print err
                            }
            Right x  -> runManyCom x [] []
    | opt == 1 = case entry of
        Left err -> do{ putStr "parse error at "
                        ; print err
                        }
        Right x  -> print x

main :: IO ()
main = do 
    fleText <- readFile "code2.art";
    let out = parse file "" fleText;
    run out 1;
    