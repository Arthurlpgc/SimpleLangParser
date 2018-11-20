module Structure where
import Debug.Trace
type Location = Int
type Index = [String]
type Stack = [Int]

position :: String -> Index -> Location
position name [] = 0
position name (a:b) | a == name = 0
    | otherwise = (position name b) + 1

fetch :: Location -> Stack -> Int
fetch n (v:vs) | n == 0 = v
    | otherwise = fetch (n-1) vs

put :: Location -> Int -> Stack -> Stack
put 0 val [] = [val]
put 0 val (v:vs) = (val:vs)
put pos val (v:vs) = v:(put (pos-1) val vs)

putInd :: Location -> String -> Index -> Index
putInd 0 val [] = [val]
putInd 0 val (v:vs) = (val:vs)
putInd pos val (v:vs) = v:(putInd (pos-1) val vs)

addVariable :: String -> Int -> Stack -> Index -> (Stack, Index)
addVariable identifier value stack index = do
    let x = position identifier index;
    (put x value stack, putInd x identifier index)

    
data Exp = Constant Int
    | Variable String
    | Plus Exp Exp
    | Minus Exp Exp
    | Times Exp Exp
    | Div Exp Exp
    | Greater Exp Exp
    | Lesser Exp Exp
    | Equals Exp Exp
    deriving Show

data Com = Printe Exp
    | Declare String Exp Com
    | Assign String Exp
    | While Exp Com
    | If Exp Com Com
    | SeqV [Com]
    deriving Show

newtype M a = StOut (Stack -> (a, Stack, String))
unStOut (StOut f) = f

getfrom :: Location -> M Int
getfrom location = StOut $ \stack -> (fetch location stack, stack, "")

sumSt :: Exp -> Exp -> Index -> Int -> Stack -> (Int, Stack, String)
sumSt exp1 exp2 index mul stack = do
    let (a, stack1, _) = (unStOut (eval1 exp1 index)) stack
    let (b, stack2, _) = (unStOut (eval1 exp2 index)) stack1
    (a + (mul * b), stack2, "")

mulSt :: Exp -> Exp -> Index -> Stack -> (Int, Stack, String)
mulSt exp1 exp2 index stack = do
    let (a, stack1, _) = (unStOut (eval1 exp1 index)) stack
    let (b, stack2, _) = (unStOut (eval1 exp2 index)) stack1
    (a * b, stack2, "")

divSt :: Exp -> Exp -> Index -> Stack -> (Int, Stack, String)
divSt exp1 exp2 index stack = do
    let (a, stack1, _) = (unStOut (eval1 exp1 index)) stack
    let (b, stack2, _) = (unStOut (eval1 exp2 index)) stack1
    (quot a b, stack2, "")

relStH :: Int -> Int -> Int -> Int
relStH rel a b 
    | rel == 0 && a == b = 1
    | rel == 1 && a > b = 1
    | rel == -1 && a < b = 1
    | otherwise = 0

relSt :: Exp -> Exp -> Index -> Int -> Stack -> (Int, Stack, String)
relSt exp1 exp2 index rel stack = do
    let (a, stack1, _) = (unStOut (eval1 exp1 index)) stack
    let (b, stack2, _) = (unStOut (eval1 exp2 index)) stack1
    (relStH rel a b, stack2, "")

eval1 :: Exp -> Index -> M Int
eval1 exp index = case exp of
    Constant n -> StOut $ \stack -> (n, stack, "")
    Variable x -> (getfrom (position x index))
    Plus exp1 exp2 -> StOut (sumSt exp1 exp2 index 1)
    Minus exp1 exp2 -> StOut (sumSt exp1 exp2 index (-1))
    Times exp1 exp2 -> StOut (mulSt exp1 exp2 index)
    Div exp1 exp2 -> StOut (divSt exp1 exp2 index)
    Equals exp1 exp2 -> StOut (relSt exp1 exp2 index 0)
    Greater exp1 exp2 -> StOut (relSt exp1 exp2 index 1)
    Lesser exp1 exp2 -> StOut (relSt exp1 exp2 index (-1))

newtype M2 = StOut2 (Stack -> Index -> (Stack, Index, String))
unStOut2 (StOut2 f) = f

printExecHelper :: Exp -> Stack -> Index -> (Stack, Index, String)
printExecHelper exp stack index = do 
    let (result, a, b) = ((unStOut(eval1 exp index)) stack);
    (stack, index, show result ++ "\n")

printExec :: Exp -> M2
printExec exp = StOut2 $ printExecHelper exp

declrExec :: String -> Exp -> Com -> Stack -> Index -> (Stack, Index, String)
declrExec identifier exp com stack index = do
    let (value, _, output) = (unStOut (eval1 exp index)) stack;
    let (stack2, index2) = addVariable identifier value stack index;
    let (_, _, output) = (unStOut2 (exec com)) stack2 index2;
    (stack, index, output)

assignExec :: String -> Exp -> Stack -> Index -> (Stack, Index, String)
assignExec identifier exp stack index = do
    let (value, _, output) = (unStOut (eval1 exp index)) stack;
    let (stack2, index2) = addVariable identifier value stack index;
    (stack2, index2, output)

whileExecHelper :: Int -> Exp -> Com -> String -> Stack -> Index -> (Stack, Index, String)
whileExecHelper val exp com str stack index
    | val == 0 = (stack, index, str)
    | otherwise = do 
        let (stack2, index2, str2) = (unStOut2 (exec com)) stack index;
        whileExec exp com (str ++ str2) stack2 index2;

whileExec :: Exp -> Com -> String -> Stack -> Index -> (Stack, Index, String)
whileExec exp com str stack index = do
    let (value, _, _) = (unStOut (eval1 exp index)) stack;
    (whileExecHelper value exp com str stack index);    

ifExecHelper :: Int -> Com -> Com -> Stack -> Index -> (Stack, Index, String)
ifExecHelper val com1 com2 stack index
    | val == 0 = (unStOut2 (exec com2)) stack index
    | otherwise = (unStOut2 (exec com1)) stack index

ifExec :: Exp -> Com -> Com -> Stack -> Index -> (Stack, Index, String)
ifExec exp com1 com2 stack index = do
    let (value, _, _) = (unStOut (eval1 exp index)) stack;
    ifExecHelper value com1 com2 stack index

seqExec :: [Com] -> Stack -> Index -> (Stack, Index, String)
seqExec [] stack index = (stack, index, "")
seqExec (hd:tl) stack index = do
    let (stackH, indexH, strH) = (unStOut2 (exec hd)) stack index
    let (stackT, indexT, strT) = seqExec tl stackH indexH
    (stackT, indexT, strH ++ strT)

exec :: Com -> M2 
exec com = case com of
    Printe exp -> (printExec exp)
    Declare identifier exp com -> StOut2 $ (declrExec identifier exp com)
    Assign identifier exp -> StOut2 $ (assignExec identifier exp)
    While exp com -> StOut2 $ (whileExec exp com "")
    If exp com1 com2 -> StOut2 $ (ifExec exp com1 com2)
    SeqV coms -> StOut2 $ (seqExec coms)

    -- x= While (Greater (Constant 7) (Variable "ASD")) (Assign "ASD"(Constant 8))