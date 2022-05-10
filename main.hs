import System.Environment
-------------------TREE---------------------------------------

data Operator = Add | Sub | Mul | Div | Pow | Root | Sin | Cos | Tg | Exp | Ln | Not

instance Show Operator where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Pow = "^"
    show Root = "root"
    show Sin = "sin"
    show Cos = "cos"
    show Tg = "tg"
    show Exp = "exp"
    show Ln = "ln"
    show Not = "-"

type Function = (Double, [String], [String])

fstF :: Function -> Double
fstF (f, _, _) = f
sndF :: Function -> [String]
sndF (_, s, _) = s
thdF :: Function -> [String]
thdF (_, _, t) = t

data Data = Op Operator | Func ([Function], [Function]) | Var Char

instance Show Data where
    show (Op op) = show op
    show (Func (a, b)) = "(" ++ printFunc a ++ "|" ++ printFunc b ++ ")"
    show (Var v) = [v]

data Tree = Empty | Node Data Tree Tree

instance Show Tree where
    show Empty = []
    show (Node a l r) = (show l) ++ " " ++ (show a) ++ " " ++ (show r)

deleteSpaces :: String -> String
deleteSpaces = filter (/=' ')

readDouble :: String -> Double
readDouble = read

findComma :: (String, Int, Int) -> Int
findComma ([], _, _) = 0
findComma (('(':xs), l, r) = findComma (xs, l + 1, r) + 1
findComma ((')':xs), l, r) = findComma (xs, l, r + 1) + 1
findComma ((x:xs), l, r)    | x == ',' && l == r + 1 =  0
                            | otherwise = findComma (xs, l, r) + 1

checkBracket :: (Int, Int) -> String -> Bool
checkBracket (_, _) [] = False
checkBracket (l, r) [x] = x == ')' && l == (r + 1)
checkBracket (l, r) ('(':xs) = l > r && checkBracket (l + 1, r) xs
checkBracket (l, r) (')':xs) = l > r && checkBracket (l, r + 1) xs
checkBracket (l, r) (x:xs) = l > r && checkBracket (l, r) xs

bee eq = (checkBracket (1, 0) . tail) eq

bracketSyntax eq = head eq == '(' && checkBracket (1, 0) (tail eq)
rootSyntax eq = head eq == 'r' && eq !! 1 == 'o' && eq !! 2 == 'o' && eq !! 3 == 't'
sinSyntax eq = head eq == 's' && eq !! 1 == 'i' && eq !! 2 == 'n'
cosSyntax eq = head eq == 'c' && eq !! 1 == 'o' && eq !! 2 == 's'
tgSyntax eq = head eq == 't' && eq !! 1 == 'g'
expSyntax eq = head eq == 'e' && eq !! 1 == 'x' && eq !! 2 == 'p'
lnSyntax eq = head eq == 'l' && eq !! 1 == 'n'
notSyntax eq = head eq == '-'

numberSyntax ([], _) = True
numberSyntax (('.':xs), True) = False
numberSyntax (('.':xs), False) = numberSyntax (xs, True)
numberSyntax ((x:xs), dot)  | x >= '0' && x <= '9' = numberSyntax (xs, dot)
                            | otherwise = False
                            
opIndex :: String -> Int -> Int
opIndex eq x = length eq - x - 1

parseLast :: (String, Int, Int, Int, Int, Int) -> Int
parseLast ([], _, pos, _, _, _) = pos
parseLast (('(':xs), prio, pos, curr, l, r) = parseLast (xs, prio, pos, curr + 1, l + 1, r) 
parseLast ((')':xs), prio, pos, curr, l, r) = parseLast (xs, prio, pos, curr + 1, l, r + 1)
parseLast ((x:xs), prio, pos, curr, l, r)   | x == '^' && l == r && prio < 1 = parseLast (xs, 1, curr, curr + 1, l, r)
                                            | (x == '*' || x == '/') && l == r && prio < 2 = parseLast (xs, 2, curr, curr + 1, l, r)
                                            | (x == '+' || x == '-') && l == r && prio < 3 = curr
                                            | otherwise = parseLast (xs, prio, pos, curr + 1, l, r)

parseEq :: String -> Tree
parseEq eq  | bracketSyntax eq = parseEq . init $ tail eq
            | length eq == 1 && head eq >= 'a' && head eq <= 'z' = Node (Var $ head eq) Empty Empty
            | operator == '+' = Node (Op Add) (parseEq $ take operatorIndex eq) (parseEq $ drop (operatorIndex + 1) eq)
            | operator == '-' && operatorIndex /= 0 = Node (Op Sub) (parseEq $ take operatorIndex eq) (parseEq $ drop (operatorIndex + 1) eq)
            | operator == '*' = Node (Op Mul) (parseEq $ take operatorIndex eq) (parseEq $ drop (operatorIndex + 1) eq)
            | operator == '/' = Node (Op Div) (parseEq $ take operatorIndex eq) (parseEq $ drop (operatorIndex + 1) eq)
            | operator == '^' = Node (Op Pow) (parseEq $ take operatorIndex eq) (parseEq $ drop (operatorIndex + 1) eq)
            | rootSyntax eq = Node (Op Root) (parseEq . drop 5 $ take commaIndex eq) (parseEq . init $ drop (commaIndex + 1) eq)
            | sinSyntax eq = Node (Op Sin) (parseEq . drop 4 $ take (commaIndex - 1) eq) Empty
            | cosSyntax eq = Node (Op Cos) (parseEq . drop 4 $ take (commaIndex - 1) eq) Empty
            | tgSyntax eq = Node (Op Tg) (parseEq . drop 3 $ take (commaIndex - 1) eq) Empty
            | expSyntax eq = Node (Op Exp) (parseEq . drop 4 $ take (commaIndex - 1) eq) Empty
            | lnSyntax eq = Node (Op Ln) (parseEq . drop 3 $ take (commaIndex - 1) eq) Empty
            | notSyntax eq = Node (Op Not) (parseEq $ drop 1 eq) Empty
            | numberSyntax (eq, False) = Node (Func ([(readDouble eq, [], [])], [(0, [], [])])) Empty Empty
            | otherwise = Empty
                where
                    commaIndex = findComma (eq, 0, 0)
                    operatorIndex = opIndex eq $ parseLast (reverse eq, 0, 0, 0, 0, 0)
                    operator = eq !! operatorIndex

makeTree :: String -> Tree
makeTree = parseEq . deleteSpaces

specTree :: Char -> Tree -> Tree
specTree _ Empty = Empty
specTree x (Node (Op op) l r) = Node (Op op) (specTree x l) (specTree x r)
specTree x (Node (Func (a, b)) l r) = Node (Func (a, b)) (specTree x l) (specTree x r)
specTree x (Node (Var v) l r)   | v == x = Node (Func ([(1.0, [[v]], [])], [(1.0, [], [])])) (specTree x l) (specTree x r)
                                | otherwise = Node (Func ([(1.0, [[v]], [])], [])) (specTree x l) (specTree x r)

---------------------PRINT-----------------------------------------------------------

filterEmpt :: [Function] -> [Function]
filterEmpt = filter ((/=0.0) . fstF)

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y <= x] ++ [x] ++ quicksort [y | y <- xs, y > x]

denomsort :: [Function] -> [Function]
denomsort [] = []
denomsort (x:xs) = denomsort [y | y <- xs, comparator x y] ++ [x] ++ denomsort [y | y <- xs, not $ comparator x y]
    where
        comparator x y  | thdF x == thdF y = x >= y
                        | otherwise = thdF x >= thdF y



makePower :: [String] -> String
makePower [] = []
makePower list = drop 1 $ retFst $ foldl folder ([], head list, 1) (tail list ++ [" "])
    where
        folder (acc, [den], c) x = if x == [den] then (acc, [den], c + 1) else if c /= 1 then (acc ++ "*" ++ [den] ++ "^" ++ show c, x, 1) else (acc ++ "*" ++ [den], x, 1)
        folder (acc, den, c) x = if x == den then (acc, den, c + 1) else if c /= 1 then (acc ++ "*" ++ shouldGetBracket den ++ "^" ++ show c, x, 1) else (acc ++ "*" ++ den, x, 1)
        shouldGetBracket x = if bracketSyntax x then x else "(" ++ x ++ ")"
        retFst (f, _, _) = f

mergeFunc (acc, [], num) x = (acc ++ [(1.0, [showAll num], [])], thdF x, [(fstF x, sndF x, [])])
mergeFunc (acc, den, num) x | thdF x == den && fstF x /= 0.0 = (acc, den, addF num [(fstF x, sndF x, [])])
                            | length num == 1 = (acc ++ [(1.0, [showAll num], den)], thdF x, [(fstF x, sndF x, [])])
                            | otherwise = (acc ++ [(1.0, ["(" ++ showAll num ++ ")"], den)], thdF x, [(fstF x, sndF x, [])])

mergeOnDenom :: [Function] -> [Function]
mergeOnDenom func   | null sorted = []
                    | otherwise = getFst $ foldl mergeFunc ([], thdF first, [(fstF first, sndF first, [])]) (tail sorted ++ [(0.0, [], [])])
    where
        first = head sorted
        sorted = denomsort $ filterEmpt func
        getFst (x, _, _) = x

printDenom :: [String] -> String
printDenom [] = []
printDenom [d] = '/' : d
printDenom d = "/(" ++ makePower d ++ ")"

showF :: Function -> String
showF (0.0, _, _) = []
showF f = sign ++ value (fstF f) ++ connector (fstF f) ++ makePower (sndF f) ++ printDenom (thdF f)
    where
        hasNum = not (null (sndF f))
        intValue = round (fstF f)
        isInt = fstF f == fromInteger intValue
        connector (-1.0) = []
        connector 1.0 = []
        connector _ = if hasNum then "*" else []
        value 1.0 = if not hasNum then "1" else []
        value (-1.0) = if not hasNum then "-1" else []
        value _ = if isInt then show intValue else show (fstF f)
        sign    | fstF f > 0.0 = "+" 
                | hasNum && fstF f == (-1.0) = "-"
                | otherwise = []


showAll :: [Function] -> String
showAll [] = []
showAll f   | head parsed == '+' = drop 1 parsed
            | head parsed == '-' = "(" ++ parsed ++ ")"
            | otherwise = parsed
    where
        parsed = concat . quicksort $ map showF (filterEmpt f)
        

printFunc :: [Function] -> String
printFunc f = if null result then "0" else result
    where
        result = showAll $ mergeOnDenom f

------------------------CALCULATE----------------------------------------------------------------

addOrConcat [] (0.0, _, _) = []
addOrConcat [] r = [r]
addOrConcat (l:lx) r    | sndF l == sndF r && thdF l == thdF r = if sum == 0.0 then lx else (sum, sndF l, thdF l) : lx
                        | otherwise = l : addOrConcat lx r
    where
        sum = fstF l + fstF r

addF l r = foldl addOrConcat l r

addFnD (lf, ld) (rf, rd) = (addF lf rf, addF ld rd)


negF = map (\x -> (negate $ fstF x, sndF x, thdF x))

negFnD (lf, ld) = (negF lf, negF ld)


subFnD l r = addFnD l (negFnD r) 


reduce :: Function -> Function -> (Function, Function)
reduce l r = ((fstF l, fst newND, fst newDN), (fstF r, snd newDN, snd newND))
    where
        newND = deleteIntersection (sndF l) (thdF r)
        newDN = deleteIntersection (thdF l) (sndF r)

deleteIntersection :: Eq a => [a] -> [a] -> ([a], [a])
deleteIntersection l r = foldl (\(x, y) v -> if elem v x then (remove v x, y) else (x, v:y)) (l, []) r
    where
        remove _ [] = []
        remove x (y:ys) | x == y = ys
                        | otherwise = y : remove x ys

multiply :: (Function, Function) -> Function
multiply ((0.0, _, _), (_, _, _)) = (0.0, [], [])
multiply ((_, _, _), (0.0, _, _)) = (0.0, [], [])
multiply (l, r) = (fstF l * fstF r, quicksort $ sndF l ++ sndF r, quicksort $ thdF l ++ thdF r)     

mulF l r = foldl addOrConcat [] (filterEmpt [multiply $ reduce x y | x <- l, y <- r, fstF x /= 0.0, fstF y /= 0.0])

mulFnd (lf, ld) (rf, rd) = (mulF lf rf, addF (mulF lf rd) (mulF ld rf))


deleteSame :: [Function] -> [Function] -> [Function]
deleteSame d n = if null $ fst deleted then snd deleted else n
    where
        deleted = deleteIntersection d n

divF :: [Function] -> [Function] -> [Function]
divF [] _ = []
divF _ [] = []
divF _ [(0.0, _, _)] = []
divF l [r] = mulF l [(1.0 / fstF r, thdF r, sndF r)]
divF l r    | null newL = [(1.0, [], [])]
            | length l /= length newL = addF [(1.0, [], [])] (divF newL [(1.0, ["(" ++ rValue ++ ")"], [])])
            | otherwise = divF newL [(1.0, ["(" ++ rValue ++ ")"], [])]
    where
        newL = deleteSame r l
        rValue = printFunc r

divFnD (lf, ld) (rf, rd) = (divF lf rf, divF (divF (addF (mulF rf ld) (negF $ mulF lf rd)) rf) rf)


showVal f@([(_, [], [])]) =  printFunc f
showVal f@([(1.0, [n], [])]) = printFunc f
showVal f@([(-1.0, [n], [])]) = "(" ++ printFunc f ++ ")"
showVal f = if bracketSyntax printed then printed else "(" ++ printed ++ ")"
    where
        printed = printFunc f


powF :: [Function] -> [Function] -> [Function]
powF [] _ = []
powF _ [] = []
powF [(0.0, _, _)] [(0.0, _, _)] = []
powF [(0.0, _, _)] _ = [(0.0, [], [])]
powF _ [(0.0, _, _)] = [(1.0, [], [])]
powF [(x, [], [])] [(y, [], [])] = [(x ** y, [], [])]
powF l r = [(1.0, [showVal l ++ showPow], [])]
    where
        pow = showVal r
        showPow = if pow == "1" then [] else "^" ++ pow

powFnD (lf, ld) (rf, rd) = (powF lf rf, mulF (powF lf (addF rf [(-1.0, [], [])])) (addF (mulF rf ld) (mulF (lnF lf) (mulF lf rd))))


rootF :: [Function] -> [Function] -> [Function]
rootF [] _ = []
rootF _ [] = []
rootF _ [(0.0, _, _)] = []
rootF [(0.0, _, _)] _ = [(0.0, [], [])]
rootF [(x, [], [])] [(y, [], [])] = [(x ** (1 / y), [], [])]
rootF l r = [(1.0, ["root(" ++ printFunc l ++ "," ++ printFunc r ++ ")"], [])]

rootFnD (lf, ld) (rf, rd) = (rootF lf rf, divF (divF (mulF (powF lf (addF (divF [(1.0, [], [])] rf) [(-1.0, [], [])])) (addF (mulF rf ld) (negF $ mulF (lnF lf) (mulF lf rd)))) rf) rf)


sinF [] = []
sinF [(x, [], [])] = [(sin x, [], [])]
sinF f = [(1.0, ["sin(" ++ printFunc f ++ ")"], [])]

sinFnD (lf, ld) = (sinF lf, mulF ld (cosF lf))


cosF [] = []
cosF [(x, [], [])] = [(cos x, [], [])]
cosF f = [(1.0, ["cos(" ++ printFunc f ++ ")"], [])]

cosFnD (lf, ld) = (cosF lf, negF $ mulF ld (sinF lf))


tgF [] = []
tgF [(x, [], [])] = [(tan x, [], [])]
tgF f = [(1.0, ["tg(" ++ printFunc f ++ ")"], [])]

tgFnD (lf, ld) = (lnF lf, divF (divF ld (cosF lf)) (cosF lf))


expF [] = []
expF [(x, [], [])] = [(exp x, [], [])]
expF f = [(1.0, ["exp(" ++ printFunc f ++ ")"], [])]

expFnD (lf, ld) = (expF lf, mulF (expF lf) ld) 


lnF [] = []
lnF [(0.0, [], [])] = []
lnF [(x, [], [])] = [(log x, [], [])]
lnF f = [(1.0, ["ln(" ++ printFunc f ++ ")"], [])]

lnFnD (lf, ld) = (lnF lf, divF ld lf)



computeTree :: Tree -> ([Function], [Function])
computeTree Empty = ([], [])
computeTree (Node (Var _) _ _) = ([], [])
computeTree (Node (Func x) l r) = x
computeTree (Node (Op Add) l r) = addFnD (computeTree l) (computeTree r)
computeTree (Node (Op Not) l _) = negFnD (computeTree l)
computeTree (Node (Op Sub) l r) = subFnD (computeTree l) (computeTree r)
computeTree (Node (Op Mul) l r) = mulFnd (computeTree l) (computeTree r)
computeTree (Node (Op Div) l r) = divFnD (computeTree l) (computeTree r)
computeTree (Node (Op Pow) l r) = powFnD (computeTree l) (computeTree r)
computeTree (Node (Op Root) l r) = rootFnD (computeTree l) (computeTree r)
computeTree (Node (Op Sin) l _) = sinFnD (computeTree l)
computeTree (Node (Op Cos) l _) = cosFnD (computeTree l)
computeTree (Node (Op Tg) l _) = tgFnD (computeTree l)
computeTree (Node (Op Exp) l _) = expFnD (computeTree l)
computeTree (Node (Op Ln) l _) = lnFnD (computeTree l)

-----------------------------------------------------------------

printFunction (l, r) = printFunc l ++ " | " ++ printFunc r
printDerivative (_, r) = if bracketSyntax derivative then init $ tail derivative else derivative
    where
        derivative = printFunc r

computeDerivatives :: String -> [String]
computeDerivatives eq = [printDerivative . computeTree $ specTree 'x' baseTree, printDerivative . computeTree $ specTree 'y' baseTree, printDerivative . computeTree $ specTree 'z' baseTree]
    where
        baseTree = makeTree eq


main = do
    (arg:_) <- getArgs
    let derivatives = computeDerivatives arg 
    putStrLn $ "d/dx: " ++ derivatives !! 0
    putStrLn $ "d/dy: " ++ derivatives !! 1
    putStrLn $ "d/dz: " ++ derivatives !! 2


