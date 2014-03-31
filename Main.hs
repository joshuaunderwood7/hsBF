import System.Environment

bfmachine = ((repeat 0), 0, (repeat 0))
type BFmachine = ([Int], Int, [Int])
type BFprogram = (String, Char, String)
sampleProgram = ([], '+', "++[>+>+<<-]")

deref :: ([Int], Int, [Int]) -> Int
deref machine@(_,x,_) = x
increment machine@(prev, current, next) = (prev, (current + 1), next)
decrement machine@(prev, current, next) = (prev, (current - 1), next)
pointerLeft machine@(prev, current, next) = ((tail prev), (head prev), ([current] ++ next))
pointerRight machine@(prev, current, next) = (([current] ++ prev), (head next), (tail next))

programFWD program@(prev, current, []) = ((current : prev), '~', []) 
programFWD program@(prev, current, next) = ((current : prev), head next, tail next) 
programBCK program@([], current, next) = ([], '~', next) 
programBCK program@(prev, current, next) = (tail prev, head prev, (current : next)) 

currentSymbol :: BFprogram -> Char
currentSymbol program@(_, x, _) = x
endOfProgram program@(_,x,_)
    | x == '~'  = True
    | otherwise = False

printLocalMachine machine@(prev, current, next) = print $ (reverse (take 10 prev), current, take 10 next)

scanBCK :: Int -> BFprogram -> BFprogram
scanBCK n program
    | (currentSymbol program) == '[' && n == 0 = program
    | (currentSymbol program) == '[' && n /= 0 = scanBCK (n - 1) $ programBCK program
    | (currentSymbol program) == ']'           = scanBCK (n + 1) $ programBCK program
    | otherwise                                = scanBCK n $ programBCK program


scanFWD :: Int -> BFprogram -> BFprogram
scanFWD n program
    | (currentSymbol program) == ']' && n == 0 = program
    | (currentSymbol program) == ']' && n /= 0 = scanFWD (n - 1) $ programFWD program
    | (currentSymbol program) == '['           = scanFWD (n + 1) $ programFWD program
    | otherwise                                = scanFWD n $ programFWD program

execCommand machine command
    | command == '<' = pointerLeft machine
    | command == '>' = pointerRight machine
    | command == '+' = increment machine
    | command == '-' = decrement machine
    | otherwise      = machine

progCommand machine program
    | (currentSymbol program) == '[' && (deref machine) /= 0 = program
    | (currentSymbol program) == '[' && (deref machine) == 0 = scanFWD 0 $ programFWD program
    | (currentSymbol program) == ']' && (deref machine) /= 0 = scanBCK 0 $ programBCK program
    | (currentSymbol program) == ']' && (deref machine) == 0 = program
    | otherwise      = program
    
{--
ioCommand machine
    | command == '.' = getCharBF machine
    | command == ',' = putCharBF machine
    | otherwise      = return machine 
--}

loadStringToProgram :: String -> BFprogram
loadStringToProgram (x:xs) = ([], x, xs)

mainLoop machine program 
    | endOfProgram program = machine
    | otherwise = do
        let newmachine = execCommand machine (currentSymbol program)
        let newprogram = progCommand machine program
        mainLoop newmachine (programFWD newprogram)

main = do
    let program = "++This is my BF program+[>>++<+<-<->]+-[~]++"
--    let program = "++[->+++[-]<]+"
    print "--newprogram--"
    print program
    printLocalMachine $ mainLoop bfmachine (loadStringToProgram program)
