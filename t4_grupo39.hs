import Stack
import System.IO
import Data.List
import Data.Char

type Name = String 
type Arity = Int 
type Token = String 
type Macro = (Name, Arity, [Token])

type Value = Integer 
type Variable = (Name, Value)

type State = ([Variable], Stack Integer, String) 
type Program = ([Macro], State, [Token])

evalToken :: [Macro] -> State -> Token -> State
evalToken macros estado "" = estado
evalToken macros ([variaveis], pilha, saida) linha
                            | isNumber elemento == True   = evalToken 
                                                (macros) ([variaveis], (pilhaInserir (read primeira :: Integer) pilha), saida) (novaLinha)
                            | elemento          == '@'    = evalToken
                                                (macros) (mudaVariavel, pilhaPop pilha, saida) (novaLinha)
                            | elemento          == '$', macroAridade == 0 = evalToken
                                                            (macros) ([variaveis], pilha, saida) (tokensRetirados ++ (novaLinha))
                            | elemento          == '$', macroAridade > 0 = evalToken
                                                            (macros) ([variaveis], mandaUnsPops, saida) (trataTokens ++ (novaLinha))                                                                                
                            | primeira          == "."    = evalToken
                                                (macros) ([variaveis], pilhaPop pilha, (saida++(show (pilhaTopo pilha))++"\n")) (novaLinha)
                            | primeira          == ","    = evalToken
                                                (macros) ([variaveis], pilhaPop pilha, (saida++(show (pilhaTopo pilha))++" ")) (novaLinha)                                               
                            | primeira          == "+"    = evalToken
                                                (macros) ([variaveis], pilhaSoma pilha, saida) (novaLinha)
                            | primeira          == "-"    = evalToken 
                                                (macros) ([variaveis], pilhaSubt pilha, saida) (novaLinha)
                            | primeira          == "*"    = evalToken 
                                                (macros) ([variaveis], pilhaMult pilha, saida) (novaLinha)
                            | primeira          == "/"    = evalToken 
                                                (macros) ([variaveis], pilhaDivi pilha, saida) (novaLinha)
                            | primeira          == "%"    = evalToken 
                                                (macros) ([variaveis], pilhaMod pilha, saida) (novaLinha)                                                
                            | primeira          == ">"    = evalToken      
                                                (macros) ([variaveis], pilhaMaior pilha, saida) (novaLinha)
                            | primeira          == "<"    = evalToken      
                                                (macros) ([variaveis], pilhaMenor pilha, saida) (novaLinha)   
                            | primeira          == "="    = evalToken      
                                                (macros) ([variaveis], pilhaIgual pilha, saida) (novaLinha)
                            | primeira          == "|"    = evalToken      
                                                (macros) ([variaveis], pilhaOr pilha, saida) (novaLinha)              
                            | primeira          == "&"    = evalToken      
                                                (macros) ([variaveis], pilhaAnd pilha, saida) (novaLinha)                                                              
                            | primeira          == "!"    = evalToken      
                                                (macros) ([variaveis], pilhaDifer pilha, saida) (novaLinha)
                            | primeira          == "dup"  = evalToken 
                                                (macros) ([variaveis], pilhaDup pilha, saida) (novaLinha)
                            | primeira          == "pop"  = evalToken 
                                                (macros) ([variaveis], pilhaPop pilha, saida) (novaLinha) 
                            | primeira          == "nil"  = evalToken 
                                                (macros) ([variaveis], pilha, saida) (novaLinha)                                                
                            | primeira          == "swap" = evalToken 
                                                (macros) ([variaveis], pilhaSwap pilha, saida) (novaLinha)
                            | primeira          == "size" = evalToken 
                                                (macros) ([variaveis], pilhaSize pilha, saida) (novaLinha)
                            | (isPrefixOf "if:" primeira)   == True = evalToken 
                                                (macros) ([variaveis], pilhaPop pilha, saida) (novaLinha)
                            | (isPrefixOf "loop:" primeira) == True = evalToken 
                                                (macros) ([variaveis], pilhaPop pilha, saida) ((handleLoop (pilhaTopo pilha) (drop 5 primeira)) ++ (novaLinha))
                            | otherwise = evalToken 
                                        (macros) ([variaveis], pilhaInserir (getVar [variaveis] primeira) pilha, saida) (novaLinha)
                                            
                            where palavras = words linha
                                  primeira = head palavras
                                  elemento = head primeira
                                  novaLinha = unwords $ tail $ palavras
                                  tokensRetirados = unwords (getMacroTokens macros (tail primeira))
                                  macroAridade = getMacroArity macros (tail primeira)
                                  mandaUnsPops = popAridade macroAridade pilha
                                  trataTokens = handleTokens tokensRetirados macroAridade pilha
                                  mudaVariavel = (setVar [variaveis] (tail primeira) (pilhaTopo pilha))



eval :: Program -> State
eval (_, estado, []) = estado
eval (makros, estado, linhas)
                    | length [linhas] > 1 = eval (makros, (evalToken makros estado (head linhas)), tail linhas)
                    | otherwise           = eval (makros, (evalToken makros estado (head linhas)), tail linhas)

processData :: String -> IO State
processData x = return (eval programa)
                  where programa = ((processAux macros) , criaNovoEstado , (drop (numero+1) funcao))
                        funcao = tiramarcas $ tiracoments $ lines x
                        numero = read (funcao !! 0) :: Int
                        macros = (map words (drop 1 (take (numero+1) funcao)))
                        
criaNovoEstado :: State
criaNovoEstado = ([("oi",1)],pilhaVazia,"")


run :: FilePath -> IO ()   
run x = 
  do
    let entrada = x ++ ".txt"
    contents <- readFile entrada  
    resultado <- (processData contents)
    let final = tirarOutput resultado
    let saida = x ++ ".out.txt"
    writeFile saida (""++final)

    
getMacroTokens :: [Macro] -> Name -> [Token]
getMacroTokens [] _ = []
getMacroTokens ((nomeM, aridade, [elemento]):xs) nomeP
                                              | nomeP == nomeM = [elemento]
                                              | otherwise      = getMacroTokens xs nomeP
                                              
getMacroArity :: [Macro] -> Name -> Arity
getMacroArity [] _ = 0
getMacroArity ((nomeM, aridade, [elemento]):xs) nomeP
                                              | nomeP == nomeM = aridade
                                              | otherwise      = getMacroArity xs nomeP
                                              
getVar :: [Variable] -> Name -> Value
getVar [] _ = 0
getVar ((nomeV,valor):xs) nomeP
                          | nomeP == nomeV = valor
                          | otherwise      = getVar xs nomeP
                          
setVar :: [Variable] -> Name -> Value -> [Variable]
setVar ((nomeV,valorM):xs) nomeP valorP = (nomeP,valorP):((nomeV,valorM):xs)

--
-- Funcoes Auxiliares
--
tiracoments :: [String] -> [String]    
tiracoments (x:xs) = filter (not . (\x -> '#' `elem` x)) (x:xs)

tiramarcas :: [String] -> [String]
tiramarcas [] = []
tiramarcas (x:xs)
        | unwords (words x) == "" = tiramarcas xs
        | otherwise = unwords (words x):tiramarcas xs
        
processAux :: [[String]] -> [Macro]       
processAux [] = []
processAux (x:xs) = (x !! 0, read (x !! 1) :: Int , drop 2 x):processAux xs

handleAridade :: Arity -> Stack Integer -> [Integer]
handleAridade 1 pilha = [pilhaTopo pilha]
handleAridade x pilha = pilhaTopo pilha:handleAridade (x-1) (pilhaPop pilha)

popAridade :: Arity -> Stack Integer -> Stack Integer
popAridade 1 pilha = pilhaPop pilha
popAridade x pilha = popAridade (x-1) (pilhaPop pilha)

handleTokens :: Token -> Arity -> Stack Integer -> Token
handleTokens "" aridade pilha = ""
handleTokens toks aridade pilha
            | head lista == '_' =  show ((handleAridade aridade pilha) !! ((read (tail lista) :: Int) - 1)) ++ " " ++ (handleTokens (unwords (tail (words toks))) aridade pilha)
            | otherwise         = lista ++ " " ++ (handleTokens (unwords (tail (words toks))) aridade pilha)
            where lista = head (words toks)
            
tirarOutput :: State -> [Char]
tirarOutput (_,_,frase) = trim (frase)

trim :: [Char] -> [Char]
trim = dropWhileEnd isSpace . dropWhile isSpace

handleLoop :: Integer -> String -> String
handleLoop 0 _ = ""                              
handleLoop x token
        | x > 0 = handleLoop (x-1) token ++ " "  ++  show x ++ " " ++ token