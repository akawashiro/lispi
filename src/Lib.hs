{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Lib where

import Prelude hiding (lex)
import Control.Monad.State
import Control.Monad.Trans.Either
import Text.Read (readMaybe)
import Data.List.Split

data Exp =
  Var String 
  | Num Int
  | Quote Exp
  | If [Exp]
  | Set Exp Exp
  | Def Exp Exp
  | Lam [Exp] Exp
  | Beg [Exp]
  | Proc [Exp]
  | Null
  deriving (Show,Eq)

type LispM = EitherT String (State [String])
isEnd :: LispM Bool
isEnd = (== []) `liftM` get
peekOne :: LispM String
peekOne = head `liftM` get
dropOne :: LispM ()
dropOne = do
  t <- tail `liftM` get
  put t
getOne :: LispM String
getOne = do
  s <- peekOne
  dropOne
  return s
addOne :: String -> LispM ()
addOne s = do
  ss <- get
  put (s:ss)
matchOne :: String -> LispM ()
matchOne s = do
  h <- getOne
  if s /= h
  then fail $ "cannot match " ++ s ++ " with " ++ h
  else return ()

lexer :: String -> [String]
lexer s = filter (/= "") $ splitOn " " (f s)
  where 
    f ('(':ss) = ' ':'(':' ':f ss
    f (')':ss) = ' ':')':' ':f ss
    f (c:ss) = c:f ss
    f [] = []

runParser :: String -> Either String Exp
runParser s = evalState (runEitherT parseExp) (lexer s)

ex1 = "x"
ex2 = "(begin (define x 10) x)"

-- testLisp :: String -> String
testLisp s = show $ (runParser s) >>= return . evalExp

lookupEnv :: Exp -> State [(Exp,Exp)] Exp
lookupEnv exp = do
  env <- get
  case env of
    [] -> return exp
    ((e1,e2):es) -> if e1 == exp 
      then return e2 
      else lookupEnv exp

setEnv :: Exp -> Exp -> [(Exp,Exp)] -> [(Exp,Exp)]
setEnv v e [] = []
setEnv v e ((vv,ee):r) = if v == vv then ((vv,e):r) else (vv,ee):setEnv v e r

evalExp :: Exp -> Exp
evalExp e = evalState (evalExp' e) []

evalExp' :: Exp -> State [(Exp,Exp)] Exp
evalExp' exp = case exp of
  Var v -> lookupEnv exp
  Num i -> return $ Num i
  Quote e -> return $ e
  If (t:c:a:[]) -> do
    tt <- evalExp' t
    if tt == Num 0 then evalExp' a else evalExp' c
  Set v e -> do
    env <- get
    put $ setEnv v e env
    return Null
  Def v e -> do
    env <- get
    put ((v,e):env)
    return Null
  Lam _ _ -> return exp
  Beg [e] -> evalExp' e
  Beg (e:es) -> evalExp' e >> evalExp' (Beg es)
  Proc ((Lam as b): gas) -> do
    env <- get
    put (zip as gas ++ env)
    evalExp' b
  Null -> return Null

parseExp :: LispM Exp
parseExp = do
  h <- getOne
  case h of
    "(" -> do
      e <- parseExp'
      matchOne ")"
      return e
    _ -> if isVar h then return $ Var h else return $ Num (read h :: Int)
  where
    parseExp' = do
      i <- getOne
      case i of
        "quote" -> do
          e <- parseExp
          return $ Quote e
        "if" -> do
          es <- parseExpList
          return $ If es
        "set!" -> do
          v <- parseExp
          e <- parseExp
          return $ Set v e
        "define" -> do
          v <- parseExp
          e <- parseExp
          return $ Def v e
        "lambda" -> do
          matchOne "("
          vs <- parseExpList
          matchOne ")"
          e <- parseExp
          return $ Lam vs e
        "begin" -> do
          es <- parseExpList
          return $ Beg es
        _ -> if isVar i 
          then do
            addOne i
            es <- parseExpList
            return $ Proc es
          else
            fail "Start of proc is number."

isVar :: String -> Bool
isVar s = (readMaybe s :: Maybe Int) == Nothing

parseExpList :: LispM [Exp]
parseExpList = do
  h <- peekOne
  if h == ")" 
  then return []
  else do
    e <- parseExp
    es <- parseExpList
    return (e:es)

