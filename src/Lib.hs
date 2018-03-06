{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Lib where

import Prelude hiding (lex)
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Text.Read (readMaybe)

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
  deriving (Show,Eq)

type LispM = MaybeT (State [String])
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
  if s == h
  then fail ""
  else return ()

lexer :: String -> [String]
lexer [] = []
lexer (' ':ss) = lexer ss
lexer ('(':ss) = "(" : lexer ss
lexer (')':ss) = ")" : lexer ss
lexer (c:ss) = let (r:rs) = lexer ss in if "(" == r || ")" == r then [c]:r:rs else (c:r):rs

runParser :: String -> Maybe Exp
runParser s = evalState (runMaybeT parseExp) (lexer s)

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
            fail ""

isVar :: String -> Bool
isVar s = (readMaybe s :: Maybe Int) == Nothing

parseExpList :: LispM [Exp]
parseExpList = do
  h <- getOne
  if h == ")" 
  then return []
  else do
    e <- parseExp
    es <- parseExpList
    return (e:es)

