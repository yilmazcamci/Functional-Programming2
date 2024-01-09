{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}

module Parser where

import Data.Char
import Data.List

import Control.Monad
import Control.Applicative

import Control.Monad.Trans

data StateIOMonad st a = SIOM (st -> IO (a, st))

data Parser a = P (String -> Maybe (a, String))
  deriving (Functor)


data StateMonadT st m a = SMT (st -> m (a, st))
  deriving (Functor)
removeSMT (SMT smt) = smt

instance Monad m => Applicative (StateMonadT st m) where
  pure :: Monad m => a -> StateMonadT st m a 
  pure x = SMT $ \st -> pure (x,st)
  (<*>) = ap

instance Monad m => Monad (StateMonadT st m) where
  (>>=) :: Monad m => StateMonadT st m a -> (a -> StateMonadT st m b) -> StateMonadT st m b
  m >>= f = SMT $ \st -> do (a,nst) <- (removeSMT m) st
                            removeSMT (f a) nst

-- lift :: Monad m => m a -> t m a
instance MonadTrans (StateMonadT st) where
  lift :: (Monad m) => m a -> StateMonadT st m a
  lift ma = SMT $ \st ->  do a <- ma; return (a, st)
  
  
instance MonadIO m => MonadIO (StateMonadT st m) where
  liftIO :: MonadIO m => IO a -> StateMonadT st m a
  liftIO  = lift . liftIO

type Bla a = StateMonadT () IO a

foo :: Bla ()
foo = do liftIO $ putStr "> "
         s <- liftIO $ getLine
         liftIO $ putStrLn ("Hello " ++ s)


parse :: Parser a -> (String -> Maybe (a, String))
parse (P parser) = parser


instance Applicative Parser where
  pure :: a -> Parser a 
  pure x = P (\inp -> pure (x,inp)) 

  (<*>) = ap

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P $ \st -> do (a,nst) <- (parse p) st
                          parse (f a) nst
  
-- p :: Parser a
-- f :: a -> Parser b



instance Alternative Parser where
  empty :: Parser a
  empty  = P $ \inp -> Nothing
  
  (<|>) :: Parser a -> Parser a -> Parser a
  pl <|> pr  = P $ \inp -> case parse pl inp of
                            Nothing -> parse pr inp
                            result  -> result                            



  
item :: Parser Char
item = P $ \inp ->
  case inp of
    []     -> Nothing
    (x:xs) -> Just (x, xs)


--


sat :: (Char -> Bool) -> Parser Char
sat p  = do it <- item
            if p it then pure it else empty

char :: Char -> Parser Char
char c = sat (== c)

string :: String -> Parser String
string [] = pure []  
string (x:xs) = do c <- char x
                   cs <- string xs
                   pure (c:cs)

ident :: Parser String
ident  =  (:) <$> sat isLower <*> many (sat isAlphaNum) 


digit :: Parser Char
digit = sat isDigit

nat :: Parser Integer
nat = read <$> (some digit)

int :: Parser Integer
int = nat <|> negate <$> (char '-' *> nat)

space :: Parser ()
space = many (sat isSpace) *> pure ()

--

symbol :: String -> Parser String
symbol symb = string symb <* space

identifier :: Parser String
identifier = ident <* space

integer :: Parser Integer
integer = int <* space

--
natList :: Parser [Integer]
natList = undefined
   
{-
  a parser for parsing function calls
      f(1,2,3)
  funCall = ident "(" argList ")"
  
  argList = arg { "," arg }
          | ""
          
  arg = integer
-}

funCall :: Parser (String,[Integer])
funCall = (,) <$> identifier <*> (symbol "(" *> argList <* symbol ")") 

argList :: Parser [Integer]
argList = ((:) <$> arg <*> many (symbol "," *> arg)) <|> pure []   

arg :: Parser Integer
arg = integer

atMost :: Int -> Parser a -> Parser [a]
atMost n p 
   | n > 0     = (:) <$> p <*> atMost (n-1) p <|> pure []
   | otherwise = pure []

nTimes :: Int -> Parser a -> Parser [a]
nTimes = replicateM

between :: (Int,Int) -> Parser a -> Parser [a]
between (n,m) p = (++) <$> nTimes n p <*> atMost (m-n) p


