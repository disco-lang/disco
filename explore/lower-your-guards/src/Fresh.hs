{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Fresh where

import Control.Monad.State
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Text as T

type Fresh = State (NE.NonEmpty Frame)

data Frame = Frame {fBound :: M.Map VarID (Maybe T.Text), fNextID :: Int}
  deriving (Show, Eq, Ord)

newtype VarID = Var Int
  deriving (Show, Eq, Ord)

blank :: NE.NonEmpty Frame
blank = Frame {fBound = M.empty, fNextID = 0} :| []

fresh :: Maybe T.Text -> Fresh VarID
fresh mName = do
  Frame {fBound = bound, fNextID = nextID} <- gets NE.head
  t <- gets NE.tail
  let h = Frame {fBound = M.insert (Var nextID) mName bound, fNextID = nextID + 1}
  put $ h :| t
  return (Var nextID)

enterScope :: Fresh ()
enterScope = do
  nextID <- gets (fNextID . NE.head)
  stack <- get
  put $ Frame {fBound = M.empty, fNextID = nextID} :| NE.toList stack

exitScope :: Fresh ()
exitScope = modify (NE.fromList . NE.tail)

performInNewScope :: Fresh a -> Fresh a
performInNewScope m = do
  enterScope
  a <- m
  exitScope
  return a
