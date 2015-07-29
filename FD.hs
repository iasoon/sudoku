-- Functions for finite domain problems.
module FD
    ( Var, newVar, equals, getValue, xor, all, none, allDifferent
    ) where

import SAT
import Control.Monad
import Data.Foldable hiding (all)
import Prelude hiding (all)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (catMaybes, listToMaybe)
import Control.Monad.State

data Var t = Var
    { domain :: M.Map t Formula }

equals :: (Ord t) => t -> Var t -> Maybe Formula
equals key = M.lookup key . domain

getValue :: (Ord t) => Var t -> SAT (Maybe t)
getValue = fmap (listToMaybe . catMaybes) . mapM getTrues . M.assocs . domain
    where getTrues (k, f) = fmap (>>= g k) $ evalFormula f
          g k b = guard b >> return k


newVar :: (Ord t, Foldable f) => f t -> SAT (Var t)
newVar ts = do
    dom <- foldrM insAtom M.empty ts
    formula . xor . M.elems $ dom
    return $ Var dom
        where insAtom key map = do
                a <- newAtom
                return $ M.insert key a map

xor :: [Formula] -> Formula
xor []     = F
xor (f:fs) = Or (And f (none fs)) (And (Not f) (xor fs))

all :: [Formula] -> Formula
all = foldr And T

none :: [Formula] -> Formula
none []     = T
none (f:fs) = And (Not f) $ none fs

allDifferent :: (Ord t) => [Var t] -> Formula
allDifferent vars = all . map onlyOneEquals . S.toList $ keys
    where keys = S.unions . map (M.keysSet . domain) $ vars
          onlyOneEquals k = xor . catMaybes . map (equals k) $ vars
