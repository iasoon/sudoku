{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts  #-}

module SAT
    ( SAT, Formula (T, F, Not, Or, And)
    , formula, newAtom
    , evalFormula
    , evalSAT
    , solveSAT
    ) where

import qualified Data.IntMap.Strict as M
import Data.Maybe (listToMaybe)
import Control.Monad.State
import Data.Foldable
import Control.Applicative (Alternative, (<$>), (<*>))
import Control.Monad.Identity

import Data.List (sortBy)
import Data.Function (on)

type AtomId = M.Key
type FormulaId = M.Key
--------------------------------------------------------------------------------
-- Formula data type

data Formula = Atom { atomId :: AtomId }
                 | T | F
                 | Not Formula
                 | Or Formula Formula
                 | And Formula Formula
                 deriving (Show, Eq)

fromBool :: Bool -> Formula
fromBool True  = T
fromBool False = F

toBool :: Formula -> Maybe Bool
toBool T = Just $ True
toBool F = Just $ False
toBool _ = Nothing

simplify' :: Formula -> Formula
simplify' (Not T)   = F
simplify' (Not F)   = T
simplify' (Or T _)  = T
simplify' (Or _ T)  = T
simplify' (Or F p)  = p
simplify' (Or p F)  = p
simplify' (And T p) = p
simplify' (And p T) = p
simplify' (And F _) = F
simplify' (And _ F) = F
simplify' p         = p


simplify :: Formula -> Formula
simplify = simplify' . mapFormula simplify

atoms :: Formula -> [AtomId]
atoms a@(Atom _)  = [atomId a]
atoms (Not p)     = atoms p
atoms (Or p1 p2)  = atoms p1 ++ atoms p2
atoms (And p1 p2) = atoms p1 ++ atoms p2
atoms _           = []

mapAtoms :: (Formula -> Formula) -> Formula -> Formula
mapAtoms f pred = mapAtoms' pred
    where f' (Atom a) = f $ Atom a
          f' p        = p
          mapAtoms'   = f' . mapFormula mapAtoms'

mapMFormula :: (Monad m) => (Formula -> m Formula) -> Formula -> m Formula
mapMFormula f (Not p)     = Not <$> f p
mapMFormula f (Or p1 p2)  = Or  <$> f p1 <*> f p2
mapMFormula f (And p1 p2) = And <$> f p1 <*> f p2
mapMFormula f p           = return p

mapFormula :: (Formula -> Formula) -> Formula -> Formula
mapFormula f = runIdentity . mapMFormula (return . f)

replacePred :: Formula -> Formula -> Formula -> Formula
replacePred from to pred = replace pred
    where replace pred | pred == from = to
                       | otherwise    = mapFormula replace pred

--------------------------------------------------------------------------------
-- SAT

data AtomState = Det Bool | Undet
        { triggers :: [FormulaId], triggerCount :: Int}
    deriving (Show)

data SATState = SATState
    { satAtoms       :: M.IntMap AtomState
    , satFormulas  :: M.IntMap Formula
    , formulaInd   :: Int -- Number of assigned formulas, to avoid overlaps
    } deriving (Show)

isDet :: AtomState -> Bool
isDet (Det _) = True
isDet _       = False

fromDet :: AtomState -> Maybe Bool
fromDet (Det val) = Just val
fromDet _         = Nothing

newtype SAT a = SAT (StateT SATState [] a)
    deriving ( Applicative, Functor, Monad, Alternative, MonadState SATState)

evalSAT (SAT s) = evalStateT s $ SATState M.empty M.empty 0
simplifySAT :: Formula -> SAT Formula

branch :: [a] -> SAT a
branch = SAT . lift

simplifySAT pred = simplify <$> do
    atoms <- gets satAtoms
    return $ mapAtoms (replace atoms) pred
    where   replace atoms atom = maybe atom fromBool $
                M.lookup (atomId atom) atoms >>= fromDet

evalFormula :: Formula -> SAT (Maybe Bool)
evalFormula = fmap toBool . simplifySAT

--------------------------------------------------------------------------------
-- Accessors
modifyAtoms :: (M.IntMap AtomState -> M.IntMap AtomState) -> SAT ()
modifyAtoms f = modify $ \s -> s { satAtoms = f $ satAtoms s }

modifyFormulas :: (M.IntMap Formula -> M.IntMap Formula) -> SAT ()
modifyFormulas f = modify $ \s -> s { satFormulas = f $ satFormulas s }

lookupAtom :: AtomId -> SAT (Maybe AtomState)
lookupAtom key = gets $ M.lookup key . satAtoms

getAtom :: AtomId -> SAT (Maybe Bool)
getAtom atom = liftM (>>= fromDet) $ lookupAtom atom


--------------------------------------------------------------------------------
-- SAT Operations

newAtom :: SAT Formula
newAtom = do
    key <- gets $ nextKey . satAtoms
    modifyAtoms $ M.insert key $ Undet [] 0
    return Atom { atomId = key }
        where nextKey map | M.null map = 1
                          | otherwise  = (+1) . fst . M.findMax $ map

runTrigger :: AtomId -> Bool -> FormulaId -> SAT ()
runTrigger atom value trigger = do
    -- get simplified formula
    pred <- gets $ fmap rewrite . M.lookup trigger . satFormulas
    case pred of
        Nothing   -> return ()
        Just pred -> satisfy pred >>= updatePred
        where rewrite = simplify . replacePred (Atom atom) (fromBool value)
              updatePred = maybe
                        (modifyFormulas $ M.delete trigger)
                        (\p -> modifyFormulas $ M.insert trigger p)

determine :: AtomId -> Bool -> SAT ()
determine atom value = do
    Just atomState <- lookupAtom atom
    case atomState of
        Det val -> guard (val == value)
        Undet triggers _ -> do
            modifyAtoms $ M.insert atom (Det value)
            mapM_ (runTrigger atom value) triggers

-- try to satisfy a formula
satisfy :: Formula -> SAT (Maybe Formula)
satisfy T               = return Nothing
satisfy (Atom id)       = determine id True >> return Nothing
satisfy (Not (Atom id)) = determine id False >> return Nothing
satisfy pred            = return $ Just pred


storeFormula :: Formula -> SAT ()
storeFormula pred = do
    key <- gets $ (+1) . formulaInd
    modify $ \s -> s { formulaInd = key }
    modifyFormulas $ M.insert key pred
    -- add triggers to atoms in formula
    modifyAtoms $ \as -> foldr (addTrigger key) as (atoms pred)
        where   addTrigger key atom atomMap = M.adjust (f key) atom atomMap
                f t (Undet ts c) = Undet (t:ts) (c+1)

solveSAT :: SAT ()
solveSAT = do
    -- Trigger count always remains constant
    atoms <- gets $ map fst . sortByCount . unDetPairs . satAtoms
    solve atoms
    where   sortByCount = sortBy (compare `on` (triggerCount . snd))
            unDetPairs = filter (not . isDet . snd) . M.assocs
            -- We are not done while there are unsatisfied formulas left.
            unlessDone m = gets (not . null . satFormulas) >>= \p -> when p m
            solve atoms = unlessDone $ do
                store <- gets satAtoms
                let (a:as) = dropWhile (isDet . (store M.!)) atoms
                branch [True, False] >>= determine a
                solve as


-- convert to Negation Normal Form
nnf :: Formula -> Formula
nnf = mapFormula nnf . deMorgan

deMorgan :: Formula -> Formula
deMorgan (Not (And p1 p2)) = Or (Not p1) (Not p2)
deMorgan (Not (Or p1 p2))  = And (Not p1) (Not p2)
deMorgan p                 = p

tseitinAnds :: Formula -> SAT Formula
tseitinAnds form = mapMFormula tseitinAnds form >>= tseitinAnd
    where tseitinAnd (And a b) = do
            c <- newAtom
            formula $ Or c $ Or (Not a) (Not b)
            formula $ Or a $ Not c
            formula $ Or b $ Not c
            return c
          tseitinAnd p = return p

conjunctions :: Formula -> [Formula]
conjunctions (And a b) = conjunctions a ++ conjunctions b
conjunctions p         = [p]

--------------------------------------------------------------------------------
-- Helper functions
--

putFormula :: Formula -> SAT ()
putFormula formula = satisfy formula >>= whenJust storeFormula
    where whenJust = maybe (return ())

formula :: Formula -> SAT ()
formula form = simplifySAT form >>= rewrite >>= mapM_ putFormula
    where rewrite = mapM tseitinAnds . conjunctions . nnf
