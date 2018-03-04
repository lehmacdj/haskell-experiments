{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

module FTreeFlatten where

import Control.Lens hiding ((:<))
import Control.Monad.State
import Control.Arrow
import Control.Comonad.Cofree
import Control.Comonad

-- | e : Ptr t -> MEM(e) : t
data Type = IntTy | BoolTy | Ptr Type | Unknown

type Temp = Integer
type Label = Integer

-- | Operators for IR. These are the same as the operators used by the Java
-- interpreter.
data Operator
  = ADD | SUB | MUL | HMUL | DIV | MOD
  | AND | OR | XOR
  | LSHIFT | RSHIFT | ARSHIFT
  | EQ | NEQ | LT | GT | LEQ | GEQ

-- | Core types of nodes for IR expressions.
data E e
  = CONST Int
  | TEMP Temp
  | NAME Label
  | MEM e
  | OP Operator e e
  | CALL e [e]
  deriving (Functor)

-- | Impure IR expressions.
data EEff a
  = ERec a (E (EEff a))
  | ESEQ a (STree a) (EEff a)
  deriving (Functor)

instance Comonad EEff where
  extract (ERec a _) = a
  extract (ESEQ a _ _) = a
  duplicate e@(ERec _ e') = ERec e (duplicate <$> e')
  duplicate e@(ESEQ _ s e') = ESEQ e (deepDuplicate s) (duplicate e') where
    deepDuplicate (STip se) = STip $ duplicate <$> se
    deepDuplicate (SEQ ss) = SEQ $ deepDuplicate <$> ss

-- | Expressions that do not contain side effects.
-- * As an additional invariant CALL may not occur in EPure except where allowed
-- by the the definition of SFlat.
type EPure a = Cofree E a

-- | Core types of nodes for IR statements.
-- The first e that is an argument to move has the invariant that it can only
-- contain TEMP or MEM constructors.
data S e
  = MOVE e e
  | EXP e
  | JUMP e
  | CJUMP e Label Label
  | LABEL Label
  | RETURN
  deriving (Functor)

-- | Unflattened IR statements.
data STree a
  = STip (S (EEff a))
  | SEQ [STree a]
  deriving (Functor)

-- | Flattened/Lowered IR statements. There are a couple of additional
-- invariants.
-- * CALL is allowed as the first argument of MOVE
-- * The only argument of EXP that is allowed is CALL
type SFlat a = [S (EPure a)]

-- | Experimental approaches that should be isomorphic to the respective
-- version without a prime (modulo "fast and loose reasoning")
-- type EEff' = Free (Product (Const [STree]) []) (E EEff)
-- type STree' = Free [] (S EEff')

-- Return a temporary location to be used for TEMP.
getTemp :: MonadState Temp m => m Temp
getTemp = modify (+1) >> get

-- | Takes an EPure creates a MOVE statement that stores it and the TEMP it is
-- stored to.
storingTemp :: MonadState Temp m => EPure a -> m (S (EPure a), EPure a)
storingTemp e = (flip MOVE e &&& id) . (extract e :<) . TEMP <$> getTemp

-- | Constructs a SFlat that computes the first expression store it in a new
-- TEMP then computes the second expression. Returns that SFlat the TEMP used
-- for storing the first value and the pure expression constructed from the
-- second expression.
interleaveTemp
  :: MonadState Temp m
  => EEff a
  -> EEff a
  -> m (SFlat a, Temp, EPure a)
interleaveTemp e1 e2 = do
  (s1, e1') <- flattenEEff e1
  (s2, e2') <- flattenEEff e2
  temp <- getTemp
  pure (s1 ++ [MOVE (extract e1 :< TEMP temp) e1'] ++ s2, temp, e2')

flattenEEff :: MonadState Temp m => EEff a -> m (SFlat a, EPure a)

flattenEEff (ERec a (CONST i)) = pure ([], a :< CONST i)
flattenEEff (ERec a (NAME l)) = pure ([], a :< NAME l)
flattenEEff (ERec a (TEMP t)) = pure ([], a :< TEMP t)
flattenEEff (ERec a (MEM m)) = over (mapped._2) ((a :<) . MEM) (flattenEEff m)

flattenEEff (ESEQ _ s e) = do
  s' <- flattenSTree s
  (se, e') <- flattenEEff e
  pure (s' ++ se, e')

flattenEEff (ERec a (CALL f as)) = do
  (ss, es') <- mapAndUnzipM flattenEEff (f:as)
  (ms, tf:tas) <- mapAndUnzipM storingTemp es'
  let ss' = concat $ zipWith ((reverse .) . (:)) ms ss
  temp <- (a :<) . TEMP <$> getTemp
  pure (ss' ++ [MOVE temp (a :< CALL tf tas)], temp)

-- TODO: optimize for case when e1 and e2 commute
flattenEEff (ERec a (OP o e1 e2)) = do
  (s, temp, e') <- e1 `interleaveTemp` e2
  pure (s, a :< OP o (extract e1 :< TEMP temp) e')

flattenSTree :: MonadState Temp m => STree a -> m (SFlat a)

flattenSTree (STip (EXP e)) = fst <$> flattenEEff e
flattenSTree (STip (LABEL l)) = pure [LABEL l]
flattenSTree (STip RETURN) = pure [RETURN]
flattenSTree (SEQ ss) = concat <$> traverse flattenSTree ss

flattenSTree (STip (JUMP e)) =
  (\(s, e') -> s ++ [JUMP e']) <$> flattenEEff e

flattenSTree (STip (CJUMP e l1 l2)) =
  (\(s, e') -> s ++ [CJUMP e' l1 l2]) <$> flattenEEff e

flattenSTree (STip (MOVE e1@(ERec _ TEMP{}) e2)) = do
  (s1, e1') <- flattenEEff e1
  (s2, e2') <- flattenEEff e2
  pure $ s1 ++ s2 ++ [MOVE e1' e2']

-- TODO: optimize for case when e1 and e2 commute
flattenSTree (STip (MOVE (ERec a (MEM e1)) e2)) = do
  (s, temp, e') <- e1 `interleaveTemp` e2
  pure $ s ++ [MOVE (a :< MEM (extract e1 :< TEMP temp)) e']

flattenSTree (STip (MOVE _ _)) = error "impossible due to S invariant"
