{-# language GADTs #-}
{-# language FlexibleInstances, UndecidableInstances  #-}
{-# language RankNTypes #-}
module Data.Tensor.Compiler.Edsl2Edsl where

import Control.Monad (ap)
import Control.Monad.State
import Control.Monad.Trans.Writer

import Data.Int (Int32)
import Data.IORef 

{- | "Compilation as typed EDSL-to-EDSL transformation" - E Axelsson - https://arxiv.org/html/1603.08865 -}

{- | By parameterizing Prog on the expression type exp, we can have imperative programs with different representations of expressions . -}


data Prog exp a where
  Return :: a -> Prog exp a
  (:>>=) :: Prog exp a -> (a -> Prog exp b) -> Prog exp b
  CMD :: CMD exp a -> Prog exp a
  
instance Functor (Prog exp) where
  fmap f m = m >>= return . f

instance Applicative (Prog exp) where
  pure = return
  (<*>) = ap

instance Monad (Prog exp) where
  return = Return
  (>>=) = (:>>=)


data CMD exp a where
  -- References:
  InitRef :: Type a => exp a -> CMD exp (Ref a)
  GetRef  :: Type a => Ref a -> CMD exp (Val a)
  SetRef  :: Type a => Ref a -> exp a -> CMD exp ()

  -- Input/output:
  Read     :: CMD exp (Val Int32)
  Write    :: exp Int32 -> CMD exp ()
  PrintStr :: String -> CMD exp ()

  -- Loops:
  For :: exp Int32 -> (Val Int32 -> Prog exp ()) -> CMD exp ()

{- | NOTE : • The constraint ‘Ord a’ is no smaller than the instance head
      (Use UndecidableInstances to permit this)-}
class    (Eq a, Ord a, Show a) => Type a
instance (Eq a, Ord a, Show a) => Type a

{- | In a standard interpretation (e.g. running the program in Haskell’s IO monad), we want Val a to be represented by an actual value of type a. But in a non-standard interpretation such as compilation to C code, we want Val a to be a variable identifier. To reconcile these different needs, we define Val as a sum type: -}

-- Variable identifier
type VarId = String

data Val a
  = ValRun a       -- Concrete value
  | ValComp VarId  -- Symbolic value
  
{- | For the same reason, Ref is defined to be either an actual IORef or a variable identifier: -}

data Ref a
  = RefRun (IORef a)  -- Concrete reference
  | RefComp VarId     -- Symbolic reference

{-| The following function lifts an interpretation of instructions in a monad m to an interpretation of programs in the same monad -}

interpret :: Monad m => (forall a. CMD exp a -> m a) -> Prog exp b -> m b
interpret int mi = case mi of
  (Return a) -> return a
  (p :>>= k) -> interpret int p >>= interpret int . k
  (CMD cmd)  -> int cmd

{-| Using interpret, we can define the standard interpretation that runs a program in the IO monad: -}

runIO :: EvalExp exp => Prog exp a -> IO a
runIO = interpret runCMD

runCMD :: EvalExp exp => CMD exp a -> IO a
runCMD (InitRef a)           = RefRun <$> newIORef (evalExp a)
runCMD (GetRef (RefRun r))   = ValRun <$> readIORef r
runCMD (SetRef (RefRun r) a) = writeIORef r (evalExp a)
runCMD Read                  = ValRun . read <$> getLine
runCMD (Write a)             = putStr $ show $ evalExp a
runCMD (PrintStr s)          = putStr s
runCMD (For n body)          =
    mapM_ (runIO . body . ValRun) [0 .. evalExp n - 1]

{-| Most of the work is done in runCMD which gives the interpretation of each instruction. Note the use of ValRun and RefRun on both sides of the equations. As said earlier, ValComp and RefComp are not used in the standard interpretation.

Since running a program also involves evaluating expressions, we need the constraint EvalExp exp that gives access to the function evalExp (for example in the interpretation of InitRef). EvalExp is defined as follows: -}

class EvalExp exp where
  -- Evaluate a closed expression
  evalExp :: exp a -> a


{-| Compiler front-end : smart constructors : -}

initRef :: Type a => exp a -> Prog exp (Ref a)
initRef = CMD . InitRef

setRef :: Type a => Ref a -> exp a -> Prog exp ()
setRef r a = CMD (SetRef r a)


{-| But instructions that involve Val will need more care. We do not actually want to expose Val to the user, but rather use exp to hold values. To achieve this, we introduce a class for expressions supporting injection of constants and variables: -}

class FreeExp exp where
  -- Inject a constant value
  constExp :: Type a => a -> exp a
  -- Inject a variable
  varExp   :: Type a => VarId -> exp a

{-| Using FreeExp, we can make a general function that converts Val to an expression – independent of interpretation: -}

valToExp :: (Type a, FreeExp exp) => Val a -> exp a
valToExp (ValRun a)  = constExp a
valToExp (ValComp v) = varExp v




{-| Now we can define the rest of the front end, using valToExp where needed: -}

getRef :: (Type a, FreeExp exp) => Ref a -> Prog exp (exp a)
getRef = fmap valToExp . CMD . GetRef

readInput :: FreeExp exp => Prog exp (exp Int32)
readInput = valToExp <$> CMD Read

writeOutput :: exp Int32 -> Prog exp ()
writeOutput = CMD . Write

printStr :: String -> Prog exp ()
printStr = CMD . PrintStr

for :: FreeExp exp => exp Int32 -> (exp Int32 -> Prog exp ()) -> Prog exp ()
for n body = CMD $ For n (body . valToExp)

{-| As a convenience, we also provide a function for modifying a reference using a pure function: -}

modifyRef :: (Type a, FreeExp exp) => Ref a -> (exp a -> exp a) -> Prog exp ()
modifyRef r f = setRef r . f =<< getRef r



{-| Now that we have a front end for the imperative EDSL, we also need to define an expression type before we can use it for anything.

We begin with a very simple expression type that only has variables, literals and some primitive functions: -}

data LowExp a where
  LVar :: Type a => VarId -> LowExp a
  LLit :: Type a => a -> LowExp a
  LAdd :: (Num a, Type a) => LowExp a -> LowExp a -> LowExp a
  LMul :: (Num a, Type a) => LowExp a -> LowExp a -> LowExp a
  LNot :: LowExp Bool -> LowExp Bool
  LEq  :: Type a => LowExp a -> LowExp a -> LowExp Bool

-- Convenience Num instance

instance (Num a, Type a) => Num (LowExp a) where
  fromInteger = LLit . fromInteger
  (+) = LAdd
  (*) = LMul


{-| The implementation of LowExp is completed by instantiating the EvalExp and FreeExp classes: -}

instance EvalExp LowExp where
  evalExp (LLit a)   = a
  evalExp (LAdd a b) = evalExp a + evalExp b
  evalExp (LMul a b) = evalExp a * evalExp b
  evalExp (LNot a)   = not $ evalExp a
  evalExp (LEq a b)  = evalExp a == evalExp b

instance FreeExp LowExp where
  constExp = LLit
  varExp   = LVar

{-| Example program : -}

sumInput :: Prog LowExp ()
sumInput = do
  r <- initRef 0
  printStr "Please enter 4 numbers\n"
  for 4 $ \i -> do
    printStr " > "
    n <- readInput
    modifyRef r (+n)
  printStr "The sum of your numbers is "
  s <- getRef r
  writeOutput s
  printStr ".\n"  






{-| Code generation -}

-- Code generation monad
type Code = WriterT [Stmt] (State Unique)

type Stmt   = String
type Unique = Integer

code :: ShowExp exp => Prog exp a -> Code a
code = interpret codeCMD

lowToCode :: Prog LowExp a -> String
lowToCode = runCode . code


runCode :: Code a -> String
runCode = unlines . flip evalState 0 . execWriterT . indent

-- Emit a statement in the generated code
stmt :: Stmt -> Code ()
stmt s = tell [s]

-- Generate a unique identifier
unique :: String -> Code VarId
unique base = do
  u <- get; put (u+1)
  return (base ++ show u)

-- Generate a unique symbolic value
freshVar :: Type a => Code (Val a)
freshVar = ValComp <$> unique "v"

-- Generate a unique reference
freshRef :: Type a => Code (Ref a)
freshRef = RefComp <$> unique "r"

-- Modify a code generator by indenting the generated code
--
-- `censor f m` is an action that executes the action m and applies the function f to its output, leaving the return value unchanged. (from `transformers` WriterT)
indent :: Code a -> Code a
indent = censor $ map ("    " ++)

-- Code generation of instructions
codeCMD :: ShowExp exp => CMD exp a -> Code a
codeCMD (InitRef a) = do
  r <- freshRef
  stmt $ unwords [show r, "<- initRef", showExp a]
  return r
codeCMD (GetRef r) = do
  v <- freshVar
  stmt $ unwords [show v, "<- getRef", show r]
  return v
codeCMD (SetRef r a) = stmt $ unwords ["setRef", show r, showExp a]
codeCMD Read = do
  v <- freshVar
  stmt $ unwords [show v, "<- readInput"]
  return v
codeCMD (Write a)    = stmt $ unwords ["writeOutput", showExp a]
codeCMD (PrintStr s) = stmt $ unwords ["printStr", show s]
codeCMD (For n body) = do
  i <- freshVar
  stmt $ unwords ["for", show i, "<", showExp n]
  indent $ code (body i)
  stmt "end for"


{-| showing expressions -}

instance Show (Val a) where show (ValComp a) = a
instance Show (Ref a) where show (RefComp r) = r

class ShowExp exp where
  showExp :: exp a -> String

bracket :: String -> String
bracket s = "(" ++ s ++ ")"

instance ShowExp LowExp where
  showExp (LVar v)   = v
  showExp (LLit a)   = show a
  showExp (LAdd a b) = bracket (showExp a ++ " + " ++ showExp b)
  showExp (LMul a b) = bracket (showExp a ++ " * " ++ showExp b)
  showExp (LNot a)   = bracket ("not " ++ showExp a)
  showExp (LEq a b)  = bracket (showExp a ++ " == " ++ showExp b)



{-| The expression language LowExp is really too simple to be very useful. For example, it does not allow any kind of iteration, so any iterative program has to be written using the monadic for-loop. 

To fix this, we introduce a new expression language, HighExp: -}

data HighExp a where
  -- Simple constructs (same as in LowExp):
  HVar :: Type a => VarId -> HighExp a
  HLit :: Type a => a -> HighExp a
  HAdd :: (Num a, Type a) => HighExp a -> HighExp a -> HighExp a
  HMul :: (Num a, Type a) => HighExp a -> HighExp a -> HighExp a
  HNot :: HighExp Bool -> HighExp Bool
  HEq  :: Type a => HighExp a -> HighExp a -> HighExp Bool

  -- Let binding:
  Let  :: Type a
       => HighExp a                 -- value to share
       -> (HighExp a -> HighExp b)  -- body
       -> HighExp b

  -- Pure iteration:
  Iter :: Type s
       => HighExp Int32            -- number of iterations
       -> HighExp s                -- initial state
       -> (HighExp s -> HighExp s) -- step function
       -> HighExp s                -- final state

instance (Num a, Type a) => Num (HighExp a) where
  fromInteger = HLit . fromInteger
  (+) = HAdd
  (*) = HMul

instance FreeExp HighExp where
  constExp = HLit
  varExp   = HVar


{- |
Now we are getting close to the main point of the article. This is what we have so far:

    * A way to generate code from low-level programs (of type Prog LowExp a).

    * The ability to write high-level programs (of type Prog HighExp a).

What is missing is a function that translates high-level programs to low-level programs. Generalizing the problem a bit, we need a way to rewrite a program over one expression type to a program over a different expression type – a process which we call “re-expressing”.

Since Prog is a monad like any other, we can actually express this translation function using interpret:
-}


reexpress :: (forall a . exp1 a -> Prog exp2 (exp2 a))
          -> Prog exp1 b
          -> Prog exp2 b
reexpress reexp = interpret (reexpressCMD reexp)

reexpressCMD :: (forall a . exp1 a -> Prog exp2 (exp2 a))
             -> CMD exp1 b
             -> Prog exp2 b
reexpressCMD reexp (InitRef a)  = CMD . InitRef =<< reexp a
reexpressCMD reexp (GetRef r)   = CMD (GetRef r)
reexpressCMD reexp (SetRef r a) = CMD . SetRef r =<< reexp a
reexpressCMD reexp Read         = CMD Read
reexpressCMD reexp (Write a)    = CMD . Write =<< reexp a
reexpressCMD reexp (PrintStr s) = CMD (PrintStr s)
reexpressCMD reexp (For n body) = do
    n' <- reexp n
    CMD $ For n' (reexpress reexp . body)

transHighExp :: HighExp a -> Prog LowExp (LowExp a)
transHighExp (HVar v)   = return (LVar v)
transHighExp (HLit a)   = return (LLit a)
transHighExp (HAdd a b) = LAdd <$> transHighExp a <*> transHighExp b
transHighExp (HMul a b) = LMul <$> transHighExp a <*> transHighExp b
transHighExp (HNot a)   = LNot <$> transHighExp a
transHighExp (HEq a b)  = LEq  <$> transHighExp a <*> transHighExp b
-- It gets more interesting when we get to Let. There is no corresponding construct in LowExp, so we have to realize it using imperative constructs:
transHighExp (Let a body) = do
  r  <- initRef =<< transHighExp a
  a' <- CMD $ GetRef r
  transHighExp $ body $ valToExp a'
-- Translation of Iter is similar to that of Let in that it uses a reference for the local state. The iteration is realized by an imperative for loop. In each iteration, the state variable r is read, then the next state is computed and written back to the r. After the loop, the content of r is returned as the final state.
transHighExp (Iter n s body) = do
  n' <- transHighExp n
  r  <- initRef =<< transHighExp s
  for n' $ \_ -> do
    sPrev <- CMD $ GetRef r
    sNext <- transHighExp $ body $ valToExp sPrev
    setRef r sNext
  getRef r

translateHigh :: Prog HighExp a -> Prog LowExp a
translateHigh = reexpress transHighExp

compile :: Prog HighExp a -> String
compile = lowToCode . translateHigh  






