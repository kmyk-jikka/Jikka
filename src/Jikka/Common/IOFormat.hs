{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Jikka.Common.IOFormat where

import Control.Arrow
import Control.Monad.Identity
import Data.IORef
import Data.List
import qualified Data.Map as M
import qualified Data.Vector as V
import Jikka.Common.Error
import Jikka.Common.IO (hGetWord)
import System.IO (stdin)
import Text.Read (readMaybe)

data FormatExpr
  = Var String
  | Plus FormatExpr Integer
  | At FormatExpr String
  | Len FormatExpr
  deriving (Eq, Ord, Read, Show)

data FormatTree
  = Exp FormatExpr
  | Newline
  | Seq [FormatTree]
  | Loop String FormatExpr FormatTree
  deriving (Eq, Ord, Read, Show)

data IOFormat = IOFormat
  { inputVariables :: [String],
    inputTree :: FormatTree,
    -- | This uses `Either` to distinguish a type and the 1-tuple of it.
    outputVariables :: Either String [String],
    outputTree :: FormatTree
  }
  deriving (Eq, Ord, Read, Show)

mapFormatTreeM :: Monad m => (FormatTree -> m FormatTree) -> FormatTree -> m FormatTree
mapFormatTreeM f = \case
  Loop i n body -> do
    body <- mapFormatTreeM f body
    f $ Loop i n body
  Seq formats -> Seq <$> mapM f formats
  format -> f format

mapFormatTree :: (FormatTree -> FormatTree) -> FormatTree -> FormatTree
mapFormatTree f = runIdentity . mapFormatTreeM (return . f)

normalizeFormatTree :: FormatTree -> FormatTree
normalizeFormatTree = \case
  Exp e -> Exp e
  Newline -> Newline
  Seq formats ->
    let unSeq = \case
          Seq formats -> formats
          format -> [format]
     in Seq (concatMap (unSeq . normalizeFormatTree) formats)
  Loop i n body -> case normalizeFormatTree body of
    Seq [] -> Seq []
    body -> Loop i n body

normalizeIOFormat :: IOFormat -> IOFormat
normalizeIOFormat format =
  format
    { inputTree = normalizeFormatTree (inputTree format),
      outputTree = normalizeFormatTree (outputTree format)
    }

hasNewline :: FormatTree -> Bool
hasNewline = \case
  Exp _ -> False
  Newline -> True
  Seq formats -> any hasNewline formats
  Loop _ _ body -> hasNewline body

formatFormatExpr :: FormatExpr -> String
formatFormatExpr = \case
  Var x -> x
  Plus e k -> "(" ++ formatFormatExpr e ++ " + " ++ show k ++ ")"
  At e i -> formatFormatExpr e ++ "[" ++ i ++ "]"
  Len e -> "len(" ++ formatFormatExpr e ++ ")"

formatFormatTree :: FormatTree -> String
formatFormatTree =
  let replace :: Eq a => [a] -> [a] -> [a] -> [a]
      replace patt subst = go
        where
          go text | patt `isPrefixOf` text = subst ++ go (drop (length patt) text)
          go [] = []
          go (c : s) = c : go s
      unwords' = replace "\n\n" "\n" . replace "\n " "\n" . replace " \n" "\n" . unwords
   in \case
        Exp e -> formatFormatExpr e
        Newline -> "(newline)\n"
        Seq formats -> unwords' (map formatFormatTree formats)
        Loop i n body ->
          unwords'
            [ "for " ++ i ++ " < " ++ formatFormatExpr n ++ " {\n",
              formatFormatTree body ++ "\n",
              "}"
            ]

formatIOFormat :: IOFormat -> String
formatIOFormat format =
  unlines
    ( [ "input tree:"
      ]
        ++ map ("    " ++) (lines (formatFormatTree (inputTree format)))
        ++ [ "input variables: " ++ show (inputVariables format),
             "output variables: " ++ show (outputVariables format),
             "output tree:"
           ]
        ++ map ("    " ++) (lines (formatFormatTree (outputTree format)))
    )

packSubscriptedVar :: String -> [String] -> FormatExpr
packSubscriptedVar x indices = foldl At (Var x) indices

packSubscriptedVar' :: String -> [String] -> FormatTree
packSubscriptedVar' = (Exp .) . packSubscriptedVar

unpackSubscriptedVar :: MonadError Error m => FormatExpr -> m (String, [String])
unpackSubscriptedVar = \case
  Var x -> return (x, [])
  At e i -> second (++ [i]) <$> unpackSubscriptedVar e
  e -> throwInternalError $ "not a subscripted variable: " ++ formatFormatExpr e

makeReadValueIO :: forall m value. (MonadError Error m, MonadIO m) => (value -> m Integer) -> (Integer -> value) -> (value -> m (V.Vector value)) -> (V.Vector value -> value) -> IOFormat -> m ([value], M.Map String value)
makeReadValueIO toInt fromInt toList fromList format = wrapError' "Jikka.Common.IOFormat.makeReadValueIO" $ do
  env <- liftIO $ newIORef M.empty :: m (IORef (M.Map String value))
  sizes <- liftIO $ newIORef M.empty :: m (IORef (M.Map String Integer))
  let lookup :: String -> m value
      lookup x = do
        y <- M.lookup x <$> liftIO (readIORef env)
        case y of
          Nothing -> throwInternalError $ "undefined variable: " ++ x
          Just y -> return y
  let goEmpty :: FormatTree -> m ()
      goEmpty = \case
        Exp e -> do
          (x, _) <- unpackSubscriptedVar e
          y <- M.lookup x <$> liftIO (readIORef env)
          case y of
            Just _ -> return ()
            Nothing -> do
              let y = fromList V.empty
              liftIO $ modifyIORef' env (M.insert x y)
        Newline -> return ()
        Seq formats -> mapM_ goEmpty formats
        Loop _ _ body -> do
          goEmpty body
  let go :: FormatTree -> m ()
      go = \case
        Exp e -> do
          (x, indices) <- unpackSubscriptedVar e
          word <- liftIO $ hGetWord stdin
          n <- case readMaybe word of
            Just n -> return n
            Nothing -> throwWrongInputError $ "not a integer: " ++ word
          y <- M.lookup x <$> liftIO (readIORef env)
          y <- case y of
            Just y -> return y
            Nothing -> do
              let go' x i = do
                    size <- M.lookup i <$> liftIO (readIORef sizes)
                    case size of
                      Nothing -> throwInternalError $ "undefined variable: " ++ i
                      Just size -> return . fromList $ V.replicate (fromInteger size) x
              foldM go' (fromInt (-1)) indices
          let go' y = \case
                [] -> return (fromInt n)
                (i : indices) -> do
                  i <- toInt =<< lookup i
                  y <- toList y
                  z <- go' (y V.! fromInteger i) indices
                  return . fromList $ y V.// [(fromInteger i, z)]
          y <- go' y indices
          liftIO $ modifyIORef' env (M.insert x y)
        Newline -> return ()
        Seq formats -> mapM_ go formats
        Loop i n body -> do
          n <- case n of
            Var n -> toInt =<< lookup n
            Plus (Var n) k -> (+ k) <$> (toInt =<< lookup n)
            Len (Var xs) -> toInteger . V.length <$> (toList =<< lookup xs)
            _ -> throwInternalError $ "invalid loop size in input tree: " ++ formatFormatExpr n
          liftIO $ modifyIORef' sizes (M.insert i n)
          if n == 0
            then goEmpty body
            else forM_ [0 .. n -1] $ \i' -> do
              liftIO $ modifyIORef' env (M.insert i (fromInt i'))
              go body
  go (inputTree format)
  values <- mapM lookup (inputVariables format)
  env <- liftIO $ readIORef env
  return (values, env)

makeWriteValueIO :: (MonadError Error m, MonadIO m) => (value -> m [value]) -> (Integer -> value) -> (value -> m Integer) -> (value -> m (V.Vector value)) -> IOFormat -> M.Map String value -> value -> m ()
makeWriteValueIO toTuple fromInt toInt toList format env value = wrapError' "Jikka.Common.IOFormat.makeWriteValueIO" $ do
  env <- liftIO $ newIORef env
  let lookup x = do
        y <- M.lookup x <$> liftIO (readIORef env)
        case y of
          Nothing -> throwInternalError $ "undefined variable: " ++ x
          Just y -> return y
  case outputVariables format of
    Left x -> liftIO $ modifyIORef' env (M.insert x value)
    Right xs -> do
      values <- toTuple value
      when (length values /= length xs) $ do
        throwRuntimeError $ "sizes of values mismtach: expected = " ++ show (length xs) ++ ", actual = " ++ show (length values)
      forM_ (zip xs values) $ \(x, value) -> do
        liftIO $ modifyIORef' env (M.insert x value)
  let evaluate = \case
        Var n -> lookup n
        Plus e k -> fromInt . (+ k) <$> (toInt =<< evaluate e)
        Len e -> do
          e <- toList =<< evaluate e
          return . fromInt . toInteger $ V.length e
        At e i -> do
          xs <- toList =<< evaluate e
          i <- toInt =<< lookup i
          case xs V.!? fromInteger i of
            Nothing -> throwRuntimeError $ "length of list is shorter than expected: expected > " ++ show i ++ ", actual = " ++ show (V.length xs)
            Just x -> return x
  let go = \case
        Exp e -> do
          x <- toInt =<< evaluate e
          liftIO $ putStr (show x ++ " ")
        Newline -> liftIO $ putChar '\n'
        Seq formats -> mapM_ go formats
        Loop i n body -> do
          n <- toInt =<< evaluate n
          forM_ [0 .. n -1] $ \i' -> do
            liftIO $ modifyIORef' env (M.insert i (fromInt i'))
            go body
  go (outputTree format)
