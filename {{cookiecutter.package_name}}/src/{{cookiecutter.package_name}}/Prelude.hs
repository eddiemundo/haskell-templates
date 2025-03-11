module {{cookiecutter.package_name}}.Prelude (
  module Prelude,
  type HasCallStack,
  type SomeException,
  type FatalError (..),
  type ByteString,
  type Map,
  type Natural,
  type Set,
  type Text,
  type Type,
  type (:>),
  type Error,
  type Eff,
  type Exception,
  IsString,
  (&),
  (<|>),
  (>>>),
  bimap,
  bind,
  bitraverse,
  catMaybes,
  coerce,
  effEitherToError,
  effMaybeToError,
  eitherToError,
  eitherToLocalError,
  eitherToMaybe,
  errorToEither,
  errorToEitherPure,
  fold,
  fromMaybe,
  head,
  identity,
  makeFieldLabelsNoPrefix,
  mapError,
  mapErrorToLocal,
  mapLeft,
  mapMaybe,
  mapRight,
  maybeToError,
  maybeToList,
  on,
  readJsonFile,
  runErrorWith,
  showAsText,
  throwError,
  throwLocalError,
  toList,
  try,
  void,
  wither,
  writeJsonFile,
  liftIO,
  trace,
  traceShow,
  traceShow_,
  when,
  traceMsgs,
) where

import Control.Applicative (Alternative ((<|>)))
import Control.Category ((>>>))
import Control.Exception.Safe (Exception, SomeException, try)
import Control.Monad (when)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Bifunctor (Bifunctor (first))
import Data.Bifunctor.Apply (Bifunctor (bimap, second))
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Either.Extra (eitherToMaybe)
import Data.Foldable (Foldable (fold, foldl', toList))
import Data.Function (on, (&))
import Data.Functor (void)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Set (Set)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as Text
import Debug.Trace (trace, traceShow)
import Effectful (Eff, IOE, MonadIO (liftIO), runPureEff, (:>))
import Effectful.Dispatch.Dynamic (LocalEnv, SharedSuffix, localSeqUnlift)
import Effectful.Error.Static (CallStack, Error, HasCallStack, runError, runErrorWith, throwError)
import GHC.Natural (Natural)
import Optics.TH (makeFieldLabelsNoPrefix)
import Witherable (Filterable (catMaybes, mapMaybe), Witherable (wither))
import Prelude hiding (filter, head, id)
import Prelude qualified as OriginalPrelude

identity :: a -> a
identity = OriginalPrelude.id

showAsText :: (Show a) => a -> Text
showAsText = show >>> Text.pack

mapError :: forall e1 e2 es a. (Error e2 :> es, Show e2, HasCallStack) => (CallStack -> e1 -> e2) -> Eff (Error e1 : es) a -> Eff es a
mapError f action = do
  result <- runError action
  case result of
    Left (cs, e1) -> throwError $ f cs e1
    Right a -> pure a

maybeToError :: (Error e :> es, Show e, HasCallStack) => e -> Maybe a -> Eff es a
maybeToError e = \case
  Nothing -> throwError e
  Just a -> pure a

eitherToError :: (Error e2 :> es, Show e2, HasCallStack) => (e1 -> e2) -> Either e1 a -> Eff es a
eitherToError f = \case
  Left e1 -> throwError $ f e1
  Right a -> pure a

effEitherToError :: (Error e2 :> es, Show e2, HasCallStack) => (e1 -> e2) -> Eff es (Either e1 a) -> Eff es a
effEitherToError f action = do
  result <- action
  eitherToError f result

bind :: (Monad m) => (a -> m b) -> m a -> m b
bind = (=<<)

newtype FatalError = FatalError Text
  deriving stock (Show)
  deriving anyclass (Exception)

mapRight :: (Bifunctor f) => (b -> c) -> f a b -> f a c
mapRight = second

throwLocalError :: (SharedSuffix es handlerEs, Error e :> localEs, Show e, HasCallStack) => LocalEnv localEs handlerEs -> e -> Eff es a
throwLocalError localEnv e = localSeqUnlift localEnv $ \unlift -> unlift $ throwError e

mapErrorToLocal ::
  forall e1 e2 a handlerEs localEs es.
  (SharedSuffix es handlerEs, Error e2 :> localEs, Show e2, HasCallStack) =>
  LocalEnv localEs handlerEs ->
  (CallStack -> e1 -> e2) ->
  Eff (Error e1 : es) a ->
  Eff es a
mapErrorToLocal localEnv f action = do
  result <- runError action
  case result of
    Left (cs, e1) -> throwLocalError localEnv $ f cs e1
    Right a -> pure a

eitherToLocalError ::
  forall e1 e2 a localEs handlerEs es.
  ( SharedSuffix es handlerEs
  , Error e2 :> localEs
  , Show e2
  , HasCallStack
  ) =>
  LocalEnv localEs handlerEs ->
  (e1 -> e2) ->
  Either e1 a ->
  Eff es a
eitherToLocalError localEnv f = \case
  Left e -> throwLocalError localEnv $ f e
  Right a -> pure a

mapLeft :: (Bifunctor f) => (a -> c) -> f a b -> f c b
mapLeft = first

head :: (Foldable t) => t a -> Maybe a
head =
  toList >>> \case
    [] -> Nothing
    x : _ -> Just x

errorToEither ::
  (CallStack -> e1 -> e2) ->
  Eff (Error e1 : es) a ->
  Eff es (Either e2 a)
errorToEither f action = do
  result <- runError action
  case result of
    Left (cs, e1) -> pure $ Left $ f cs e1
    Right a -> pure $ Right a

errorToEitherPure ::
  (CallStack -> e1 -> e2) ->
  Eff (Error e1 : '[]) a ->
  Either e2 a
errorToEitherPure f action = do
  errorToEither f action & runPureEff

effMaybeToError :: (Error e :> es, Show e, HasCallStack) => e -> Eff es (Maybe a) -> Eff es a
effMaybeToError e action = do
  result <- action
  maybeToError e result

readJsonFile :: forall a es. (Error FatalError :> es, IOE :> es, FromJSON a) => FilePath -> Eff es a
readJsonFile path = do
  Aeson.decodeFileStrict' @a path
    & try @_ @SomeException
    & liftIO
    & effEitherToError (FatalError . showAsText)
    & effMaybeToError (FatalError $ "Failed to decode JSON file: " <> Text.pack path)

writeJsonFile :: forall a es. (Error FatalError :> es, IOE :> es, ToJSON a) => FilePath -> a -> Eff es ()
writeJsonFile path a = do
  Aeson.encodeFile path a
    & try @_ @SomeException
    & liftIO
    & effEitherToError (FatalError . showAsText)

traceShow_ :: (Show a) => a -> ()
traceShow_ a = trace (show a) ()

traceMsgs :: [Text] -> ()
traceMsgs as = do
  as
    & foldl'
      ( \ !acc a -> do
          let !() = traceShow a ()
          acc
      )
      ()

