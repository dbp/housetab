{-# LANGUAGE OverloadedStrings #-}
module Context where

import           Control.Monad              (join)
import           Data.Monoid                ((<>))
import           Data.Pool                  (Pool)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Vault.Lazy            as Vault
import           Database.PostgreSQL.Simple (Connection)
import           Larceny                    (Library, Substitutions, render,
                                             renderWith)
import           Network.Wai                (Request (..), Response)
import           Network.Wai.Session        (Session)
import           Web.Fn


data Ctxt = Ctxt { request :: FnRequest
                 , db      :: Pool Connection
                 , library :: Library ()
                 , sess    :: Vault.Key (Session IO Text (Maybe Text))
                 }

instance RequestContext Ctxt where
  getRequest (Ctxt r _ _ _) = r
  setRequest (Ctxt _ p l s) r = Ctxt r p l s

render' :: Ctxt -> Text -> IO (Maybe Response)
render' ctxt = renderWith' ctxt mempty

renderWith' :: Ctxt -> Substitutions () -> Text -> IO (Maybe Response)
renderWith' ctxt subs tpl =
  do t <- renderWith (library ctxt) subs () (T.splitOn "/" tpl)
     case t of
       Nothing -> return Nothing
       Just t' -> okHtml t'

tshow :: Show a => a -> Text
tshow = T.pack . show

getFromSession :: Ctxt -> Text -> IO (Maybe Text)
getFromSession ctxt k =
  case Vault.lookup (sess ctxt) (vault (fst $ request ctxt)) of
    Just (getsess, _) ->
      join <$> getsess k
    Nothing ->
      error $ "getFromSession: no session in vault when looking for '" <> T.unpack k <> "'"

setInSession :: Ctxt -> Text -> Text -> IO ()
setInSession ctxt k v =
  case Vault.lookup (sess ctxt) (vault (fst $ request ctxt)) of
    Just (_, setsess) -> setsess k (Just v)
    Nothing ->
      error $ "setInSession: no session in vault when setting '" <> T.unpack k <> "' to '" <> T.unpack v <> "'"

clearFromSession :: Ctxt -> Text -> IO ()
clearFromSession ctxt k =
  let Just (_, setsess) = Vault.lookup (sess ctxt) (vault (fst $ request ctxt))
     in setsess k Nothing
