{-# LANGUAGE EmptyDataDecls         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}

-- | Display a type which satisfies 'ToHtml' with a lucid template.
--
-- A template is a function of type @ToHtml a => a -> Html ()@
--
-- For example,
--
-- @
-- test :: ToHtml a => a -> Html ()
-- test = p_ . toHtml
--
-- data Test
--
-- instance HasTemplate Test where
--     template _ = test
-- @

module Servant.HTML.Lucid.Template where

import           Data.Proxy
import           Data.Typeable      (Typeable)
import           Lucid              (Html, ToHtml (..), renderBS)
import qualified Network.HTTP.Media as M
import           Servant.API        (Accept (..), MimeRender (..))


data HTML (f :: *) deriving Typeable


instance Accept (HTML t) where
    contentType _ = "text" M.// "html" M./: ("charset", "utf-8")


class HasTemplate t where
    template :: ToHtml a => Proxy (HTML t) -> a -> Html ()


instance (ToHtml a, HasTemplate t) => MimeRender (HTML t) a where
    mimeRender p x = renderBS (template p x)
