{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleInstances #-}

-- | The Play Framework style data binding in Scotty.
--
-- Data difinition:
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- >
-- > import Web.Scotty.Binding.Play (deriveBindable)
-- >
-- > data Sample = Sample
-- >     { field1 :: Int
-- >     , field2 :: Text
-- >     }
-- >
-- > deriveBindable ''Sample
--
-- set as GET parameter:
--
-- > > curl http://localhost:3000/?data.field1=1&data.field2=whisky
--
-- We can get 'Sample' in Scotty:
--
-- > main :: IO ()
-- > main = scotty 3000 $ get "/" $ do
-- >     a <- parseParam "data"
-- >     liftIO $ print $ field1 a --> 1
-- >     liftIO $ print $ field2 a --> "whisky"

module Web.Scotty.Binding.Play
    ( Bindable(..)
    , deriveBindable
    ) where

import Control.Applicative
import Control.Monad.Error.Class (catchError)
import Data.ByteString (ByteString)
import Data.Maybe (catMaybes)
import Data.Monoid
import qualified Data.Text as ST
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.Builder as LTB
import qualified Data.Text.Lazy.Builder.Int as LTB
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Web.Scotty (ActionM, Parsable, param)

-- | Class of generic bindable data structure.
class Bindable a where
    parseParams
        :: Text -- ^ prefix
        -> ActionM a
    parseParams prefix = parseParams' prefix Nothing
    parseParams'
        :: Text -- ^ prefix
        -> Maybe Text -- ^ suffix
        -> ActionM a

instance Bindable a => Bindable [a] where
    parseParams' prefix _ = parseParamList prefix [0..]

instance Bindable Bool where
    parseParams' = parse

instance Bindable Char where
    parseParams' = parse

instance Bindable Double where
    parseParams' = parse

instance Bindable Float where
    parseParams' = parse

instance Bindable Int where
    parseParams' = parse

instance Bindable Integer where
    parseParams' = parse

instance Bindable () where
    parseParams' = parse

instance Bindable ByteString where
    parseParams' = parse

instance Bindable ST.Text where
    parseParams' = parse

instance Bindable Text where
    parseParams' = parse

instance Bindable a => Bindable (Maybe a) where
    parseParams' pre msuf = (Just <$> parseParams' pre msuf)
        `catchError` \_ -> return Nothing

parse :: Parsable a => Text -> Maybe Text -> ActionM a
parse prefix msuffix = param $ mconcat $ catMaybes [Just prefix, msuffix]

parseParamList :: Bindable a => Text -> [Int] -> ActionM [a]
parseParamList _      []     = fail "not reached"
parseParamList prefix (n:ns) = do
    a <- parseParams' (prefix <> br) Nothing
    as <- parseParamList prefix ns `catchError` \_ -> return []
    return $ a:as
  where
    toText = LTB.toLazyText . LTB.decimal
    br = "[" <> toText n <> "]"

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

-- x <- param $ mconcat $ catMaybes [Just pname, Just ".", Just fname, sname]
getParamS :: Name -> Name -> VarStrictType -> Q (Name, StmtQ)
getParamS pname sname (fname, _, _) = do
    x <- newName "x"
    return (x, bindS (varP x)
        [|parseParams (mconcat (catMaybes
            [ Just $(varE pname)
            , Just "."
            , Just $(stringE $ nameBase fname)
            , $(varE sname)
            ]))
         |])

-- | by TH
deriveBindable :: Name -> DecsQ
deriveBindable dat = do
    (TyConI (DataD _ _ _ [RecC dConst vsTypes] _)) <- reify dat
    let pname = mkName "prefix"
    let sname = mkName "msuffix"
    binds <- mapM (getParamS pname sname) vsTypes
    let con = appsE (conE dConst:map (varE . fst) binds)
    let expr = doE (map snd binds ++ [noBindS [|return $con|]])
    [d|
        instance Bindable $(conT dat) where
            parseParams' prefix msuffix = $expr
      |]
