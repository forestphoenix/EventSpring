{-# LANGUAGE MultiParamTypeClasses #-}

module EventSpring.Serialized where

import           Data.ByteString.Lazy (ByteString)
import Data.Typeable (Typeable)

class (Typeable content, Eq content) => Serialized content where
    serializer :: Serializer content

data Serializer a = Serializer {
    serialize   :: a -> ByteString,
    deserialize :: ByteString -> Either String a
}
