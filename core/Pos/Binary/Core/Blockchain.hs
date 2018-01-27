-- | Binary serialization of core block types.

module Pos.Binary.Core.Blockchain
       (
       ) where

import           Universum

import           Pos.Binary.Class (BiDec (..), BiEnc (..), encodeListLen, enforceSize)
import           Pos.Binary.Core.Common ()
import qualified Pos.Core.Block.Blockchain as T
import           Pos.Crypto.Configuration (HasCryptoConfiguration, getProtocolMagic, protocolMagic)

instance ( Typeable b
         , BiEnc (T.BHeaderHash b)
         , BiEnc (T.BodyProof b)
         , BiEnc (T.ConsensusData b)
         , BiEnc (T.ExtraHeaderData b)
         , HasCryptoConfiguration
         ) =>
         BiEnc (T.GenericBlockHeader b) where
    encode bh =  encodeListLen 5
              <> encode (getProtocolMagic protocolMagic)
              <> encode (T._gbhPrevBlock bh)
              <> encode (T._gbhBodyProof bh)
              <> encode (T._gbhConsensus bh)
              <> encode (T._gbhExtra bh)
instance ( Typeable b
         , BiDec (T.BHeaderHash b)
         , BiDec (T.BodyProof b)
         , BiDec (T.ConsensusData b)
         , BiDec (T.ExtraHeaderData b)
         , HasCryptoConfiguration
         ) =>
         BiDec (T.GenericBlockHeader b) where
    decode = do
        enforceSize "GenericBlockHeader b" 5
        blockMagic <- decode
        when (blockMagic /= getProtocolMagic protocolMagic) $
            fail $ "GenericBlockHeader failed with wrong magic: " <> show blockMagic
        _gbhPrevBlock <- ({-# SCC "decode_header_prev" #-} decode)
        _gbhBodyProof <- ({-# SCC "decode_header_body_proof" #-} decode)
        _gbhConsensus <- ({-# SCC "decode_header_consensus" #-} decode)
        _gbhExtra     <- ({-# SCC "decode_header_extra" #-} decode)
        pure T.UnsafeGenericBlockHeader {..}

instance ( Typeable b
         , BiEnc (T.BHeaderHash b)
         , BiEnc (T.BodyProof b)
         , BiEnc (T.ConsensusData b)
         , BiEnc (T.ExtraHeaderData b)
         , BiEnc (T.Body b)
         , BiEnc (T.ExtraBodyData b)
         , HasCryptoConfiguration
         ) =>
         BiEnc (T.GenericBlock b) where
    encode gb =  encodeListLen 3
              <> encode (T._gbHeader gb)
              <> encode (T._gbBody gb)
              <> encode (T._gbExtra gb)
instance ( Typeable b
         , BiDec (T.BHeaderHash b)
         , BiDec (T.BodyProof b)
         , BiDec (T.ConsensusData b)
         , BiDec (T.ExtraHeaderData b)
         , BiDec (T.Body b)
         , BiDec (T.ExtraBodyData b)
         , HasCryptoConfiguration
         ) =>
         BiDec (T.GenericBlock b) where
    decode = do
        enforceSize "GenericBlock" 3
        _gbHeader <- ({-# SCC "decode_block_header" #-} decode)
        _gbBody   <- ({-# SCC "decode_block_body" #-} decode)
        _gbExtra  <- ({-# SCC "decode_block_extra" #-} decode)
        pure T.UnsafeGenericBlock {..}
