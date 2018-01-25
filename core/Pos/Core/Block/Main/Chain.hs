{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

-- | Definitions of the main blockchain ('Blockchain' class and related).

module Pos.Core.Block.Main.Chain
       ( BodyProof (..)
       , ConsensusData (..)
       , Body (..)
       ) where

import           Universum

import qualified Data.Text.Buildable
import           Fmt (genericF)

import           Pos.Binary.Class (Bi)
import           Pos.Binary.Core.Delegation ()
import           Pos.Binary.Core.Ssc ()
import           Pos.Binary.Core.Txp ()
import           Pos.Binary.Core.Update ()
import           Pos.Core.Block.Blockchain (Blockchain (..), GenericBlockHeader (..))
import           Pos.Core.Block.Main.Types (MainBlock, MainBlockchain, MainExtraBodyData,
                                            MainExtraHeaderData, MainToSign (..))
import           Pos.Core.Block.Union.Types (Block, BlockHeader, BlockSignature (..))
import           Pos.Core.Class (IsMainHeader (..))
import           Pos.Core.Common (ChainDifficulty)
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Core.Delegation (DlgPayload)
import           Pos.Core.Slotting.Types (SlotId (..))
import           Pos.Core.Ssc (SscPayload, SscProof, mkSscProof)
import           Pos.Core.Txp (TxPayload, TxProof, mkTxProof)
import           Pos.Core.Update (UpdatePayload, UpdateProof, mkUpdateProof)
import           Pos.Crypto (Hash, PublicKey, hash)

instance ( HasConfiguration
         , Bi BlockHeader
         , Bi (BodyProof MainBlockchain)
         , IsMainHeader (GenericBlockHeader MainBlockchain)) =>
         Blockchain (MainBlockchain v) where

    -- | Proof of everything contained in the payload.
    data BodyProof (MainBlockchain v) = MainProof
        { mpTxProof       :: !TxProof
        , mpMpcProof      :: !SscProof
        , mpProxySKsProof :: !(Hash (DlgPayload v))
        , mpUpdateProof   :: !UpdateProof
        } deriving (Eq, Show, Generic)

    data ConsensusData (MainBlockchain v) = MainConsensusData
        { -- | Id of the slot for which this block was generated.
          _mcdSlot       :: !SlotId
        , -- | Public key of the slot leader. It's essential to have it here,
          -- because FTS gives us only hash of public key (aka 'StakeholderId').
          _mcdLeaderKey  :: !PublicKey
        , -- | Difficulty of chain ending in this block.
          _mcdDifficulty :: !ChainDifficulty
        , -- | Signature given by slot leader.
          _mcdSignature  :: !BlockSignature
        } deriving (Generic, Show, Eq)

    type BBlockHeader (MainBlockchain v) = BlockHeader
    type ExtraHeaderData (MainBlockchain v) = MainExtraHeaderData

    -- | In our cryptocurrency, body consists of payloads of all block
    -- components.
    data Body (MainBlockchain v) = MainBody
        { -- | Txp payload.
          _mbTxPayload :: !TxPayload
        , -- | Ssc payload.
          _mbSscPayload :: !SscPayload
        , -- | Heavyweight delegation payload (no-ttl certificates).
          _mbDlgPayload :: !DlgPayload
          -- | Additional update information for the update system.
        , _mbUpdatePayload :: !UpdatePayload
        } deriving (Eq, Show, Generic, Typeable)

    type ExtraBodyData (MainBlockchain v) = MainExtraBodyData
    type BBlock (MainBlockchain v) = Block

    mkBodyProof MainBody{..} =
        MainProof
        { mpTxProof = mkTxProof _mbTxPayload
        , mpMpcProof = mkSscProof _mbSscPayload
        , mpProxySKsProof = hash _mbDlgPayload
        , mpUpdateProof = mkUpdateProof _mbUpdatePayload
        }

deriving instance Show MainToSign
deriving instance Eq MainToSign

instance Buildable (BodyProof (MainBlockchain v)) where
    build = genericF

instance NFData (BodyProof (MainBlockchain v))
instance NFData (ConsensusData (MainBlockchain v))
instance NFData (Body (MainBlockchain v))
instance NFData (MainBlock v)
