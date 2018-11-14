{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RankNTypes                 #-}
-- TODO: Not sure about the best way to avoid the orphan instances here
{-# OPTIONS_GHC -fno-warn-orphans -Wno-redundant-constraints #-}

-- | Externally-owned sequential (EOS) HD wallets
module Cardano.Wallet.Kernel.DB.EosHdWallet (
    -- * Supporting types
    EosHdWallets(..)
  , EosHdAccount(..)
    -- ** Initialiser
  , initEosHdWallets
    -- ** Lenses
  , eosHdWalletsRoots
  , eosHdWalletsAccounts
  , eosHdWalletsAddresses
  , eosHdRootId
  , eosHdRootName
  , eosHdRootAssurance
  , eosHdRootAddressPoolGap
  , eosHdAccountId
  , eosHdAccountName
  , eosHdAccountState
  , eosHdAccountAutoPkCounter
  , eosHdAccountPublicKey
  ) where

import           Universum hiding ((:|))

import           Control.Lens.TH (makeLenses)
import qualified Data.IxSet.Typed as IxSet (Indexable (..))
import           Data.SafeCopy (base, deriveSafeCopy)

import           Test.QuickCheck (Arbitrary (..))

import qualified Pos.Crypto as Core

import           Cardano.Wallet.Kernel.DB.HdWallet
import           Cardano.Wallet.Kernel.DB.Util.AcidState ()
import           Cardano.Wallet.Kernel.DB.Util.IxSet
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet hiding (Indexable)

-- | Root of an externally-owned sequential HD wallet.
--
-- The wallet has sequentially assigned account indices and
-- sequentially assigned address indices.
data EosHdRoot = EosHdRoot {
      -- | Wallet ID
      _eosHdRootId             :: !HdRootId
      -- | Wallet name
    , _eosHdRootName           :: !WalletName
      -- | Assurance level
    , _eosHdRootAssurance      :: !AssuranceLevel
      -- | Address pool gap for this wallet
    , _eosHdRootAddressPoolGap :: !Word
    } deriving (Eq, Show)

instance Arbitrary EosHdRoot where
    arbitrary = EosHdRoot <$> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> pure (20 :: Word)

-- | Account in externally-owned sequential HD wallet
data EosHdAccount = EosHdAccount {
      -- | Account index
      _eosHdAccountId            :: !HdAccountId

      -- | Account name
    , _eosHdAccountName          :: !AccountName

      -- | Account state
      --
      -- When the account is up to date with the blockchain, the account state
      -- coincides with the state of a " wallet " as mandated by the formal
      -- spec.
    , _eosHdAccountState         :: !HdAccountState

      -- | A local counter used to generate new 'AutoIncrementKey' for
      -- addresses.
    , _eosHdAccountAutoPkCounter :: !AutoIncrementKey

      -- | Account's public key (we obtained it from the user during wallet creation).
    , _eosHdAccountPublicKey     :: !Core.PublicKey
    }

-- | All wallets, accounts and addresses in the EOS HD wallets
data EosHdWallets = EosHdWallets {
    _eosHdWalletsRoots     :: !(IxSet EosHdRoot)
  , _eosHdWalletsAccounts  :: !(IxSet EosHdAccount)
  , _eosHdWalletsAddresses :: !(IxSet (Indexed HdAddress))
  }

deriveSafeCopy 1 'base ''EosHdWallets
makeLenses ''EosHdWallets

initEosHdWallets :: EosHdWallets
initEosHdWallets = EosHdWallets IxSet.empty IxSet.empty IxSet.empty

{-------------------------------------------------------------------------------
  Template Haskell splices
-------------------------------------------------------------------------------}

makeLenses ''EosHdRoot
makeLenses ''EosHdAccount

deriveSafeCopy 1 'base ''EosHdRoot
deriveSafeCopy 1 'base ''EosHdAccount

{-------------------------------------------------------------------------------
  Derived lenses
-------------------------------------------------------------------------------}

eosHdAccountRootId :: Lens' EosHdAccount HdRootId
eosHdAccountRootId = eosHdAccountId . hdAccountIdParent

{-------------------------------------------------------------------------------
  IxSet instantiations
-------------------------------------------------------------------------------}

instance HasPrimKey EosHdRoot where
    type PrimKey EosHdRoot = HdRootId
    primKey = _eosHdRootId

instance HasPrimKey EosHdAccount where
    type PrimKey EosHdAccount = HdAccountId
    primKey = _eosHdAccountId

type SecondaryEosHdRootIxs    = '[]
type SecondaryEosHdAccountIxs = '[HdRootId]

type instance IndicesOf EosHdRoot    = SecondaryEosHdRootIxs
type instance IndicesOf EosHdAccount = SecondaryEosHdAccountIxs

instance IxSet.Indexable (HdRootId ': SecondaryEosHdRootIxs)
                         (OrdByPrimKey EosHdRoot) where
    indices = ixList

instance IxSet.Indexable (HdAccountId ': SecondaryEosHdAccountIxs)
                         (OrdByPrimKey EosHdAccount) where
    indices = ixList
                (ixFun ((:[]) . view eosHdAccountRootId))
