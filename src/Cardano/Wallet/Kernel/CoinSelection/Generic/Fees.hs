{-# LANGUAGE BangPatterns #-}

module Cardano.Wallet.Kernel.CoinSelection.Generic.Fees (
    ExpenseRegulation(..)
  , FeeOptions(..)
  , adjustForFees
  ) where

import           Universum

import           Control.Monad.Trans.Except (Except)
import           Formatting (bprint)
import           Formatting.Buildable (Buildable (..))

import           Cardano.Wallet.Kernel.CoinSelection.Generic

{-------------------------------------------------------------------------------
  Top-level API
-------------------------------------------------------------------------------}

data ExpenseRegulation =
      SenderPaysFee
    -- ^ The sender pays for the fee. This is the typical case.
    | ReceiverPaysFee
    -- ^ The receiver pays for the fee. This is useful for cases
    -- where users wants to transfer funds between wallets owned by them,
    -- and they wish to trasfer an @exact@ amount (or, for example, the max
    -- amount).

data FeeOptions dom = FeeOptions {
      -- | Estimate fees based on number of inputs and values of the outputs
      foEstimate          :: Int -> [Value dom] -> Fee dom

      -- | Expense regulation (who pays the fees?)
    , foExpenseRegulation :: ExpenseRegulation
    }

-- | Given the coin selection result from a policy run, adjust the outputs
-- for fees, potentially returning additional inputs that we need to cover
-- all fees.
adjustForFees :: forall utxo m. (CoinSelDom (Dom utxo), Monad m)
              => FeeOptions (Dom utxo)
              -> (Value (Dom utxo) ->
                   CoinSelT utxo CoinSelHardErr m (Maybe (UtxoEntry (Dom utxo))))
              -> [CoinSelResult (Dom utxo)]
              -> CoinSelT utxo CoinSelHardErr m
                   ([CoinSelResult (Dom utxo)], SelectedUtxo (Dom utxo), Value (Dom utxo))
adjustForFees feeOptions pickUtxo css = do
    case foExpenseRegulation feeOptions of
      ReceiverPaysFee -> coinSelLiftExcept $
        (, emptySelection, valueZero) <$> receiverPaysFee upperBound css
      SenderPaysFee ->
        senderPaysFee pickUtxo upperBound css
  where
    upperBound = feeUpperBound feeOptions css

{-------------------------------------------------------------------------------
  Receiver pays fee
-------------------------------------------------------------------------------}

receiverPaysFee :: forall dom. CoinSelDom dom
                => Fee dom
                -> [CoinSelResult dom]
                -> Except CoinSelHardErr [CoinSelResult dom]
receiverPaysFee totalFee =
    mapM go . divvyFee (outVal . coinSelRequest) totalFee
  where
    go :: (Fee dom, CoinSelResult dom)
       -> Except CoinSelHardErr (CoinSelResult dom)
    go (fee, cs) =
        case outSubFee fee (coinSelRequest cs) of
          Just newOut ->
            return $ cs { coinSelOutput = newOut }
          Nothing ->
            throwError $
              CoinSelHardErrOutputCannotCoverFee (pretty (coinSelRequest cs)) (pretty fee)

{-------------------------------------------------------------------------------
  Sender pays fee
-------------------------------------------------------------------------------}

senderPaysFee :: (Monad m, CoinSelDom (Dom utxo))
              => (Value (Dom utxo) ->
                   CoinSelT utxo CoinSelHardErr m (Maybe (UtxoEntry (Dom utxo))))
              -> Fee (Dom utxo)
              -> [CoinSelResult (Dom utxo)]
              -> CoinSelT utxo CoinSelHardErr m
                   ([CoinSelResult (Dom utxo)], SelectedUtxo (Dom utxo), Value (Dom utxo))
senderPaysFee pickUtxo totalFee css = do
    let (css', remainingFee) = feeFromChange totalFee css
    (additionalUtxo, additionalChange) <- coverRemainingFee pickUtxo remainingFee
    return (css', additionalUtxo, additionalChange)

coverRemainingFee :: forall utxo m. (Monad m, CoinSelDom (Dom utxo))
                  => (Value (Dom utxo) -> CoinSelT utxo CoinSelHardErr m (Maybe (UtxoEntry (Dom utxo))))
                  -> Fee (Dom utxo)
                  -> CoinSelT utxo CoinSelHardErr m (SelectedUtxo (Dom utxo), Value (Dom utxo))
coverRemainingFee pickUtxo fee = go emptySelection
  where
    go :: SelectedUtxo (Dom utxo)
       -> CoinSelT utxo CoinSelHardErr m (SelectedUtxo (Dom utxo), Value (Dom utxo))
    go !acc
      | selectedBalance acc >= getFee fee =
          return (acc, unsafeValueSub (selectedBalance acc) (getFee fee))
      | otherwise = do
          mio <- (pickUtxo $ unsafeValueSub (getFee fee) (selectedBalance acc))
          io  <- maybe (throwError CoinSelHardErrCannotCoverFee) return mio
          go (select io acc)

-- | Attempt to pay the fee from change outputs, returning any fee remaining
--
-- NOTE: For sender pays fees, distributing the fee proportionally over the
-- outputs is not strictly necessary (fairness is not a concern): we could just
-- use the change of the first output to cover the entire fee (if sufficiently
-- large). Doing it proportionally however has the benefit that the fee
-- adjustment doesn't change the payment:change ratio too much, which may be
-- important for the correct operation of the coin selection policy.
--
-- NOTE: This does mean that /if/ the policy generates small outputs with
-- very large corresponding change outputs, we may not make optional use of
-- those change outputs and perhaps unnecessarily add additional UTxO entries.
-- However, in most cases the policy cares about the output:change ratio,
-- so we stick with this approach nonetheless.
feeFromChange :: forall dom. CoinSelDom dom
              => Fee dom
              -> [CoinSelResult dom]
              -> ([CoinSelResult dom], Fee dom)
feeFromChange totalFee =
    bimap identity unsafeFeeSum
    . unzip
    . map go
    . divvyFee (outVal . coinSelRequest) totalFee
  where
    -- | Adjust the change output, returning any fee remaining
    go :: (Fee dom, CoinSelResult dom) -> (CoinSelResult dom, Fee dom)
    go (fee, cs) =
        let (change', fee') = reduceChangeOutputs fee (coinSelChange cs)
        in (cs { coinSelChange = change' }, fee')

-- | Reduce the given change outputs by the total fee, returning the remainig
-- change outputs if any are left, or the remaining fee otherwise
--
-- As for the overall fee in 'feeFromChange', we divvy up the fee over all
-- change outputs proportionally, to try and keep any output:change ratio
-- as unchanged as possible
reduceChangeOutputs :: forall dom. CoinSelDom dom
                    => Fee dom -> [Value dom] -> ([Value dom], Fee dom)
reduceChangeOutputs totalFee cs =
    case divvyFeeSafe identity totalFee cs of
        Nothing ->
            (cs, totalFee)
        Just xs ->
            bimap identity unsafeFeeSum
            . unzip
            . map go
            $ xs
  where
    -- Reduce single change output, returning remaining fee
    go :: (Fee dom, Value dom) -> (Value dom, Fee dom)
    go (fee, change)
      | change >= getFee fee =
          (unsafeValueSub change (getFee fee), Fee valueZero)
      | otherwise =
          (valueZero, adjustFee (`unsafeValueSub` change) fee)

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

feeUpperBound :: CoinSelDom dom
              => FeeOptions dom -> [CoinSelResult dom] -> Fee dom
feeUpperBound FeeOptions{..} css =
    foEstimate numInputs outputs
  where
    numInputs = fromIntegral $ sum (map (sizeToWord . coinSelInputSize) css)
    outputs   = concatMap coinSelOutputs css

-- | divvy fee across outputs, discarding zero-output if any. Returns `Nothing`
-- when there's no more outputs after filtering, in which case, we just can't
-- divvy fee.
divvyFeeSafe
    :: forall dom a. CoinSelDom dom
    => (a -> Value dom)
    -> Fee dom
    -> [a]
    -> Maybe [(Fee dom, a)]
divvyFeeSafe f fee as = case filter ((/= valueZero) . f) as of
    []  -> Nothing
    as' -> Just (divvyFee f fee as')

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance Buildable ExpenseRegulation where
    build SenderPaysFee   = bprint "SenderPaysFee"
    build ReceiverPaysFee = bprint "ReceiverPaysFee"
