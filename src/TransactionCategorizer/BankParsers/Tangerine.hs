{-# LANGUAGE OverloadedStrings #-}

module TransactionCategorizer.BankParsers.Tangerine where

import Data.Time (Day)
import qualified Data.Text as T
import Data.Csv
    ( (.:), FromNamedRecord(..), )
import TransactionCategorizer.Utils.Date (mmddyyyyDateParser)
import qualified TransactionCategorizer.BankParsers.Transaction as Trans

data TangerineTransaction = MkTangerineTransaction {
    transactionDate :: Day,
    transaction :: T.Text,
    name :: T.Text,
    memo :: T.Text,
    amount :: Double
} deriving (Show)

instance FromNamedRecord TangerineTransaction where
    parseNamedRecord r = MkTangerineTransaction
        <$> mmddyyyyDateParser "Date" r
        <*> r .: "Transaction"
        <*> r .: "Name"
        <*> r .: "Memo"
        <*> r .: "Amount"

toTransaction :: TangerineTransaction -> Trans.Transaction
toTransaction MkTangerineTransaction { transactionDate = tangerineTransactionDate
                                     , transaction = tangerineTransaction
                                     , name = _
                                     , memo = _
                                     , amount = tangerineAmount
                                     } = Trans.MkTransaction { Trans.transactionDate=tangerineTransactionDate
                                                             , Trans.description=tangerineTransaction
                                                             , Trans.category=Nothing
                                                             , Trans.amount=tangerineAmount
                                                             }