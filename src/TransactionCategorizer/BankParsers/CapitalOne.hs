{-# LANGUAGE OverloadedStrings #-}

module TransactionCategorizer.BankParsers.CapitalOne where

import Data.Time (Day)
import qualified Data.Text as T
import Data.Csv
    ( (.:), FromNamedRecord(..), )
import TransactionCategorizer.Utils.Date (yyyymmddSkewerDateParser)
import qualified TransactionCategorizer.BankParsers.Transaction as Trans

data CapitalOneTransaction = MkCapitalOneTransaction {
    transactionDate :: Day,
    postDate :: Day,
    cardNo :: T.Text,
    description :: T.Text,
    category :: T.Text,
    debit :: Maybe Double,
    credit :: Maybe Double
} deriving (Show)

instance FromNamedRecord CapitalOneTransaction where
    parseNamedRecord r = MkCapitalOneTransaction 
        <$> yyyymmddSkewerDateParser "Transaction Date" r
        <*> yyyymmddSkewerDateParser "Posted Date" r
        <*> r .: "Card No."
        <*> r .: "Description"
        <*> r .: "Category"
        <*> r .: "Debit"
        <*> r .: "Credit"

toTransaction :: CapitalOneTransaction -> Trans.Transaction
toTransaction MkCapitalOneTransaction { transactionDate = capOneTransactionDate
                                  , description = capOneDescription
                                  , category = capOneCategory
                                  , debit = capOneDebit
                                  , credit = capOneCredit
                                  } = Trans.MkTransaction { Trans.transactionDate=capOneTransactionDate
                                                          , Trans.description=capOneDescription
                                                          , Trans.category=Just capOneCategory
                                                          , Trans.amount=amt
                                                          }
    where
        -- ignore payments for now, count them as 0.0
        amt = case (capOneDebit, capOneCredit) of
            (Just d, Nothing) -> d
            (Nothing, Just c) -> -c
            _ -> 0