{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}

module TransactionCategorizer.BankParsers.Transaction where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Csv (ToNamedRecord(..), FromNamedRecord(..), (.=), (.:), encodeByName, namedRecord)
import Data.Time (Day)
import Data.Text
import TransactionCategorizer.Utils.ByteString (charToWord8, stringToByteString)
import Data.String
import TransactionCategorizer.BankParsers.Other.Day()
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Aeson as Aeson
import GHC.Generics
import qualified Data.Vector as V


-- Our internal representation of a transaction. We convert from
-- different bank transaction csv formats to this type.
data Transaction = MkTransaction {
  transactionDate :: Day
  , description :: Text
  , category :: Maybe Text
  , amount :: Double
} deriving (Show, Generic)

instance FromNamedRecord Transaction where
  parseNamedRecord r = MkTransaction
    <$> r .: "transactionDate"
    <*> r .: "description"
    <*> r .: "category"
    <*> r .: "amount"

instance ToNamedRecord Transaction where
  toNamedRecord (MkTransaction date desc cat amt) = namedRecord
    [ "transactionDate" .= show date
    , "description"     .= desc
    , "category"        .= cat
    , "amount"          .= amt
    ]

-- Typeclass to allow bank -> golden transaction conversions
class ToTransaction a where
  toTransaction :: a -> Transaction

-- Add new bank types here
data BankType = ChaseBank | WellsFargoBank | CitiBank | CapitalOneBank | AmericanExpressBank | TangerineBank | UnknownBank deriving (Show, Eq)

detectBankType :: BS.ByteString -> BankType
detectBankType csvBS
  | isChaseHeader headers = ChaseBank
  | isCitiHeader headers = CitiBank
  | isCapitalOneHeader headers = CapitalOneBank
  | isAmexSimpleHeader headers = AmericanExpressBank
  | isTangerineHeader headers = TangerineBank
  -- Banks with no header - must be parsed last. Otherwise, they might be miscategorized (for example, Citi and WF both have 4 headers / 4 commas)
  | isWfHeader headers = WellsFargoBank
  | otherwise = UnknownBank
  where
    headers = BS8.takeWhile (/= '\n') csvBS

-- Helper detection functions
isChaseHeader :: BS.ByteString -> Bool
isChaseHeader headers = headers == (stringToByteString "Transaction Date,Post Date,Description,Category,Type,Amount,Memo")

isWfHeader :: BS.ByteString -> Bool
isWfHeader headers = BS.count (charToWord8 ',') headers == 4

isCitiHeader :: BS.ByteString -> Bool
isCitiHeader headers = headers == (stringToByteString "Status,Date,Description,Debit,Credit")

isCapitalOneHeader :: BS.ByteString -> Bool
isCapitalOneHeader headers = headers == (stringToByteString "Transaction Date,Posted Date,Card No.,Description,Category,Debit,Credit")

isAmexSimpleHeader :: BS.ByteString -> Bool
isAmexSimpleHeader headers = headers == (stringToByteString "Date,Description,Amount")

isTangerineHeader :: BS.ByteString -> Bool
isTangerineHeader headers = headers == (stringToByteString "Date,Transaction,Name,Memo,Amount")

updateTransactionCategory :: Transaction -> Text -> Transaction
updateTransactionCategory MkTransaction { transactionDate=transactionDate
                                        , description=description
                                        , category=_oldCategory
                                        , amount=amount
                                        } newCategory = MkTransaction { transactionDate
                                                                      , description
                                                                      , category = Just newCategory
                                                                      , amount
                                                                      }

instance Aeson.ToJSON Transaction

toCsv :: V.Vector Transaction -> BL.ByteString
toCsv transactions = encodeByName header $ V.toList transactions
  where
    header = V.fromList ["transactionDate", "description", "category", "amount"]