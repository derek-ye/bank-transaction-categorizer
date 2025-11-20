{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.CategorizeTransactions where

import Import hiding ((.), zip)
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Csv as Csv
import TransactionCategorizer.BankParsers.Transaction
import qualified TransactionCategorizer.BankParsers.Chase as Chase
import qualified TransactionCategorizer.BankParsers.WellsFargo as WellsFargo
import qualified TransactionCategorizer.BankParsers.Citi as Citi
import qualified TransactionCategorizer.BankParsers.CapitalOne as CapOne
import qualified TransactionCategorizer.BankParsers.AmericanExpress as Amex
import qualified TransactionCategorizer.BankParsers.Tangerine as Tangerine
import TransactionCategorizer.Core.Categorizer (categorizeTransactions)
import qualified Data.Vector as V
import TransactionCategorizer.Utils.Csv
import Network.Wai

newtype CategorizeTransactionsResult = CategorizeTransactionsResult {
    categorizedTransactions :: Map.Map Text Text
} deriving (Generic, Show)

instance ToJSON CategorizeTransactionsResult

postCategorizeTransactionsR :: Handler TypedContent
postCategorizeTransactionsR = do
    app <- getYesod
    let openaiKey = appOpenAiKey $ appSettings app

    -- Get raw request body as ByteString
    req <- waiRequest
    rawBody <- fmap LBS.toStrict $ strictRequestBody
    let csvBS = rawBody

    let bankType = detectBankType csvBS
    let result = case bankType of
                ChaseBank -> chaseHandler $ removeHeader $ Csv.decodeByName $ LBS.fromStrict csvBS
                CitiBank -> citiHandler $ removeHeader $ Csv.decodeByName $ LBS.fromStrict csvBS
                CapitalOneBank -> capitalOneHandler $ removeHeader $ Csv.decodeByName $ LBS.fromStrict csvBS
                WellsFargoBank -> wfHandler $ Csv.decode Csv.NoHeader $ LBS.fromStrict csvBS
                AmericanExpressBank -> Amex.amexHandler $ removeHeader $ Csv.decodeByName $ LBS.fromStrict csvBS
                TangerineBank -> tangerineHandler $ removeHeader $ Csv.decodeByName $ LBS.fromStrict csvBS
                UnknownBank -> error "Unknown bank"
    recategorizedTransactions <- liftIO $ recategorizeTransactions openaiKey result
    let csv = toCsv recategorizedTransactions
    pure $ TypedContent "text/csv" $ toContent csv
    where
        chaseHandler :: Either String (Vector Chase.ChaseTransaction) -> Vector Transaction
        chaseHandler (Left e) = error $ "Failed to parse Chase csv: " <> e
        chaseHandler (Right transactions) = Chase.toTransaction <$> transactions

        wfHandler :: Either String (Vector WellsFargo.WellsFargoTransaction) -> Vector Transaction
        wfHandler (Left e) = error $ "Failed to parse Wells Fargo csv: " <> e
        wfHandler (Right transactions) = WellsFargo.toTransaction <$> transactions

        citiHandler :: Either String (Vector Citi.CitiTransaction) -> Vector Transaction
        citiHandler (Left e) = error $ "Failed to parse Citi csv: " <> e
        citiHandler (Right transactions) = Citi.toTransaction <$> transactions

        capitalOneHandler :: Either String (Vector CapOne.CapitalOneTransaction) -> Vector Transaction
        capitalOneHandler (Left e) = error $ "Failed to parse Capital One csv: " <> e
        capitalOneHandler (Right transactions) = CapOne.toTransaction <$> transactions

        tangerineHandler :: Either String (Vector Tangerine.TangerineTransaction) -> Vector Transaction
        tangerineHandler (Left e) = error $ "Failed to parse Tangerine csv: " <> e
        tangerineHandler (Right transactions) = Tangerine.toTransaction <$> transactions

recategorizeTransactions :: Text -> Vector Transaction -> IO (Vector Transaction)
recategorizeTransactions openaiKey transactions = do
    -- must parse this into a maybe
    categories <- categorizeTransactions openaiKey $ toList (description <$> transactions)

    -- Separate out lines below as a pure function for better stubbing (dont throw in pure code in your core)
    -- and use `guard` with Either (underscore throws away the underscored variable)
    let _ = if (V.length transactions /= V.length categories) then error $ "Different number of transactions and categories: " <> show (V.length transactions) <> " " <>  show (V.length categories) else Prelude.undefined
    pure $ fmap createCategorizedTransactions (V.zip transactions categories)
    where
        createCategorizedTransactions :: (Transaction, Text) -> Transaction
        createCategorizedTransactions (t, c) = updateTransactionCategory t c
