{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

import Data.Bits
import Data.List

import Parser

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret path1 path2 = 
  do
    file1 <- BS.readFile path1
    file2 <- BS.readFile path2
    return (BS.pack . (filter (\x -> x /= 0)) . (map (\(w1, w2) -> w1 `xor` w2)) $ (BS.zip file1 file2))
    

-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key decryptedPath = 
  let encryptedPath = decryptedPath ++ ".enc" in
  do
    cyperText <- BS.readFile encryptedPath
    let cyperTexts = BS.unpack cyperText
    let keys = cycle (BS.unpack key)
    let decryptedTexts = map (
                               \(w1, w2) -> w1 `xor` w2
                             ) (zip cyperTexts keys)
    let decryptedText = BS.pack decryptedTexts
    BS.writeFile decryptedPath decryptedText

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile path = 
  do
    jsonText <- BS.readFile path
    return (decode jsonText)

-- Exercise 4 -----------------------------------------

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs victimsPath transactionDataPath = 
  do
    victimsData <- parseFile victimsPath
    let _ = victimsData `asTypeOf` (Just [""])
    case victimsData of 
      Nothing -> return Nothing
      Just victims -> do
        transactionData <- parseFile transactionDataPath
        case transactionData of 
          Nothing -> return Nothing
          Just transactions -> do
            let isVictim = \x -> any (\y -> y == x) victims
            let badTs = filter (\t -> isVictim (tid t)) transactions
            return (if (length badTs) == 0 then Nothing else Just badTs)
    

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow ts = 
  foldl (
          \m t -> 
            let fromUpdated = Map.alter (
                                          \x -> case x of 
                                            Just b -> Just (b - (amount t))
                                            Nothing -> Just (-(amount t))
                                        ) (from t) m in
              Map.alter (
                          \x -> case x of 
                            Just b -> Just (b + (amount t))
                            Nothing -> Just (amount t)
                        ) (to t) fromUpdated
        ) Map.empty ts

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal flowMap = 
  let (criminal, _) = Map.foldlWithKey (
                                         \(maxName, maxFlow) name flow -> 
                                           if flow > maxFlow then (name, flow)
                                           else (maxName, maxFlow)
                                       ) ("", 0) flowMap in
    criminal

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs flowMap tids = 
  let (payers, payees) = Map.foldlWithKey (
                                            \(cPayers, cPayees) name flow -> 
                                              if flow > 0 then
                                                ((name, flow) : cPayers, cPayees)
                                              else
                                                (
                                                  if flow < 0 then
                                                    (cPayers, (name, -flow) : cPayees)
                                                  else
                                                    (cPayers, cPayees)
                                                )
                                          ) ([], []) flowMap in
  let sorter (_, f1) (_, f2) = compare f1 f2 in
  let sortedPayers = reverse . (sortBy sorter) $ payers in
  let sortedPayees = reverse . (sortBy sorter) $ payees in
    buildTrxs sortedPayers sortedPayees tids []
      where 
        buildTrxs [] _ _ trxs = trxs
        buildTrxs _ [] _ trxs = trxs
        buildTrxs _ _ [] trxs = trxs -- To make total func
        buildTrxs (curPayer : remainPayers) (curPayee : remainPayees) (curTids : remainTids) trxs = 
          case (curPayer, curPayee) of
            ((payerName, payerFlow), (payeeName, payeeFlow)) | payerFlow > payeeFlow -> 
              let newTrx = Transaction payerName payeeName payeeFlow curTids in
                buildTrxs ((payerName, payerFlow - payeeFlow) : remainPayers) remainPayees remainTids (trxs ++ [newTrx])
            ((payerName, payerFlow), (payeeName, payeeFlow)) | payerFlow < payeeFlow -> 
              let newTrx = Transaction payerName payeeName payerFlow curTids in
                buildTrxs remainPayers ((payeeName, payeeFlow - payerFlow) : remainPayees) remainTids (trxs ++ [newTrx])
            ((payerName, payerFlow), (payeeName, _)) -> 
              let newTrx = Transaction payerName payeeName payerFlow curTids in
                buildTrxs remainPayers remainPayees remainTids (trxs ++ [newTrx])

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON outFilePath ts = BS.writeFile outFilePath (encode ts)

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts       
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <- 
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim

