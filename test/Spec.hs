import Test.HUnit

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Exception as Ex
import Control.Monad
import Data.Time.Clock.POSIX

import Lib
import Miner
-- import Main

import qualified Data.Serialize as Cereal
import qualified Data.ByteString as BS

-- TODO testManyMinersDoingSickTricks
-- TODO testInsufficientlyFundedTransactionRejected
-- TODO testMinedBlocksHaveLeadingZeroHash


testMineManyInTime = TestCase $ do
  let chainThresh = 10000
  sender <- createPerson
  receiver <- createPerson
  startTime <- mkTimestamp
  chain <- atomically emptyChain
  chain' <- atomically $ readTVar $ chain 

  -- Repeat some shit
  take 10 $ iterate (\_ -> do
    let tx = mkTransaction chain sender (psnPublicKey receiver) 1
    chain''' <- mineSingle sender [tx] chain
    -- TODO
    return ()
  ) 0

  endTime <- mkTimestamp
  let duration = fromIntegral (endTime - startTime) / 1e9

  -- TODO assert time less than duration
  putStrLn $ "Duration: " ++ show duration

-- testMinerReceivesCoins = TestCase $ do
--   person <- createPerson
--   chain <- mineNBlocks person emptyChain 3
--   let wallet = getWallet chain person
--   assertEqual "" (wltAmount wallet) 3

-- testDoubleSpend = TestCase $ do
--     sender <- createPerson
--     receiver <- createPerson
--     chain <- mineNBlocks sender emptyChain 1
--     let tx = mkTransaction chain sender (psnPublicKey receiver) 1
--     result <- try (mineSingle sender [tx, tx] chain) :: IO (Either Ex.SomeException Chain)
--     case result of
--         Left ex -> putStrLn "nice"
--         Right chain -> error "fuck"
--     return ()

-- testRoundTripPubKeySerialize = TestCase $ do
--   person <- createPerson
--   let putter = Cereal.put person
--   let byteStr = Cereal.runPut putter
--   let maybePerson' = Cereal.runGet (Cereal.get :: Cereal.Get Person) byteStr
--   case maybePerson' of
--     Left msg -> assertFailure msg
--     Right person' -> assertEqual "" person' person

-- testSaveAndLoadChain = TestCase $ do
--   person <- createPerson
--   chain <- mineNBlocks person emptyChain 3
--   let serChain = Cereal.runPut . Cereal.put $ chain
--   BS.writeFile "blockchain.bin" serChain
--   serChain' <- BS.readFile "blockchain.bin"
--   let maybeChain' = Cereal.runGet (Cereal.get :: Cereal.Get Chain) serChain'
--   case maybeChain' of
--     Left msg -> assertFailure msg
--     Right chain' -> assertEqual "" chain' chain
--   return ()

tests = TestList [
  TestLabel "testMineManyInTime" testMineManyInTime

--   TestLabel "testMineManyInTime" testMineManyInTime,
--   TestLabel "testMinerReceivesCoins" testMinerReceivesCoins,
--   TestLabel "testDoubleSpend" testDoubleSpend,
--   TestLabel "testRoundTripPubKeySerialize" testRoundTripPubKeySerialize,
--   TestLabel "testSaveAndLoadChain" testSaveAndLoadChain
  ]

main :: IO ()
main = do
  runTestTT tests
  return ()
