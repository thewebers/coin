import Test.HUnit

import Control.Monad
import Data.Time.Clock.POSIX

import Lib
import Worker
-- import Main


-- TODO testInsufficientlyFundedTransactionRejected

-- TODO testMinedBlocksHaveLeadingZeroHash

testMinerReceivesCoins = TestCase $ do
  person <- createPerson
  chain <- mineNBlocks person emptyChain 3
  let wallet = getWallet chain person
  assertEqual "" (walletAmount wallet) 3

testDoubleSpend = TestCase $ do
    sender <- createPerson
    receiver <- createPerson
    chain <- mineNBlocks sender emptyChain 1
    -- TODO:  Some try-catch hot shit.
    -- [( | i <- [0..2]]
    -- loop
    -- let tx = send chain sender (publicKey receiver) 1
    -- chain' <- mineSingle sender [tx] chain
    -- --
    let tx = send chain sender (publicKey receiver) 1
    chain' <- mineSingle sender [tx, tx] chain
    --
    return ()

testRoundTripPubKeySerialize = undefined

testSaveAndLoadChain = undefined

tests = TestList [
  TestLabel "testMinerReceivesCoins" testMinerReceivesCoins,
  TestLabel "testDoubleSpend" testDoubleSpend
  -- TestLabel "testSaveAndLoadChain" testSaveAndLoadChain
  ]

main :: IO ()
main = do
  runTestTT tests
  return ()
