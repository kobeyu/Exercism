module BankAccount ( BankAccount, openAccount, closeAccount
                   , getBalance, incrementBalance, transfer ) where
import Control.Concurrent.STM.TVar(TVar, newTVarIO,readTVar,writeTVar)
import Control.Monad.STM(atomically,STM)
import Prelude hiding(all)
import Data.Foldable(all)

type BankAccount = TVar (Maybe Integer)

openAccount :: IO BankAccount
openAccount = newTVarIO $ Just 0

closeAccount :: BankAccount -> IO ()
closeAccount b = atomically . writeTVar b $ Nothing

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance = atomically . readTVar 

incrementBalance  :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance = (atomically .) .  incrementBalanceSTM

canAlterBalanceBy :: BankAccount -> Integer -> STM Bool
canAlterBalanceBy b a = (fmap . all) (>= negate a) (readTVar b)

incrementBalanceSTM  :: BankAccount -> Integer -> STM (Maybe Integer)
incrementBalanceSTM b a = do
	ok <- canAlterBalanceBy b a
	if ok then do
		newVal <- (fmap . fmap) (+a) (readTVar b) --one fmap for STM, one for Maybe.
		writeTVar b newVal
		return newVal
	else return Nothing

--motivating function for using TVar over IORef:
transfer :: BankAccount -> BankAccount -> Integer -> IO Bool
transfer fromb tob a = atomically $ do
	fromOK <- canAlterBalanceBy fromb (negate a)
	toOK <- canAlterBalanceBy tob a
	if fromOK && toOK then do
		Just _ <- incrementBalanceSTM fromb (negate a)
		Just _ <- incrementBalanceSTM tob a
		return True
	else 
		return False
