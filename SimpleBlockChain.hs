import Data.Hashable
import Data.List
import System.IO

type SenderName = String
type RecieverName = String
type Amount = Double

type Details = String -- transaction Details
type CurrentHash = String
type PreviousHash = String
type OverAllDetails = (Details, CurrentHash, PreviousHash) 
type SampleBlockChain = [OverAllDetails]

fstOfTriple :: Details -> OverAllDetails -> OverAllDetails
fstOfTriple x (_, _, _) = (x, _, _) -- this is to set the first element from tuple, details of transaction

sndOfTriple :: CurrentHash -> OverAllDetails -> OverAllDetails
sndOfTriple y (_, _, _) = (_, y, _) -- this is to set the second element from tuple, current has

gsndOfTriple :: OverAllDetails -> CurrentHash
gsndOfTriple (_, y, _) = y  -- this is to get the second element from tuple, current has

trdOfTriple :: PreviousHash-> OverAllDetails -> OverAllDetails
trdOfTriple z (_, _, _) = (_, _, z) -- this is to set the third element from tuple, previoushash

emptyBlockChain :: Details-> OverAllDetails -> SampleBlockChain -> SampleBlockChain
emptyBlockChain details oDetails blockChain = 
    fstOfTriple details oDetails
    sndOfTriple (hash details) oDetails
    trdOfTriple show 0 oDetails
    blockChain ++ oDetails

nonEmptyBlockChain :: Details -> OverAllDetails -> SampleBlockChain -> SampleBlockChain
nonEmptyBlockChain details oDetails blockChain = 
    fstOfTriple details oDetails
    sndOfTriple (hash details) oDetails
    trdOfTriple (gsndOfTriple oDetails) oDetails
    blockChain ++ oDetails

generateTransactionDetails :: SenderName-> RecieverName -> Amount -> Details --this method generates transaction details as a string
generateTransactionDetails sName rName amount = 
    sName ++ rName ++ show amount

insertTransaction :: Details-> OverAllDetails -> SampleBlockChain-> SampleBlockChain
insertTransaction details oDetails blockChain=  
    if(length blockChain == 0) --genisis block
        then emptyBlockChain details oDetails blockChain
    else  
        nonEmptyBlockChain details oDetails blockChain

printBlockChain :: String -> IO()
printBlockChain sBChain = putStrLn sBChain
 
main = do 
    let oDetails = OverAllDetails
    let blockChain = SampleBlockChain
    putStr("Enter Sender's Name")
    sname <- getLine
    putStr("Enter Reciever's Name")
    rname <- getLine
    putStr("Enter amountDetails")
    amountAsString <- getLine
    let amount = read amountAsString::Double    
    insertTransaction(GeneratorTransaction(sname, rname, amount), oDetails. blockChain)   
    printBlockChain(blockChain)
