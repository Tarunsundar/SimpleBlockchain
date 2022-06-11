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

fstOfTriple x (_, _, _) = (x, _, _) -- this is to set the first element from tuple,

sndOfTriple y (_, _, _) = (_, y, _) -- this is to set the second element from tuple, current has

gsndOfTriple (_, y, _) = y  -- this is to get the second element from tuple, current has

trdOfTriple z (_, _, _) = (_, _, z) -- this is to set the third element from tuple, previoushash

emptyBlockchain :: Details-> OverAllDetails -> SampleBlockChain -> SampleBlockChain
emptyBlockchain details oDetails blockChain = 
    fstOfTriple (details, oDetail)
    sndOfTriple hash details oDetail
    trdOfTriple(show 0, oDetail)
    blockChain ++ oDetail

nonEmptyBlockchain :: Details -> OverAllDetails -> SampleBlockChain -> SampleBlockChain
nonEmptyBlockChain details oDetails blockchain = 
    fstOfTriple (details, oDetail)
    sndOfTriple hash details oDetail
    trdOfTriple(gsndOfTriple(prevElement), oDetail)
    blockChain ++ oDetail

generateTransactionDetails :: senderName-> RecieverName -> Amount -> Details 
--this method generates transaction details as a string
generateTransactionDetails sName rName amount = 
    sName ++ rName ++ show amount

insertTransaction :: Details-> OverAllDetails -> SampleBlockChain-> SampleBlockChain
insertTransaction details oDetail blockChain=  
    if(length blockChain == 0) --genisis block
        then emptyBlockchain(details, oDetails, blockChain)
    else  
        nonEmptyBlockchain(details, oDetails, blockChain)

printBlockChain :: SampleBlockChain -> IO()
printBlockChain sBChain = sequence putStrnLn(sBChain)
 
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
