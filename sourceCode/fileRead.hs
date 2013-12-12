import System.IO  
import System.Directory (doesFileExist)

--- main program
main = do
  print "\nPlease enter the name of the file with the message or 'quit' to exit: "
  fileName <- getLine
  if fileName == "quit" then
      return ()
  else do
      yn <- doesFileExist fileName 
      if yn then do
          print "Please enter the mesage: "
          input <- getLine
          inputWords <- (words input)
          handle <- openFile fileName ReadMode
          fileContents <- hGetContents handle
          wordsInFile <- (words contents)
          -- get the words in the file
          let simWords =  haveWords inputWords wordsInFile 
          let message  = "The missing words are "++ (intercalate "and"  $ missingWords simWords inputWords)   
          main     
      else do 
          print "The file you entered does not exist. Please try again..."
          main
 
--- identify similar words in the inout and the file
haveWords :: [String] -> [String] -> [String]
haveWords xs ys = [ x | x <- xs, y <- ys, x == y ] 

--- id
missingWords :: [String] -> [String] -> [String] 
missingWords [] ys = ys
missingWords (x:xs) (y:ys) 
    | x == y       =  missingWords xs ys
    | otherwise    =  y :  missingWords xs ys
missingWords _ _   = []
