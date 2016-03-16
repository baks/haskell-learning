import ControlledVisit (Info(..), getInfo, traverse)

traverse' :: ([Info] -> [Info]) -> ([Info] -> [Info]) -> FilePath -> IO [Info] 
traverse' order filter path = do
                                 results <- ControlledVisit.traverse order path
                                 return (filter results)