import System.Random ( randomRIO )

stableParticles :: [String]
stableParticles = ["down quark", "down antiquark",
                  "gluon", "tau lepton", "antitau lepton", "charm quark",
                  "charm antiquark", "photon", "muon", "antimuon",
                  "positron" , "neutrino", "antineutrino",
                  "electron", "bottom quark", "bottom antiquark", "strange quark", "strange antiquark",
                  "up quark" , "up antiquark"]
                
unstableParticles :: [String]
unstableParticles = ["higgs boson","W boson","Z boson","top quark","top antiquark"]

shouldBreak :: (String, Int) -> Bool
shouldBreak (atom, outcome)
 | atom == "higgs boson" && outcome <= 433 = True
 | atom == "W boson" && outcome <= 500000 = True
 | atom == "Z boson" && outcome <= 500000 = True
 | atom == "top quark" && outcome <= 129500 = True
 | atom == "top antiquark" && outcome <= 129500 = True
 | otherwise = False 

breakAtom :: (String, Int) -> [String]
breakAtom (atom, outcome)
 | atom == "higgs boson" && outcome <= 648000 = ["bottom quark","bottom antiquark"]
 | atom == "higgs boson" && 648001 <= outcome && outcome <= 789000 = ["W boson" , "W boson"]
 | atom == "higgs boson" && 789001 <= outcome && outcome <= 877200 = ["gluon","gluon"]
 | atom == "higgs boson" && 877201 <= outcome && outcome <= 947600 = ["tau lepton", "antitau lepton"]
 | atom == "higgs boson" && 947601 <= outcome && outcome <= 980300 = ["charm quark","charm antiquark"]
 | atom == "higgs boson" && 980301 <= outcome && outcome <= 996200 = ["Z boson","Z boson"]
 | atom == "higgs boson" && 996201 <= outcome && outcome <= 998430 = ["photon","photon"]
 | atom == "higgs boson" && 998431 <= outcome && outcome <= 999540 = ["Z boson", "photon"]
 | atom == "higgs boson" && 999541 <= outcome && outcome <= 999784 = ["muon","antimuon"]
 | atom == "higgs boson" && 999785 <= outcome && outcome <= 1000000 = ["top quark","top antiquark"]
 | atom == "W boson" &&  outcome <= 333333 = ["positron","neutrino"]
 | atom == "W boson" &&  333334 <= outcome && outcome <= 666666 = ["antimuon","neutrino"]
 | atom == "W boson" &&  666667 <= outcome && outcome <= 1000000 = ["antitau lepton", "neutrino"]
 | atom == "Z boson" && outcome <= 206000 = ["neutrino", "antineutrino"]
 | atom == "Z boson" && 206001 <= outcome && outcome <= 240000 = ["electron","positron"]
 | atom == "Z boson" && 240001 <= outcome && outcome <= 274000 = ["muon","antimuon"]
 | atom == "Z boson" && 274001 <= outcome && outcome <= 308000 = ["tau lepton","antitau lepton"]
 | atom == "Z boson" && 308001 <= outcome && outcome <= 460000 = ["down quark","down antiquark"]
 | atom == "Z boson" && 460001<= outcome && outcome <= 612000 = ["strange quark","strange antiquark"]
 | atom == "Z boson" && 612001 <= outcome && outcome <= 764000 = ["bottom quark", "bottom antiquark"]
 | atom == "Z boson" && 764001 <= outcome && outcome <= 882000 = ["top quark","top antiquark"]
 | atom == "Z boson" && 882001 <= outcome && outcome <= 1000000 = ["charm quark","charm antiquark"]
 | atom == "top quark" &&  outcome <= 333333 = ["W boson","down quark"]
 | atom == "top quark" &&  333334 <= outcome && outcome <= 666666 = ["W boson","strange quark"]
 | atom == "top quark" &&  666667 <= outcome && outcome <= 1000000 = ["W boson","bottom quark"]
 | atom == "top antiquark" &&  outcome <= 333333 = ["W boson","down antiquark"]
 | atom == "top antiquark" &&  333334 <= outcome && outcome <= 666666 = ["W boson","strange antiquark"]
 | atom == "top antiquark" &&  666667 <= outcome && outcome <= 1000000 = ["W boson","bottom antiquark"]
 | otherwise = []

mutateUniverse :: [String] -> IO [String]
mutateUniverse universe = do
                          let stableOnes = [atom | atom <- universe, atom `elem` stableParticles]
                          let unstableOnes = [ atom | atom <- universe, atom `elem` unstableParticles]
                          outcomes1 <- mapM getRandom [1 .. length unstableOnes]
                          let breakingOnes = map fst $ filter shouldBreak (zip unstableOnes outcomes1)
                          let survivingOnes = map fst $ filter (not.shouldBreak) (zip unstableOnes outcomes1)
                          outcomes2 <- mapM getRandom [1 .. length breakingOnes]
                          let newOnes = concat [breakAtom (atom,out) | (atom,out) <- zip breakingOnes outcomes2]
                          return (stableOnes ++ survivingOnes ++ newOnes)

updateUniverse :: [String] -> Int -> IO String
updateUniverse universe iteration = do 
                     print universe
                     if all (`elem` stableParticles) universe
                     then return ("Reached stability in " ++ (show iteration :: String) ++ " moves!")
                     else do 
                          newUniverse <- mutateUniverse universe :: IO [String]
                          updateUniverse newUniverse (iteration + 1)
                          

readBeggining :: IO [String]
readBeggining =   do
                  putStr "Въведете начален брой: "
                  n <- getInt
                  putStr "Въведете вече началните атоми:\n"
                  mapM readString [1 .. n]
                

getInt :: IO Int
getInt = do read <$> getLine

getRandom :: Int ->  IO Int
getRandom n = do randomRIO (0, 1000000) :: IO Int
              
readString :: Int ->  IO String
readString n = do getLine

validInput :: IO [String]
validInput = do
             universe <- readBeggining :: IO [String]
             if any (\str -> (all (/= str) unstableParticles) && (all (/= str) stableParticles)) universe
             then do putStr "Моля въведете отново валидни атоми!\n"; validInput
             else return universe

main :: IO ()
main = do
       universe <- validInput :: IO [String]
       res <- updateUniverse universe 1 :: IO String
       print res
        
           
