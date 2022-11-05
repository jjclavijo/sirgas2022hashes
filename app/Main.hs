import RtkBind.Test

import Foreign.C.String
import Foreign (Storable(..),peekArray)

import Data.Map (toList)
import Data.Maybe (fromMaybe)

import Options.Applicative
import Data.Monoid ((<>))

data Args = Args {
  filename :: String
  }

args :: Parser Args
args = Args
     <$> argument str 
          ( metavar "FILENAME"
         <> help "Name of the file to Hash" )

hashFile :: Args -> IO ()
hashFile (Args fname) = do
  -- This should be options
  let ts = Gtime 0 0
  let te = Gtime 0 0
  emptyOpts <- newCString ""
  -- This should be options
  file <- newCString fname
  (n_, obs) <- readRinexT file 1 ts te 0.0 emptyOpts
  obsdata <- peek obs
  let largo = fromIntegral $ n obsdata
  manyObs <- peekArray largo $ _data obsdata
  obsTodo <- mapM readOneData manyObs
  let epochs = getEpochs obsTodo
  let (m1, ms1) = getMasks epochs
  let ums = uniformMasks m1 ms1
  putStrLn $ (show . hashMasksMap m1) (fromMaybe [] ums)

main :: IO ()
main = execParser opts >>= hashFile
  where
    opts = info (helper <*> args)
      ( fullDesc
     <> progDesc "Hashes a Rinex file Mask"
     <> header "hashrinex - eso" )

{-

-- Main construido para Tests

main :: IO ()
main = do
  -- let ts = ep2time [2005, 4, 2, 0, 15, 0] 
  -- let te = ep2time [2005, 4, 2, 0, 30, 0] 
  let ts = Gtime 0 0
  let te = Gtime 0 0
  --file <- newCString "../data/rinex/07590920.05o"
  file <- newCString "../data/rinex3/NABG00NOR_S_20220130000_01D_30S_MO.crx"
  emptyOpts <- newCString ""
  --(obs, nav, sta, n) <- readRinexT file 1 ts te 0.0 emptyOpts
  (n_, obs) <- readRinexT file 1 ts te 0.0 emptyOpts
  -- dumpObs obs
  obsdata <- peek obs
  obss <- peek $ _data obsdata
  putStrLn $ show n_
  putStrLn $ show obsdata
  putStrLn $ show obss
  obsSNR <- peekArray (fromIntegral arrlength) $ snr obss
  putStrLn $ show obsSNR
  obsLLI <- peekArray (fromIntegral arrlength) $ lli obss
  putStrLn $ show obsLLI
  obsL <- peekArray (fromIntegral arrlength) $ l obss
  putStrLn $ show obsL
  obsP <- peekArray (fromIntegral arrlength) $ p obss
  putStrLn $ show obsP
  obsD <- peekArray (fromIntegral arrlength) $ d obss
  putStrLn $ show obsD
  let obsT = obsTime obss
  putStrLn $ show obsT
  let largo = fromIntegral $ n obsdata
  manyObs <- peekArray largo $ _data obsdata
  putStrLn $ show manyObs
  putStrLn $ " Y Ahora..... "
  obsSNR <- arrayToData $ snr obss
  putStrLn $ show obsSNR
  obsL <- arrayToData $ l obss
  putStrLn $ show obsL
  putStrLn $ " Y Ahora MAS ----- "
  obsTodo <- mapM readOneData manyObs
  let epochs = getEpochs obsTodo
  putStrLn $ show epochs
  -- putStrLn $ show obsTodo
  -- manyShowableObs <- mapM readOneData manyObs
  -- putStrLn $ show manyShowableObs
  putStrLn $ " Y Ahora MAS ----- MAS "
  let (_,obsrs):_ = toList epochs
  let ms = getMaskSpec obsrs
  let mask = buildMask ms obsrs
  putStrLn $ show mask 
  putStrLn $ (show . (\(_,a) -> length a) . getMasks) epochs
  -- putStrLn $ (show . (getMasksSp . getMasks)) epochs
  --   where getMasksSp (e,ps) = [ x | (Mask x _) <- ps]
  let (m1, ms1) = getMasks epochs
  let ums = uniformMasks m1 ms1
  putStrLn $ (show . hashMasksMap m1) (fromMaybe [] ums)
-}
