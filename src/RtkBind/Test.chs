{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-} 

module RtkBind.Test (-- uTest1,uTest2,uTest3,
                     -- uTest4,uTest5,uTest6, 
                     ep2time, Gtime (..), readRinexT, dumpObs, ObsData(..), 
                     ObsOneData(..), arrlength,
                     arrayToData, readOneData, oneObsToRecord, getEpochs, getMaskSpec, buildMask, getMasks, Mask (..), uniformMasks, hashMasks, hashMasksMap
                     ) where

import Control.Applicative ((<$>), (<*>))
import GHC.Generics (Generic(..))
import Foreign (Storable(..), newArray,
                withArray, Ptr,
                alloca, with, peekArray)
import Foreign.C.Types
import Foreign.C.String

import Data.Map (Map, fromListWith, fromList)
import qualified Data.Map (foldr)
import Data.List (nub,foldl')
import Control.Monad (guard)
import Data.Maybe (isNothing,isJust,catMaybes)

import Data.Bits (setBit)
import Data.Word (Word8)

import Crypto.Hash (hash, Digest (..), SHA256)
import Data.ByteString (pack, ByteString)


#include <t_rinex.h>

{#pointer *obsd_t as PtrObsOneData->ObsOneData #}

nfreq = {#const NFREQ #}
nexobs = {#const NEXOBS #}
arrlength = nfreq + nexobs

aLength :: Int
aLength = fromIntegral arrlength

type SNRArray = Ptr CUShort
type LLIArray = Ptr CUChar
type CodeArray = Ptr CUChar
type LArray = Ptr CDouble
type PArray = Ptr CDouble
type DArray = Ptr CFloat

type SNRdata = [Int]
type LLIdata = [Int]
type CodeData = [Int]
type Ldata = [Double]
type Pdata = [Double]
type Ddata = [Double]

-- Esto se llama FunctionalDependences + ConstrainedClassMethods 
-- + ¿ MultiParamTypeClasses ?
class DataArray a b | a -> b where
  arrayToData :: (Storable a) => Ptr a -> IO [b]

instance DataArray CUShort Int where
  arrayToData arr = (map fromIntegral <$> peekArray aLength arr :: IO [Int])

instance DataArray CUChar Int where
  arrayToData arr = (map fromIntegral <$> peekArray aLength arr :: IO [Int])

instance DataArray CDouble Double where
  arrayToData arr = (map realToFrac <$> peekArray aLength arr :: IO [Double])

instance DataArray CFloat Double where
  arrayToData arr = (map realToFrac <$> peekArray aLength arr :: IO [Double])

data ObsOneData = ObsOneData {
  obsTime :: Gtime,
  sat :: CUChar,
  rcv :: CUChar,
  snr :: SNRArray,
  lli :: LLIArray,
  code :: CodeArray,
  l :: LArray,
  p :: PArray,
  d :: DArray
  } deriving (Show, Eq)

instance Storable ObsOneData where
 peek ptr  = ObsOneData
            <$> (\ptr -> do {peekByteOff ptr {#offsetof obsd_t->time #} :: IO Gtime}) ptr
            -- <$> {#get obsd_t->time #} ptr
            <*> {#get obsd_t->sat #} ptr
            <*> {#get obsd_t->rcv #} ptr
            <*> {#get obsd_t->SNR #} ptr
            <*> {#get obsd_t->LLI #} ptr
            <*> {#get obsd_t->code #} ptr
            <*> {#get obsd_t->L #} ptr
            <*> {#get obsd_t->P #} ptr
            <*> {#get obsd_t->D #} ptr
 -- poke = esta implementación probablemente este mal por la diferencia de arrays y puntros, pero no la estoy usando en realidad.
 poke ptr dat = do
            (\ptr val -> do {pokeByteOff ptr 0 (val :: Gtime)}) ptr (obsTime dat)
            {#set obsd_t->sat #} ptr (sat dat)
            {#set obsd_t->rcv #} ptr (rcv dat)
            {#set obsd_t->SNR #} ptr (snr dat)
            {#set obsd_t->LLI #} ptr (lli dat)
            {#set obsd_t->code #} ptr (code dat)
            {#set obsd_t->L #} ptr (l dat)
            {#set obsd_t->P #} ptr (p dat)
            {#set obsd_t->D #} ptr (d dat)
 alignment _ = {#alignof obsd_t #}
 sizeOf _ = {#sizeof obsd_t #}

data ObsOneDataH = ObsOneDataH {
  obsTimeH :: Gtime,
  satH :: Int,
  rcvH :: Int,
  snrH :: SNRdata,
  lliH :: LLIdata,
  codeH :: CodeData,
  lH :: Ldata,
  pH :: Pdata,
  dH :: Ddata
  } deriving (Show, Eq)

readOneData :: ObsOneData -> IO ObsOneDataH
readOneData dat = ObsOneDataH 
                    (obsTime dat) 
                    (fromIntegral $ sat  dat) 
                    (fromIntegral $ rcv  dat) 
                <$> (arrayToData  $ snr  dat)
                <*> (arrayToData  $ lli  dat)
                <*> (arrayToData  $ code dat)
                <*> (arrayToData  $ l    dat)
                <*> (arrayToData  $ p    dat)
                <*> (arrayToData  $ d    dat) 

-- sat code snr lli l p d (?)
data SingleRecord = SingleRecord Int Int Int Int Double Double Double
  deriving (Show,Eq)

-- Char is for 'S'snr 'K'lli 'L' 'P' 'D'
data SingleData = SingleInt Int Int Char Int 
                | SingleDouble Int Int Char Double
  deriving (Show,Eq)

recordToData :: SingleRecord -> [SingleData]
recordToData (SingleRecord a b c d e f g) = 
  [ SingleInt a b h i | (h, i) <- zip "SK" (c:d:[]), i /= 0 ] ++
  [ SingleDouble a b h i | (h, i) <- zip "LPD" (e:f:g:[]), i /= 0 ]

dataToCodes :: [SingleData] -> [(Int,Char)]
dataToCodes = map getCode
  where getCode (SingleInt _ a b _) = (a,b)
        getCode (SingleDouble _ a b _) = (a,b)

oneObsToRecord :: ObsOneDataH -> [SingleRecord]
oneObsToRecord dat = [ x | x <- toList dat, testZero x]
                    where 
                      toList dat = SingleRecord  
                        (satH dat) <$> (codeH $ dat)
                        <*> (snrH $ dat) <*> (lliH $ dat)
                        <*> (lH $ dat) <*> (pH $ dat) <*> (dH $ dat)
                      (<*>) = zipWith ($)
                      (<$>) = map 
                      testZero (SingleRecord a 0 0 0 0 0 0) = False
                      testZero (SingleRecord a b c d e f g) = True

getEpochs :: [ObsOneDataH] -> Map Gtime [SingleRecord]
getEpochs dat = fromListWith (++) $ map unpack dat
          where
            unpack obs = (obsTimeH obs, oneObsToRecord obs)

data MaskSpec = MaskSpec [Int] [(Int,Char)] 
  deriving Show
data Mask = Mask MaskSpec [[Bool]]
  deriving Show

newtype BareMask = BareMask [[Bool]]
  deriving Show

updateMaskSpec :: MaskSpec -> MaskSpec -> MaskSpec
updateMaskSpec ma mb = MaskSpec (satsAB ma mb) (codesAB ma mb)
        where satsAB (MaskSpec a _) (MaskSpec b _) = nub (a ++ b)
              codesAB (MaskSpec _ a) (MaskSpec _ b) = nub (a ++ b)

getMaskSpec :: [SingleRecord] -> MaskSpec
getMaskSpec records = MaskSpec sats codes
         where sats = nub [ x | (SingleRecord x _ _ _ _ _ _) <- records]
               codes = (nub . concat) $ map (dataToCodes . recordToData) records
               -- codes = nub [ x | (SingleRecord _ x _ _ _ _ _) <- records]

buildMask :: MaskSpec -> [SingleRecord] -> Mask
buildMask ms@(MaskSpec sats codes) records = 
  Mask ms [[ (s, c) `elem` sparse | s <- sats] | c <- codes ]  
    where sparse = (concat . (map (\(x,xs) -> map ((,) x) xs)))
            $ zip
            ( map (\(SingleRecord x _ _ _ _ _ _) -> x) records )
            ( map (dataToCodes . recordToData) records )

-- [ (x,y) | (SingleRecord x y _ _ _ _ _) <- records ]

getMasks :: Map Gtime [SingleRecord] -> (MaskSpec,[Mask])
getMasks = Data.Map.foldr accMs empty
  where empty = (MaskSpec [] [], [])
        accMs dat (mask, ms) = ( \m -> (m, (buildMask m dat):ms) ) $ newmask mask dat
        newmask a b = updateMaskSpec a $ getMaskSpec b

-- Para "Mejor Error Handling" reemplazar Maybe por Either y las Guards por
-- Tests que devuelve Rigth en vez de Just y Left "Descripción" en vez de nothing
-- Se puede hacer facil definiendo guardE "error" test

nullCode = (-1,'N') -- Hay formas mas haskellanas de hacer esto, ni da.
testCode :: (Int,Char) -> (Int,Char) -> Maybe Bool
testCode a b = do
  guard (b == nullCode)
  return False

nullSat = (-1) -- Hay formas mas haskelleanas de hacer esto, ni da.
testSat :: Int -> Int -> Int -> [Maybe Bool]
testSat a b l = take l $ repeat (testSat' a b)
  where testSat' a b = do
          guard (b == nullSat)
          return False

uniformMasks :: MaskSpec -> [Mask] -> Maybe [BareMask]
uniformMasks spec mss = checkAll [ bareMask spec ms m | Mask ms m <- mss ]
  where checkAll a = do
          guard (all isJust a)
          return $ catMaybes a
        bareMask spec ms m = (\a -> do
          guard (all isJust . concat $ a)
          return $ BareMask (map catMaybes a) ) $ bareMask' spec ms m
        bareMask' spec ms m = [ (map Just c) ++ (cs spec ms) | c <- m ] ++ (ss spec ms)
        cs (MaskSpec _ s1)  (MaskSpec _ s2) = [ testCode x y | (x, y) <- zip s1 (s2 ++ (repeat nullCode)), y == nullCode || y /= x ]
        ss (MaskSpec s1 c1)  (MaskSpec s2 _) = [ testSat x y (length c1) | (x, y) <- zip s1 (s2 ++ (take (length c1) (repeat nullSat))), y == nullSat || y /= x ]

-- | Pack up to eight bools in a byte.
boolsToWord8 :: [Bool] -> Word8
boolsToWord8 [] = 0
boolsToWord8 xs = foldl setBit 0 (map snd $ filter fst $ zip xs [0 .. 7])

fillBytes :: ([[Word8]],[[Bool]]) -> ([[Word8]],[[Bool]])
fillBytes (bss, accum) = (,) (map (uncurry (:)) $ zip (map boolsToWord8 accum) bss) (repeat [])

hashMasksMap :: MaskSpec -> [BareMask] -> Map (Int,(Int,Char)) (Digest SHA256)
hashMasksMap ms mm = fromList $ zip (toKeys ms) (hashMasks mm)
   where toKeys (MaskSpec ss cs) = (\a b -> (b,a)) <$> cs <*> ss -- cartesian product 
-- Nota: la instancia de Applicative de listas trabaja con producto cartesiano aunque mi intuición pensaba en zip.

--hashMasks :: [BareMask] -> [[Word8]]
hashMasks :: [BareMask] -> [Digest SHA256]
hashMasks m = map ( sha256 . pack ) $ (fst . fillBytes) $ foldl' hashMasks' (repeat [], repeat []) m
 where hashMasks' (bss, accum) (BareMask x) = 
                       fillBytesIfFull bss ( map (uncurry (:)) $ zip (concat x) accum)
       fillBytesIfFull bss accum = 
                       if (length $ accum !! 0) == 8
                       then curry fillBytes bss accum
                       else (,) bss accum

sha256 :: ByteString -> Digest SHA256
sha256 = hash

-- HashMask         
-- AESON -> Map Gtime [SingleRecord] -> Json -> Python -> Arrow -> Pandas
--                                           -> js -> ?
-- Eventualmente uniformRecords con las Máscaras.

--typedef struct {        /* observation data record */
--    gtime_t time;       /* receiver sampling time (GPST) */
--    uint8_t sat,rcv;    /* satellite/receiver number */
--    uint16_t SNR[NFREQ+NEXOBS]; /* signal strength (0.001 dBHz) */
--    uint8_t  LLI[NFREQ+NEXOBS]; /* loss of lock indicator */
--    uint8_t code[NFREQ+NEXOBS]; /* code indicator (CODE_???) */
--    double L[NFREQ+NEXOBS]; /* observation data carrier-phase (cycle) */
--    double P[NFREQ+NEXOBS]; /* observation data pseudorange (m) */
--    float  D[NFREQ+NEXOBS]; /* observation data doppler frequency (Hz) */
--} obsd_t;

data ObsData = ObsData {
               n :: CInt,
               nmax :: CInt,
               _data :: PtrObsOneData
               } deriving (Show, Eq)

instance Storable ObsData where
 peek ptr  = ObsData
            <$> {#get obs_t->n #} ptr
            <*> {#get obs_t->nmax #} ptr
            <*> {#get obs_t->data #} ptr
 -- poke = esta implementación probablemente este mal por la diferencia de arrays y puntros, pero no la estoy usando en realidad.
 poke ptr dat = do
            {#set obs_t->n #} ptr (n dat)
            {#set obs_t->nmax #} ptr (nmax dat)
            {#set obs_t->data #} ptr (_data dat)
 alignment _ = {#alignof obs_t #}
 sizeOf _ = {#sizeof obs_t #}

{#pointer *obs_t as PtrObsData -> ObsData #}

{#pointer *nav_t as NavData #}
{#pointer *sta_t as StaData #}

data Gtime = Gtime {
      time :: CTime,
      sec :: Double
      } deriving (Show, Eq, Ord)

instance Storable Gtime where
 peek ptr  = Gtime
            <$> (\ptr -> do {peekByteOff ptr 0 :: IO CTime}) ptr
            <*> (\ptr -> do {peekByteOff ptr 8 :: IO Double}) ptr
 -- poke = esta implementación probablemente este mal por la diferencia de arrays y puntros, pero no la estoy usando en realidad.
 poke ptr dat = do
            (\ptr val -> do {C2HSImp.pokeByteOff ptr 0 (val :: CTime)}) ptr (time dat)
            (\ptr val -> do {C2HSImp.pokeByteOff ptr 8 (val :: Double)}) ptr (sec dat)
 alignment _ = {#alignof gtime_t #}
 sizeOf _ = {#sizeof gtime_t #}

{#pointer *gtime_t as GtimePtr -> Gtime#}

{#fun pure epoch2time_ as ep2time {alloca- `Gtime' peek*, withArray* `[CDouble]'} -> `()' #}

{#fun readrnxt_ as readRinexT {`CString',fromIntegral `CInt',with* `Gtime',with* `Gtime', `CDouble',`CString',alloca- `PtrObsData' id,alloca- `NavData',alloca- `StaData'} -> `(Integer)' toInteger #}

{#fun dumpobs as dumpObs {id `PtrObsData'} -> `()' #}

-- TODO: extractObs AESON?
--       Disenar un Hash.
--       Chequear gNAT y compatibilizar.
