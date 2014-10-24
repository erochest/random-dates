{-# LANGUAGE OverloadedStrings #-}


module RandomDates.Text
    ( TextPair
    , tokenize
    , triples
    , index
    , trainChains
    , createTable
    , createStartTable
    , generateText
    , generateText_
    , generateParagraphs
    , generateParagraphs_
    , lastCache
    , pos
    ) where


import           Control.Applicative
import           Control.Arrow hiding (first)
import           Data.Attoparsec.Text
import           Data.Bifunctor
import           Data.Char
import           Data.Foldable
import           Data.Hashable
import qualified Data.HashMap.Strict              as M
import qualified Data.List                        as L
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                        as T
import qualified Data.Vector                      as V
import           System.Random.MWC
import           System.Random.MWC.CondensedTable
import           System.Random.MWC.Distributions

import           Topical.Text.Tokenizer           (parserTokenizer)

import           RandomDates.Types


type TextPair   = (T.Text, T.Text)
type TableCache = M.HashMap TextPair (CondensedTableV T.Text)

tokenize :: T.Text -> [T.Text]
tokenize = parserTokenizer token
    where
        token = word <|> punctuation
        word = T.append <$> takeWhile1 isAlphaNum
                        <*> option T.empty contraction
        contraction = T.cons <$> char '\'' <*> takeWhile1 isAlphaNum
        punctuation = dash <|> singleCharPunct
        dash = takeWhile1 (== '-')
        singleCharPunct = T.singleton <$> satisfy isPunctuation

triples :: [a] -> [(a, a, a)]
triples = mapMaybe triples' . L.tails
    where
        triples' (a:b:c:_) = Just (a, b, c)
        triples' _         = Nothing

index :: (Hashable b, Eq b, Hashable c, Eq c)
      => (a -> b) -> (a -> c) -> [a] -> M.HashMap b (Frequencies c)
index key subkey = L.foldl' insert M.empty
    where
        insert m a = let k  = key a
                         sk = subkey a
                         v  = case M.lookup k m of
                                  Just f  -> M.insertWith (+) sk 1 f
                                  Nothing -> M.singleton sk 1
                     in  M.insert k v m

trainChains :: [T.Text] -> MarkovChains
trainChains ts = index (\(a, b, _) -> (a, b)) (\(_, _, c) -> c) $ triples ts

randomStart :: GenIO -> MarkovChains -> IO TextPair
randomStart g mcc = genFromTable (createStartTable mcc) g

scale :: Double -> Int -> Double
scale total x = fromIntegral x / total

sumVals :: M.HashMap k Int -> Double
sumVals = fromIntegral . M.foldl' (+) 0

createTable :: Frequencies T.Text -> CondensedTableV T.Text
createTable freqs =
    tableFromProbabilities . V.fromList . M.toList $ M.map scale' freqs
    where
        scale' = scale (sumVals freqs)

createStartTable :: MarkovChains -> CondensedTableV TextPair
createStartTable mcc =
    tableFromProbabilities . V.fromList . M.toList $ M.map scale' sizes
    where
        sizes = M.map M.size mcc
        scale' = scale (sumVals sizes)

look :: TableCache -> MarkovChains -> TextPair
     -> (TableCache, CondensedTableV T.Text)
look cache chains pair =
    case M.lookup pair cache of
        Just table -> (cache, table)
        Nothing    -> (insert cache pair &&& id)
                    . createTable
                    $ M.lookupDefault M.empty pair chains
    where
        insert m k v = M.insert k v m

generateText :: GenIO -> MarkovChains -> Int -> IO [T.Text]
generateText g mc n = randomStart g mc >>= go M.empty n
    where
        go _     0 _           = return []
        go cache n pair@(_, b) = do
            let (cache', table) = look cache mc pair
            c <- genFromTable table g
            (c:) <$> go cache' (pred n) (b, c)

generateText_ :: GenIO
              -> CondensedTableV TextPair
              -> TableCache
              -> MarkovChains
              -> Int
              -> IO [(Last TableCache, T.Text)]
generateText_ g table cache mc n = genFromTable table g >>= go cache n
    where
        go _     0 _           = return []
        go cache n pair@(_, b) = do
            let (cache', table) = look cache mc pair
            c <- genFromTable table g
            ((Last (Just cache'), c) :) <$> go cache' (pred n) (b, c)

generateParagraphs :: GenIO -> MarkovChains -> Int -> Int -> Int
                   -> IO [(Last TableCache, [T.Text])]
generateParagraphs g mc psize pvar =
    generateParagraphs_ g (createStartTable mc) mc psize pvar

generateParagraphs_ :: GenIO -> CondensedTableV TextPair -> MarkovChains -> Int -> Int -> Int
                    -> IO [(Last TableCache, [T.Text])]
generateParagraphs_ g table mc psize pvar = go M.empty
    where
        psize' = fromIntegral psize
        pvar'  = fromIntegral pvar
        go _     0 = return []
        go cache n = do
            p  <-  normal psize' pvar' g
               >>= generateText_ g table cache mc . pos . truncate
            let (cache', p') = lastCache p
            ((Last (Just cache'), p') :) <$> go cache' (pred n)

lastCache :: [(Last TableCache, a)] -> (TableCache, [a])
lastCache = first (fold . getLast) . L.foldr accum (Last Nothing, [])
    where
        accum (c, x) (lc, xs) = (c <> lc, x:xs)

pos :: Int -> Int
pos n | n < 0     = (-1) * n
      | otherwise = n
