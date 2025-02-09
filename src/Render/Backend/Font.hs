{- |
Module      : Font
Description : Módulo de renderização de texto.
Copyright   : Rafael Santos Fernandes <a104271@alunos.uminho.pt>

Módulo de renderização de texto utilizando Fonts customizadas.
-}

module Render.Backend.Font where

import Graphics.Gloss
import Render.Backend.Atlas
import Data.Char (ord)
import Data.List (mapAccumL, minimumBy)
import Util

-- | Representa os offsets de cada caractére. Valores negativos representam um shift á esquerda do texto e 
-- valores positivos representam shifts á direita do texto.
type OffsetTable = [Int]

defaultOffsetTable :: OffsetTable
defaultOffsetTable = replicate 127 0

-- | Representa um Font no sistema de renderização.
data FontData = FontData
    Atlas       -- ^ O Texture Atlas da Font
    (Int,Int)   -- ^ O tamanho (x,y) base de cada caractere
    OffsetTable -- ^ A Tabela de Offsets
    deriving (Show)

-- | Cria uma Font a partir de um Texture Atlas.
makeFont :: String -> (Int,Int) -> IO FontData
makeFont n s = do
    font <- loadAtlas n
    return $ FontData font s defaultOffsetTable

-- | Obtém as coordenadas de um charactere na Font Especificada.
getFontCharPos :: FontData -> Char -> (Int, Int)
getFontCharPos (FontData f (xs,_) _) c = (col*xs,row*xs)
    where
        (bw,_) = getAtlasSize f
        code = ord c
        col = mod code (div bw xs)
        row = div code (div bw xs)

-- | Obtém um caractere renderizado com a Font fornecida.
getCharFont :: FontData -> Char -> Picture
getCharFont fd@(FontData f (s,_) _) c = getAtlasSection f (AtlasSection (_x,_y) (s,s))
    where
        (_,bh) = getAtlasSize f
        (_x,_y) = getFontCharPos fd c

-- | Representa o alinhamento de um texto multi-linha.
data TextAlignment = AlignLeft | AlignCenter | AlignRight deriving (Eq, Show)

-- | Converte uma string para uma Picture contendo a mesma string renderizada com a Font fornecida.
strToPic :: FontData -> String -> TextAlignment -> Picture
strToPic f@(FontData _ (xs,yx) ot) s a 
    | a == AlignLeft =
        Pictures . snd . mapAccumL (\c x -> (c+1, Translate 0 (fromIntegral xs * c) x)) 0 . reverse 
        $ map (Pictures . snd . mapAccumL (\(c,o) x -> 
                    ( (c + 1,o + ot !! ord x), Translate (fromIntegral xs * c + fromIntegral o) 0 $ getCharFont f x )
                ) (0,0)
            ) segs
    | a == AlignCenter =
        Pictures . snd . mapAccumL (\c x -> (c+1, Translate 0 (fromIntegral xs * c) x)) 0 . reverse 
        $ map (\x -> (Translate (fromIntegral (sw - fst (stringSize f x)) / 2) 0 . Pictures . snd . mapAccumL (\(c,o) x -> 
                    ( (c + 1,o + ot !! ord x), Translate (fromIntegral xs * c + fromIntegral o) 0 $ getCharFont f x )
                ) (0,0)) x
            ) segs
    | a == AlignRight =
        Pictures . snd . mapAccumL (\c x -> (c+1, Translate 0 (fromIntegral xs * c) x)) 0 . reverse 
        $ map (\x -> (Translate (fromIntegral (sw - fst (stringSize f x))) 0 . Pictures . snd . mapAccumL (\(c,o) x -> 
                    ( (c + 1,o + ot !! ord x), Translate (fromIntegral xs * c + fromIntegral o) 0 $ getCharFont f x )
                ) (0,0)) x
            ) segs
    where
        segs = wordsWhen (=='\n') s
        (sw,sh) = stringSize f s

-- | Obtém o tamanho de uma String segundo uma Font.
stringSize :: FontData -> String -> (Int, Int)
stringSize (FontData _ (xs,yx) ot) s = (x,y)
    where
        segs = wordsWhen (=='\n') s
        
        l = minimumBy (\a b -> compare (length b) (length a)) segs
        x = foldr (\c a -> a + (xs + ot !! ord c)) 0 l

        y = length segs * yx

-- | Obtém o tamanho real de uma String segundo uma Font, representada numa 'Picture'.
trueStringSize :: FontData -> String -> (Float,Float)
trueStringSize f@(FontData _ (w,h) ot) s = (fromIntegral x + (2 * fromIntegral (ot !! ord (last s))), fromIntegral y)
    where
        (x,y) = stringSize f s
        