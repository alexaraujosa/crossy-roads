{- |
Module      : Logger.Colors
Description : Módulo de cores para logs.
Copyright   : Rafael Santos Fernandes <a104271@alunos.uminho.pt>

Módulo de manipulação de mensagens de log, permitindo que cores sejam aplicadas ás mesmas.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module Util.Logger.Colors where

import Control.Arrow ((***))
import Data.Monoid
import Data.String
import GHC.Generics

-- | Converte um número em um código ANSI.
toANSI :: (Monoid m, IsString m) => m -> m
toANSI x = "\x1b[" <> x <> "m"

-- | Transforma um par de números num par de códigos ANSI.
toANSI2 :: (Monoid t1, Monoid t, IsString t1, IsString t) => (t, t1) -> (t, t1)
toANSI2 = toANSI *** toANSI

-- | Gera um par de cores para o background e o foreground do texto na consola.
makeANSIPair :: (Monoid m, IsString m) => (m, m) -> m -> m
makeANSIPair (l, r) s = let (cl, cr) = toANSI2 (l, r) in cl <> s <> cr

type Color = (Monoid String, IsString String) => String -> String

newtype UseColor a = Color a deriving (Show)

reset, bold, dim, italic, underline, inverse, hidden, strikethrough, black, red, green, yellow, blue, magenta, cyan, white, 
    gray, bgBlack, bgRed, bgGreen, bgYellow, bgBlue, bgMagenta, bgCyan, bgWhite :: Color
reset         = makeANSIPair ("0", "0")
bold          = makeANSIPair ("1", "22")
dim           = makeANSIPair ("2", "22")
italic        = makeANSIPair ("3", "23")
underline     = makeANSIPair ("4", "24")
inverse       = makeANSIPair ("7", "27")
hidden        = makeANSIPair ("8", "28")
strikethrough = makeANSIPair ("9", "29")
black         = makeANSIPair ("30", "39")
red           = makeANSIPair ("31", "39")
green         = makeANSIPair ("32", "39")
yellow        = makeANSIPair ("33", "39")
blue          = makeANSIPair ("34", "39")
magenta       = makeANSIPair ("35", "39")
cyan          = makeANSIPair ("36", "39")
white         = makeANSIPair ("37", "39")
gray          = makeANSIPair ("90", "39")
bgBlack       = makeANSIPair ("40", "49")
bgRed         = makeANSIPair ("41", "49")
bgGreen       = makeANSIPair ("42", "49")
bgYellow      = makeANSIPair ("43", "49")
bgBlue        = makeANSIPair ("44", "49")
bgMagenta     = makeANSIPair ("45", "49")
bgCyan        = makeANSIPair ("46", "49")
bgWhite       = makeANSIPair ("47", "49")