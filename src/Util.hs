{- |
Module      : Util
Description : Funções utilitárias miscelâneas
Copyright   : Rafael Santos Fernandes <a104271@alunos.uminho.pt>

Módulo contendo funções utilitárias gerais para a realização do projeto de LI1 em 2022/23.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Util where

--import System.Random
import Data.Time.Clock.POSIX
import Data.Time.Calendar ( toGregorian )
import Data.Time.Clock (UTCTime(..))
import System.Random
import LI12223
import Control.Exception (SomeException)
import Control.Exception.Base (catch)
import Data.Functor -- (($>))
import Data.Time.Format
import Data.Char (toUpper)

{- |
 A função @filterIndexed@ filtra os elementos de uma lista de acordo com um predicado fornecido pelo utilizador, o qual, 
recebendo cada elemento da lista seguido do seu índice na mesma lista deverá retornar o valor booleano @True@ para elementos 
a receber e o valor booleano @False@ para elementos a descartar.

 A função retornará uma lista de tuplos contendo o elemento e o seu índice, respetivamente.
-}
filterIndexed :: 
    (a -> Int -> Bool) -- ^ A função de filtro, que toma um tuplo contendo um elemento e o seu índice na lista e retorna um valor booleano.
    -> [a]             -- ^ A lista a ser filtrada.
    -> [(a,Int)]       -- ^ A lista contendo tuplos com o elemento filtrado e o seu índice, respetivamente, para todos os elementos originais.
filterIndexed f l = fI [] 0 f l
    where
        fI acc _ _ [] = acc
        fI acc i f (h:t) = if f h i then fI (acc ++ [(h,i)]) (i + 1) f t else fI acc (i + 1) f t

{- |
 A função @mapIndexed@ mapeia os elementos de uma lista de acordo com um predicado fornecido pelo utilizador, o qual, 
recebendo cada elemento da lista seguido do seu índice na mesma lista deverá retornar o valor mapeado do elemento.

 A função retornará uma lista de tuplos contendo o elemento mapeado e o seu índice, respetivamente.
-}
mapIndexed :: 
    ((a, Int) -> b) -- ^ A função de mapeamento, que toma um tuplo contendo um elemento e o seu índice na lista e retorna o elemento mapeado.
    -> [a]          -- ^ A lista a ser mapeada.
    -> [(b,Int)]    -- ^ A lista contendo tuplos com o elemento mapeado e o seu índice, respetivamente, para todos os elementos originais.
mapIndexed f l = zip (map f $ filterIndexed (\_ _ -> True) l) [1..length l]

{- |
 A função @anyIndexed@ verifica se pelo menos um elemento de uma lista satisfaz um predicado fornecido pelo utilizador, o qual,
recbendo cada elemento da lista seguido do seu índice na mesma lista deverá retornar um valor booleano.

 A função retornará um valor booleano.
-}
anyIndexed :: 
    ((a, Int) -> Bool) -- ^ A função de mapeamento, que toma um tuplo contendo um elemento e o seu índice na lista e retorna o elemento mapeado.
    -> [a]          -- ^ A lista a ser mapeada.
    -> Bool         -- ^ A lista contendo tuplos com o elemento mapeado e o seu índice, respetivamente, para todos os elementos originais.
anyIndexed f l = aI f (filterIndexed (\_ _ -> True) l)
    where
        aI f [x] = f x
        aI f (h:t) = f h || aI f t

{- |
 A função @mapWithCacheR@ é a versão non-overloaded da função @mapWithCache@. A única diferença entre as mesmas é o valor de 
retorno, retornando esta função um tuplo contendo os elementos mapeados e o último valor da cache, respetivamente.
-}

mapWithCacheR :: 
    (a -> b -> [c] -> (a,c)) -- ^ A função de mapeamento.
    -> a                      -- ^ O valor inicial da cache.
    -> [b]                    -- ^ A lista a ser mapeada.
    -> ([c],a)                -- ^ A lista de elementos mapeados.
mapWithCacheR f c = mC ([],c) f
    where
        --mC :: ([c],b) -> (b -> a -> [c] -> (b,c)) -> [a] -> ([c],b)
        mC :: ([c],a) -> (a -> b -> [c] -> (a,c)) -> [b] -> ([c],a)
        mC (acc,c) _ [] = (acc,c)
        mC (acc,c) f (h:t) = mC (acc ++ [snd $ f c h acc], fst $ f c h acc) f t

{- |
 A função @mapWithCache@ executa o mesmo comportamento da função predefinida @map@, com a exceção de que permite que um valor 
de cache @c@ seja utilizado na próxima iteração.  

 == Exemplos de Utilização
 >>> mapWithCache 0 (\x c -> (x + c,c + 1)) [0,0,0]
 [0,1,2]
-}
mapWithCache :: 
    a                     -- ^ O valor inicial da cache.
    -> (a -> b -> [c] -> (a,c)) -- ^ A função de mapeamento.
    -> [b]                -- ^ A lista a ser mapeada.
    -> [c]                -- ^ A lista de elementos mapeados.
mapWithCache c f l = fst $ mapWithCacheR f c l

{- |
 A função @getPRndElem@ retorna um elemento pseudo-aleatório de uma lista a partir de um número pseudo-aleatório @r@ pertencente 
ao intervalo [0,@max@].

-}
getPRndElem :: 
    [a]    -- ^ Uma lista de elementos da qual selecionar um elemento aleatório.
    -- -> Int -- ^ @max@: O valor máximo possível para @r@.
    -> Int -- ^ @r@: Um número pseudo-aleatório.
    -> a   -- ^ O elemento selecionado.
getPRndElem l r = l !! mod r (length l) --l !! div (r * length l) max

{- |
 A função @getTimeSA@ retorna a data no momento da chamada no formato Unix Date Format (em segundos).
-}
getTimeSA :: IO Integer
getTimeSA = round `fmap` getPOSIXTime

{- |
 A função @getDate@ retorna a data no momento da chamada no formato DD/MM/YYYY.
-}
getDate :: IO (Int, Int, Integer)
getDate = do
    time <- getCurrentTime
    let (y,m,d) = (toGregorian . utctDay) time
    return (d,m,y)

formatDate :: UTCTime -> String
formatDate time = let (y,m,d) = (toGregorian . utctDay) time in show d ++ "/" ++ show m ++ "/" ++ show y

makeTime :: Integer -> UTCTime
makeTime i = posixSecondsToUTCTime (fromInteger i)

{- |
 A função @scrambleNum@ aplica uma série de operações matemáticas que transformam um número fornecido em um outro número 
sem significado.

 __AVISO:__ Esta é uma função interna, não desenhada para uso por funções externas a este ficheiro.
-}
scrambleNum :: 
    Integer    -- ^ O número a modificar.
    -> Integer -- ^ O número resultante.
scrambleNum x = 
    mod (ceiling (fromIntegral x ^^ 92)) 456 + mod (8464541434 `div` (if x == 0 then 1 else x)) 69

{- |
 A função @getPRand@ gera um número pseudoaleatório a partir de uma seed fornecida pelo utilizador.

 Para garantir uma sequência de números pseudoaleatórios, é recomendado que se use o resultado anterior da execução desta 
função.
-}

-- TODO: Use System.Random
getPRand :: Int -> [Int]
getPRand i = randoms (mkStdGen i)

{- |
 A função @shiftN@ movimenta os elementos de uma lista num modo /wrap-around/.  

 Valores de @n@ positivos movimentam os elementos para a direita @n@ índices.  
 Valores de @n@ negativos movimentam os elementos para a esquerda @n@ índices.
-}
shiftN :: Int -> [a] -> [a]
shiftN _ [] = []
shiftN n l 
    | n > 0 = shiftN (n - 1) (last l : init l)
    | n < 0 = shiftN (n + 1) (tail l ++ [head l])
    | otherwise = l

{- |
 A função @shiftNC@ movimenta os elementos de uma lista num modo /wrap-around/ consoante o valor de um predicado fornecido
pelo utilizador, que recebe o elemento a ser rodado e o seu índice na lista e que retorna:
    - 0 se o elemento passa o predicado.
    - 1 se o elemento falha o predicado, retornando a lista construida até então.
    - Qualquer outro valor se o elemento falha o predicado e uma lista vazia é retornada.

-}
shiftNCI :: ([(a, Int)] -> Int) -> Int -> [a] -> [(a, Int)]
shiftNCI _ _ [] = []
shiftNCI f n _l 
    | n > 0 =
        if f (last l : init l) == 0 then 
            shiftNCI f (n - 1) (map fst (last l : init l)) 
        else 
            if f (last l : init l) == 1 then
                last l : init l
            else []
    | n < 0 = --shiftN (n + 1) (tail l ++ [head l])
        if f (tail l ++ [head l]) == 0 then 
            shiftNCI f (n + 1) (map fst (tail l ++ [head l])) 
        else 
            if f (tail l ++ [head l]) == 1 then
                tail l ++ [head l]
            else []
    | otherwise = l
    where
        l = filterIndexed (\x _ -> True) _l

{- | 
 A função @getTerrenoConstr@ retorna um Int representando o identificador do tipo de terreno fornecido.

 == Indentificadores de Terrenos
    - 0: 'Rio'
    - 1: 'Estrada'
    - 2: 'Relva'
-}
getTerrenoConstr (Rio _) = 0
getTerrenoConstr (Estrada _) = 1
getTerrenoConstr Relva = 2

{- |
 A função @getTerrenoVel@ retorna a velocidade de um terreno, se for dinâmico (Rio,Estrada), ou 0 se o terreno for estático 
(Relva).
-}
getTerrenoVel (Rio v) = v
getTerrenoVel (Estrada v) = v
getTerrenoVel Relva = 0

{- | 
 Retorna um valor booleano verdadeiro se uma ação retorna uma exceção, ou um valor boolean falso se nenhuma exceção é
retornada.

 Adaptado de: https://stackoverflow.com/a/41127693
-}
throws io = catch (io $> False) $ \(e :: SomeException) -> pure True

{-
 A função @replaceAt@ substitui um elemento num índices especificado numa lista.
-}
replaceAt :: [a] -> Int -> a -> [a]
replaceAt [] _ _ = error "Index Out Of Bounds."
replaceAt l i e
    | i < length l = take i l ++ [e] ++ reverse (take (length l - i - 1) (reverse l))
    | otherwise = error "Index Out Of Bounds"

{-
 A função @replaceAt@ substitui vários elementos em índices especificado numa lista.
-}
replaceAtIndexes :: [a] -> [(Int,a)] -> [a]
replaceAtIndexes [] _ = error "Index Out Of Bounds."
replaceAtIndexes l [] = l
replaceAtIndexes l il
    | i < length l = replaceAtIndexes (take i l ++ [e] ++ reverse (take (length l - i - 1) (reverse l))) (tail il)
    | otherwise = error "Index Out Of Bounds"
    where
        (i,e) = head il

{-
 A função @wordsWhen@ separa uma string de acordo com um predicado.
-}
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of {
        "" -> [];
        s' -> w : wordsWhen p s'' where (w, s'') = break p s';
    }

{-
 A função @withinBoundsWA@ restringe um valor @a@ a um intervalo entre @mi@ e @ma@, utilizando a função Modulo para a restrição.
-}
withinBoundsWA :: Integral a => a -> a -> a -> a
withinBoundsWA mi ma a = max mi (mod a ma)

{-
 A função @withinBounds@ restringe um valor @a@ a um intervalo entre @mi@ e @ma@, retornando o máximo ou o mínimo caso o valor
não se enquadre no intervalo (menor ou maior, respetivamente.)
-}
withinBounds :: Integral a => a -> a -> a -> a
withinBounds mi ma a 
    | mi > a = mi
    | ma < a = ma
    | otherwise = a

{-
 A função @trimString@ restringe uma string a uma largura especificada.
-}
trimString :: [Char] -> Int -> Bool -> String
trimString s l st
    | null s || l <= 0 = ""
    | st = take (l - 3) s ++ "..."
    | otherwise = take l s ++ "..."

{-
 A função @padString@ adiciona um caractere @max {0, n - l}@ vezes a uma string de largura @n@, de modo que @n@ corresponda a uma largura @l@ especificada.
-}
padString :: String -> Char -> Int -> String
padString s c l
    | l <= length s = s
    | otherwise = s ++ replicate (l - length s) c

{-
 A função @capitalizeWord@ capitaliza apenas o primeiro caractére de uma string.
-}
capitalizeWord :: String -> String
capitalizeWord w = toUpper (head w) : tail w

-- | Retorna a parte decimal de um número.
getDecimal :: RealFrac b => b -> b
getDecimal x = snd (properFraction x)

-- Funções relativas a tuplos de ordem 3.
fst3 (e,_,_) = e
snd3 (_,e,_) = e
thrd3 (_,_,e) = e