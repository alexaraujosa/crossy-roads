{- |
Module      : Tarefa1_2022li1g003
Description : Validação de um mapa
Copyright   : Alex Araújo de Sá  <a104257@alunos.uminho.pt>
              Rafael Santos Fernandes <a104271@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2022/23.
-}
module Tarefa1_2022li1g003 where

import LI12223
import Data.List (group, groupBy)

{- |
 A função @mapaValido@ verifica se um dado mapa pode ser, ou não, um mapa válido.
-}
mapaValido :: 
    Mapa -- ^ Um mapa a considerar para a verificação.
    -> Bool -- ^ Resultado da verificação.
mapaValido m@(Mapa w l) = 
    -- Confirmação de que o comprimento da lista de obstáculos de cada linha é igual à largura do Mapa.
    all (ensureTerreno m) l 
    -- As linhas contíguas do tipo @Rio@ passam a estar agrupadas e confirmação que estas têm direções opostas.
    && all ensureRios (groupRios l) 
    -- Confirmação de que não existem mais do que 4 rios contiguamente.
    && (if not (null (groupRios l)) then any (\x -> length x <= 4) (groupRios l) else True) 
    -- Confirmação de que não existem mais do que 5 estradas contiguamente.
    && (if not (null (groupEstradas l)) then any (\x -> length x <= 5) (groupEstradas l) else True) 
    -- Confirmação de que não existem mais do que 5 relvas contiguamente.
    && (if not (null (groupRelvas l)) then any (\x -> length x <= 5) (groupRelvas l) else True) 

{-|
 A função @ensureTerreno@ recebe um Mapa e uma linha do Mapa, verificando se o comprimento da lista de obstáculos da linha corresponde exatamente à largura do Mapa.

 __AVISO:__ Esta é uma função interna, não desenhada para uso por funções externas a este ficheiro.
-}
ensureTerreno :: 
    Mapa -- ^ O Mapa original.
    -> (Terreno, [Obstaculo]) -- ^ A linha do mapa a ser verificada.
    -> Bool -- ^ Resultado da verificação.
ensureTerreno (Mapa w _) (Rio r, o) = isRio (Rio r, o) && length o == w -- Verificação de uma linha do tipo @Rio@.
ensureTerreno (Mapa w _) (Estrada r, o) = isEstrada (Estrada r, o) && length o == w -- Verificação de uma linha do tipo @Estrada@.
ensureTerreno (Mapa w _) (Relva, o) = isRelva (Relva, o) && length o == w -- Verificação de uma linha do tipo @Relva@.

{- |
 A função @groupRios@ agrupa as linhas contiguas do tipo @Rio@.

 __AVISO:__ Esta é uma função interna, não desenhada para uso por funções externas a este ficheiro.
-}
groupRios :: 
    [(Terreno, [Obstaculo])] -- ^ Uma lista de linhas do Mapa.
    -> [[(Terreno, [Obstaculo])]] -- ^ A lista com as linhas contíguas do tipo @Rio@. 
groupRios l = [ x | x@((Rio {},_):_) <- reverse (gR [] l)]
    where
        gR acc [] = acc
        gR (hacc@((Rio _,_):_):tacc) (h@(Rio r,_):t) = gR ((hacc ++ [h]):tacc) t
        gR acc (h@(Rio r,_):t) = gR ([h]:acc) t
        gR acc (h:t) = gR ([]:acc) t

{- |
 A função @groupEstradas@ agrupa as linhas contiguas do tipo @Estrada@.

 __AVISO:__ Esta é uma função interna, não desenhada para uso por funções externas a este ficheiro.
-}
groupEstradas :: 
    [(Terreno, [Obstaculo])] -- ^ Uma lista de linhas do Mapa.
    -> [[(Terreno, [Obstaculo])]] -- ^ A lista com as linhas contíguas do tipo @Estrada@.
groupEstradas l = [ x | x@((Estrada {},_):_) <- reverse (gE [] l)]
    where
        gE acc [] = acc
        gE (hacc@((Estrada _,_):_):tacc) (h@(Estrada r,_):t) = gE ((hacc ++ [h]):tacc) t
        gE acc (h@(Estrada r,_):t) = gE ([h]:acc) t
        gE acc (h:t) = gE ([]:acc) t

{- |
 A função @groupRelvas@ agrupa as linhas contiguas do tipo @Relva@.

 __AVISO:__ Esta é uma função interna, não desenhada para uso por funções externas a este ficheiro.
-}
groupRelvas :: 
    [(Terreno, [Obstaculo])] -- ^ Uma lista de linhas do Mapa.
    -> [[(Terreno, [Obstaculo])]] -- ^ A lista com as linhas contíguas do tipo @Relva@.
groupRelvas l = [ x | x@((Relva {},_):_) <- reverse (gE [] l)]
    where
        gE acc [] = acc
        gE (hacc@((Relva,_):_):tacc) (h@(Relva,_):t) = gE ((hacc ++ [h]):tacc) t
        gE acc (h@(Relva,_):t) = gE ([h]:acc) t
        gE acc (h:t) = gE ([]:acc) t

{- |
 A função @ensureRios@ verifica se, numa lista de linhas do Mapa, caso existam linhas contíguas do tipo @Rio@, estas têm direções opostas.

 __AVISO:__ Esta é uma função interna, não desenhada para uso por funções externas a este ficheiro.
-}
ensureRios :: 
    [(Terreno, b)] -- ^ Uma lista de linhas do Mapa.
    -> Bool -- ^ Resultado da verificação.
ensureRios = eR False
    where
        eR acc [] = acc
        eR acc ((Rio r1,_):t@((Rio r2,_):_)) 
            | r1 > 0 = (r2 <= 0) && eR True t
            | r1 < 0 = (r2 >= 0) && eR True t
            | otherwise = False
        eR acc [(Rio r1,_)] = True
        eR acc ((Relva,_):t) = eR acc t
        eR acc ((Estrada _,_):t) = eR acc t

{-|
 A função @isRio@ recebe uma linha do Mapa e verifica se esta possui as características necessárias para ser considerada um @Rio@.

 __AVISO:__ Esta é uma função interna, não desenhada para uso por funções externas a este ficheiro.
-}
isRio :: 
    (Terreno, [Obstaculo]) -- ^ A linha a ser verificada.
    -> Bool -- ^ Resultado da verificação.
isRio (Rio r, o) = 
    all (\x -> x == Nenhum || x == Tronco) o
    && elem Nenhum o
    && not (any (\x -> (head x == Tronco) && (length x > 5)) (group o))
isRio (_,_) = False

{-|
 A função @isEstrada@ recebe uma linha do Mapa e verifica se esta possui as características necessárias para ser considerada uma @Estrada@.

 __AVISO:__ Esta é uma função interna, não desenhada para uso por funções externas a este ficheiro.
-}

isEstrada :: 
    (Terreno, [Obstaculo]) -- ^ A linha a ser verificada.
    -> Bool -- ^ Resultado da verificação.
isEstrada (Estrada r, o) = 
    all (\x -> x == Nenhum || x == Carro) o
    && elem Nenhum o
    && not (any (\x -> (head x == Carro) && (length x > 3)) (group o))
isEstrada (_,_) = False

{-|
 A função @isRelva@ recebe uma linha do Mapa e verifica se esta possui as características necessárias para ser considerada uma @Relva@.

 __AVISO:__ Esta é uma função interna, não desenhada para uso por funções externas a este ficheiro.
-}

isRelva :: 
    (Terreno, [Obstaculo]) -- ^ A linha a ser verificada.
    -> Bool -- ^ Resultado da verificação.
isRelva (Relva, o) = 
    all (\x -> x == Nenhum || x == Arvore) o
    && elem Nenhum o
isRelva (_,_) = False