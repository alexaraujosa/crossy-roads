{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE CPP #-}
{- |
Module      : LI12223
Description : Módulo auxiliar para LI1 22/23.
Copyright   : Manuel Barros <d13242@di.uminho.pt>
              Nelson Estevão <d12733@di.uminho.pt>
              Olga Pacheco <omp@di.uminho.pt>
              Xavier Pinho <d12736@di.uminho.pt>

Tipos de dados e funções auxiliares para a realização do projeto de LI1 em 2022/23.
 -}
module LI12223 (
  -- * Tipos de dados
  -- ** Básicos
  Coordenadas , Largura , Velocidade,
  -- ** Mapas
  Mapa(..), Terreno(..), Obstaculo(..), Line(..),
    -- ** Jogo
  Jogo(..), Jogador(..), Direcao(..), Jogada(..)
  ) where
import Data.Binary (Binary)
import GHC.Generics
import Data.List (intercalate)

-- | Velocidade que irá afetar a movimentação dos 'Obstaculo's de um 'Mapa'.
type Velocidade = Int

{- | Cada linha de um 'Mapa' é um Terreno em particular contendo 'Obstaculo's.

As linhas do tipo 'Rio' ou 'Estrada' têm a propriedade 'Velocidade' que indicam a velocidade de deslocamento dos obstáculos. Podendo esta ser negativa, indicando assim a sua direção.
-}
data Terreno
  = Rio Velocidade
  | Estrada Velocidade
  | Relva
  deriving (Generic, Show, Read, Eq)
instance Binary Terreno

-- | Um Obstáculo numa linha de um 'Mapa'.
data Obstaculo
  = Nenhum -- ^ a ausência de obstáculos
  | Tronco -- ^ os troncos deslizam apenas em 'Rio'
  | Carro -- ^ os carros movimentam-se apenas em 'Estrada'
  | Arvore -- ^ as árvores são um obstáculo fixo que não se move e apenas são possíveis em 'Relva'
  deriving (Generic, Show, Read, Eq)
instance Binary Obstaculo

-- | Comprimento de um 'Mapa'.
type Largura = Int

-- | Linha de um 'Mapa'.
type Line = (Terreno, [Obstaculo])

{- |
 O Mapa que constituí o 'Jogo'.

@
 Um Mapa válido segue um conjunto de regras:
   - 1. Não existem 'Obstaculo's em 'Terreno's impróprios, e.g. troncos em estradas ou relvas, árvores em rios ou estradas, etc.
   - 2. Rios contíguos têm direcções opostas.
   - 3. Troncos têm, no máximo, 5 unidades de comprimento.
   - 4. Carros têm, no máximo, 3 unidades de comprimento.
   - 5. Em qualquer linha existe, no mínimo, um 'Obstaculo' 'Nenhum'. Ou seja, uma linha não pode ser composta exclusivamente por 'Obstaculo's,
   precisando de haver pelo menos um espaço livre.
   - 6. O comprimento da lista de obstáculos de cada linha corresponde exactamente à largura do mapa.
   - 7. Contiguamente, não devem existir mais do que 4 rios, nem 5 estradas
   ou relvas
@

-}
data Mapa =
  Mapa Largura [(Terreno, [Obstaculo])]
  deriving (Read, Eq)

#ifdef DEBUG
instance Show Mapa where
    show (Mapa w []) = "Mapa " ++ show w ++ " []"
    show (Mapa w l) = 
      "Mapa " ++ show w ++ " [\n" 
      ++ intercalate ",\n" (
            map (\(t,ol) -> 
                "  (" ++
                padString (show t) ' ' mTL ++ " ["
                ++ if null ol then "]" else
                    padString (
                        intercalate ", " (map (\o -> 
                            case o of {
                                Carro -> "Carro ";
                                _ -> show o;
                            }
                        ) ol)
                    ) ' ' mOL
                    ++ "]"
                ++ ")"
            ) l
        )
      ++ "\n]"
      where
        mTL = maximum (map (\(t,_) -> length (show t)) l)
        mOL = maximum (map (\(_,ol) -> 
                length (
                    intercalate ", " (map (\o -> 
                        case o of {
                            Carro -> "Carro ";
                            _ -> show o;
                        }
                    ) ol)
                )
            ) l)
#else
instance Show Mapa where
    show (Mapa w l) = "Mapa " ++ show w ++ " " ++ show l 
#endif

-- | Par de coordenadas de uma posição no 'Mapa'.
type Coordenadas = (Int, Int)

-- | O Jogador define o personagem controlado no 'Jogo'.
newtype Jogador =
  Jogador Coordenadas
  deriving (Generic, Show, Read, Eq)
instance Binary Jogador

-- | Definição base de um jogo.
data Jogo =
  Jogo
    Jogador -- ^ o personagem do jogo
    Mapa -- ^ o mapa em que se está a jogar
  deriving (Show, Read, Eq)

-- | Direção de uma 'Jogada' feita por um 'Jogador' no 'Mapa'.
data Direcao
  = Cima
  | Baixo
  | Esquerda
  | Direita
  deriving (Show, Read, Eq)

-- | As acções que podem ser tomadas pelo 'Jogador' em cada estado do 'Jogo'.
data Jogada
  = Parado -- ^ tipo que define a ausência de uma acção do 'Jogador'
  | Move Direcao -- ^ um movimento do jogador numa determinada 'Direcao'
  deriving (Show, Read, Eq)


-- | Original: 'Util.padString'
padString :: String -> Char -> Int -> String
padString s c l
    | l <= length s = s
    | otherwise = s ++ replicate (l - length s) c
