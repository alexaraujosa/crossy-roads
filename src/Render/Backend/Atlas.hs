{- |
Module      : Atlas
Description : Módulo de leitura e processamento de Texture Atlas.
Copyright   : Rafael Santos Fernandes <a104271@alunos.uminho.pt>

Módulo de leitura e processamento de Texture Atlas, permitindo que várias texturas se encontrem num único ficheiro de imagem,
permitindo um uso mais eficiente da memória.

Um Texture Atlas necessita de ser uma imagem de 24-bits para evitar a utilização de um offset para as coordenadas.
-}

module Render.Backend.Atlas where
import Graphics.Gloss
import Data.Map (Map)
import qualified Data.Map as Map

-- | Representa um Texture Atlas.
type Atlas = BitmapData

-- | Representa uma secção de um Texture Atlas.
data AtlasSection = AtlasSection
    (Int,Int) -- ^ Representa a posição (x,y) de uma secção. As posições são calculadas a partir do canto inferior esquerdo.
              -- Os valores de x e de y aumentam no sentido esquerda-direita e baixo-cima, respetivamente.
    (Int,Int) -- ^ Representa o tamanho da secção. Segue as mesmas regras da posição.

-- | Converte uma 'Picture' num 'Atlas'
pic2bmp :: Picture -> Atlas
pic2bmp (Bitmap bmpData) = bmpData

-- | Carrega um 'Atlas' a partir de um ficheiro.
loadAtlas :: String -> IO Atlas
loadAtlas fn = do
    img <- loadBMP (fn ++ ".bmp")
    return $ pic2bmp img

-- | Obtém uma secção de um 'Atlas'.
-- __AVISO:__ Ver 'AtlasSection'.
getAtlasSection :: Atlas -> AtlasSection -> Picture
getAtlasSection a (AtlasSection (x,y) s@(_,sh)) = section
    where
        (w,h) = getAtlasSize a
        mask = Rectangle (x,h-y-sh) s
        section = bitmapSection mask a

-- | Obtém o tamanho de um 'Atlas'.
getAtlasSize :: Atlas -> (Int, Int)
getAtlasSize = bitmapSize

{- | 
 Representa um Sprite num 'Atlas'.

 As posições são calculadas a partir do canto inferior esquerdo. Os valores de x e de y aumentam no sentido esquerda-direita e 
baixo-cima, respetivamente.
-}
data OffsetEntry = OffsetEntry {
    x :: Int, -- ^ Representa a posição x de um Sprite.
    y :: Int, -- ^ Representa a posição y de um Sprite.
    w :: Int, -- ^ Representa a largura de um Sprite.
    h :: Int  -- ^ Representa a altura de um Sprite.
}

-- | Representa os Sprites de um 'Atlas'.
type OffsetAtlas = Map String OffsetEntry

-- | Representa uma tabela de Offsets de um 'Atlas'.
data Offsets = Offsets {
    atlas :: OffsetAtlas
}

-- | Transforma um mapa de Sprites em um mapa de 'Pictures'
parseAtlasSector :: Atlas -> OffsetAtlas -> Map String Picture
parseAtlasSector a ol = Map.fromList $ map (\(id,OffsetEntry x y w h) -> (id,getAtlasSection a (AtlasSection (x,y) (w,h)))) kvp
    where
        kvp = Map.assocs ol