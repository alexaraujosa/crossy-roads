module Tarefa3_v2 where
import Render.Play
import Render.World.Level (
        getGameMap, setGameMap, killPlayer, getGame, getGamePlayerCoords, isInGame, setPlayerCoords, getGamePlayerOffset, 
        setPlayerOffset, setPlayer, getGamePlayer, setPlayerVelocity, getGamePlayerVelocity, getPlayerLevelPoints, isNoClip, 
        isGodMode
    )
import Data.Maybe (isNothing, fromJust)
import LI12223
import Util (getTerrenoConstr, shiftN, getTerrenoVel, getDecimal)
import Tarefa3_2022li1g003 hiding (attemptMove)
import Graphics.Gloss.Interface.IO.Game (Key(..))
import Graphics.Gloss.Interface.IO.Interact (SpecialKey(..))

-- | Determina o tamanho da área do jogo
displayDimension :: (Int,Int)
displayDimension = (900,900)

-- | Determina os FPS do jogo
fps :: Float
fps = 120

-- | A função @animaJogo@ anima 1 frame do mapa do jogo.
animaJogo :: GameState -> GameState
animaJogo = animaMapa . animaJogador

-- | A função @animaJogador@ anima 1 frame do Jogador.
animaJogador :: GameState -> GameState
animaJogador gs 
    | not $ isInGame gs = gs
    | o > 0.0 = ngs
    | o == 0 && (vx /= 0 || vy /= 0) = fromJust $ setPlayerCoords (fromJust $ setPlayerVelocity gs (0,0)) (ox+vx, oy+vy)
    | isNoClip gs && hasKeypresses gs = fromJust $ setPlayerVelocity ngs (mx + ovx, my)
    | isOnTronco gs && not (isNoClip gs) =
        if mx /= 0 || my /= 0 then
            fromJust $ setPlayerVelocity ngs (mx + ovx, my)
        else
            if hasKeypresses gs then gs else fromJust $ setPlayerCoords gs (ox + step, oy)
    | isInGame gs = 
        if hasKeypresses gs then 
                fromJust $ setPlayerVelocity ngs (mx + ovx, my)
        else gs
    | otherwise = gs
    where
        (Game p@(Player o (vx,vy) (ox,oy)) (Map _ off l)) = fromJust $ getGame gs
        ln = l !! round oy
        tv = getTerrenoVel $ fst ln
        v = if signum tv == 1 then max 1 tv else min (-1) tv
        step = fromIntegral v / fps
        os = off / fps
        (mx,my) = attemptMove gs

        -- Reiniciadas keypresses + avanço do jogador
        ngs = fromJust $ setPlayer (resetKeypresses gs) $ addPlayerStep p 2

        -- Corretor de Coordenadas
        ovx 
            | isOnTronco gs = 0
            | otherwise = if dovx > 0.5 then 1 - dovx else -dovx
            where
                dovx = getDecimal ox

-- | A função @attemptMove@ limita os movimentos do Jogador a movimentos válidos.
attemptMove :: GameState -> (Float,Float)
attemptMove gs = (attemptMoveX, attemptMoveY)
    where
        (xx,yy) = getTileSize mapa
        minX = -1
        minY = if getClock2Value gs > (3*yy/4) then 0 else 1
        maxX = fromIntegral w
        maxY = if getClock2Value gs > (3*yy/4) then fromIntegral $ length l -1 else fromIntegral $ length l
        (Game p@(Player o (vx,vy) (ox,oy)) mapa@(Map w off l)) = fromJust $ getGame gs
        
        ln0 = l !! round (max 0 (oy - 1))
        lt0 = getTerrenoConstr (fst ln0)

        ln = l !! round oy
        lt = getTerrenoConstr (fst ln)

        ln2 = l !! round (min (fromIntegral $ length l - 1) (oy + 1))
        lt2 = getTerrenoConstr (fst ln2)

        attemptMoveX
            | hasKey gs (SpecialKey KeyLeft) && ox - 1 <= minX = 0
            | hasKey gs (SpecialKey KeyRight) && ox + 1 >= maxX = 0
            | isNoClip gs && hasKey gs (SpecialKey KeyLeft) = -1
            | isNoClip gs && hasKey gs (SpecialKey KeyRight) = 1
            | hasKey gs (SpecialKey KeyLeft) && lt == 2 && (snd ln !! round (ox - 1) == Arvore) = 0
            | hasKey gs (SpecialKey KeyRight) && lt == 2 && (snd ln !! round (ox + 1) == Arvore) = 0
            | hasKey gs (SpecialKey KeyLeft) = -1
            | hasKey gs (SpecialKey KeyRight) = 1
            | otherwise = 0

        attemptMoveY
            | hasKey gs (SpecialKey KeyDown) && oy + 1 >= maxY = 0
            | hasKey gs (SpecialKey KeyUp) && oy - 1 < minY = 0
            | isNoClip gs && hasKey gs (SpecialKey KeyDown) = 1
            | isNoClip gs && hasKey gs (SpecialKey KeyUp) = -1
            | hasKey gs (SpecialKey KeyDown) && lt2 == 2 && (snd ln2 !! round ox == Arvore) = 0
            | hasKey gs (SpecialKey KeyUp) && lt0 == 2 && (snd ln0 !! round ox == Arvore) = 0
            | hasKey gs (SpecialKey KeyDown) = 1
            | hasKey gs (SpecialKey KeyUp) = -1
            | otherwise = 0

-- | A função @isOnTronco@ determina se o Jogador se encontra num tronco.
isOnTronco gs
    | isTronco l pox && os <= 0.5 = True
    | v > 0 && dox > 0.5 && isAgua l rpox && isTronco l (rpox - 1) && os >= 0.5 = True
    | v > 0 && dox > 0.5 && isAgua l (rpox - 1) && isTronco l rpox && os < 0.5 = True
    | v > 0 && isTronco l ptx && dox - fromIntegral ptx <= 0.5 = True
    | v > 0 && isAgua l ptx && isTronco l pox && dox - fromIntegral ptx <= 0.5 = True
    | v > 0 && isTronco l ntx && fromIntegral ntx - dox <= 0.5 = True
    
    | v < 0 && dox > 0.5 && isAgua l rpox && isTronco l (rpox + 1) && os >= 0.5 = True
    | v < 0 && dox > 0.5 && isAgua l (rpox + 1) && isTronco l rpox && os < 0.5 = True
    | v < 0 && isTronco l ntx && (1 - dox) - fromIntegral ntx <= 0.5 = True
    | v < 0 && isAgua l ptx && isTronco l pox && (1 - dox) - fromIntegral ptx <= 0.5 = True
    | v < 0 && isTronco l ntx && fromIntegral ntx - (1 - dox) <= 0.5 = True
    | otherwise = False
    where
        (Game (Player _ _ (ox,oy)) (Map w off ol)) = fromJust $ getGame gs
        os = off / fps
        dox = getDecimal ox
        pox = floor ox
        rpox = round ox
        ptx = max 0 (pox - 1)
        ntx = if v > 0 then max w (pox + 1) else min w (pox + 1)
        l = ol !! round oy
        v = getTerrenoVel $ fst l
        
-- | A função @hasKeypresses@ determina se alguma key está pressionada.
hasKeypresses :: GameState -> Bool
hasKeypresses gs = not $ null (keys $ internal gs)

-- | A função @hasKeypresses@ desativa todas as keys de movimento pressionadas.
resetKeypresses :: GameState -> GameState
resetKeypresses gs = foldr (flip removeKey) gs [SpecialKey KeyUp, SpecialKey KeyDown, SpecialKey KeyLeft, SpecialKey KeyRight]

-- | A função @animaMapa@ anima um 1 frame do Map.
animaMapa :: GameState -> GameState
animaMapa gs 
    | isNothing m = gs
    | otherwise = let {
        om = addStep (fromJust m) 1;
        (Map _ _ tl) = om;
        nm = moveMap om
        } in fromJust $ setGameMap gs nm
    where
        m = getGameMap gs

-- | A função @moveMap@ movimenta o Map em 1 frame.
moveMap :: Map -> Map
moveMap m@(Map w off tl) = Map w off (map (`moveObstacles` off) tl)

-- | A função @moveObstacles@ movimenta os obstáculos do Map em 1 frame.
moveObstacles :: Line -> MapOffset -> Line
moveObstacles (t,ol) off
    | t == Relva = (t,ol)
    | off == 0 = (t,shiftN v ol)
    | otherwise = (t,ol)
    where
        v = getTerrenoVel t

-- | A função @addPlayerStep@ adiciona @s@ ao Offset do Jogador.
addPlayerStep :: Player -> MapOffset -> Player
addPlayerStep (Player off v c) s
    | off + s >= (fps/5) = Player 0 v c
    | otherwise = Player (off + s) v c

-- | A função @addStep@ adiciona @s@ ao Offset do Map.
addStep :: Map -> MapOffset -> Map
addStep (Map w off tl) s
    | off + s >= fps = Map w 0 tl
    | otherwise = Map w (off + s) tl

jogoTerminou :: 
    GameState -- ^ Jogo a ser considerado.
    -> GameState -- ^ Resultado da verificação.
jogoTerminou gs -- = gs
    | fromIntegral (ceiling x) >= maxX || (x < 0) = fromJust $ killPlayer gs
    | y >= maxY || y < 0 = fromJust $ killPlayer gs
    | isGodMode gs = gs
    | signum v > 0 && getTerrenoConstr (fst ln) == 0 && getDecimal x < 0.5 && not (isTronco ln (floor x)) && os <= 0.5 = 
        fromJust $ killPlayer gs
    | signum v > 0 && getTerrenoConstr (fst ln) == 0 && getDecimal x >= 0.5 
        && not (isTronco ln (floor x) || isTronco ln (ceiling x)) && os <= 0.5 = fromJust $ killPlayer gs
    | signum v < 0 && isAgua ln (floor x) && not (isTronco ln (floor x + 1)) && os <= 0.5 = fromJust $ killPlayer gs
    | signum v < 0 && isTronco ln (ceiling x) && not (isTronco ln (floor x + 1)) && os >= 0.5 = fromJust $ killPlayer gs
    | isCarro ln (floor x) && os < 0.5 = fromJust $ killPlayer gs
    | isEstrada ln (ceiling x) && os > 0.5 && getRelativeTile ln (ceiling x) (-signum v) == Carro = fromJust $ killPlayer gs
    | otherwise = gs
        
    where 
        ln = l !! round y
        tv = getTerrenoVel $ fst ln
        v = if signum tv == 1 then max 1 tv else min (-1) tv
        step = fromIntegral v / fps
        os = off / fps

        minY = if getClock2Value gs > (3*yy/4) then 0 else 1
        maxX = fromIntegral w + 0.5 :: Float
        maxY = if getClock2Value gs > (3*yy/4) then fromIntegral $ length l -1 else fromIntegral $ length l
        (Game (Player _ _ (x,y)) (Map w off l)) = fromJust $ getGame gs
        or = off * fromIntegral (getTerrenoVel $ fst (l !! floor y)) / fps
        (xx,yy) = getTileSize mapa
        mapa = fromJust $ getGameMap gs

isTronco :: 
    (Terreno, [Obstaculo]) -- ^ Linha a ser considerada.
    -> Int                 -- ^ Coordenada 'x' a ser considerada.
    -> Bool                -- ^ Resultado da verificação.
isTronco (Rio _, l) n = length l > n && l !! n == Tronco
isTronco _ _ = False


isEstrada :: 
    (Terreno, [Obstaculo]) -- ^ Linha a ser considerada.
    -> Int                 -- ^ Coordenada 'x' a ser considerada.
    -> Bool                -- ^ Resultado da verificação.
isEstrada (Estrada _, l) n = length l > n && l !! n == Nenhum
isEstrada _ _ = False

-- | A função @getRelativeTile@ obtém o Obstáculo colocado a @n@ casas de uma posição @i@, na mesma linha.
getRelativeTile :: (Terreno,[Obstaculo]) -> Int -> Int -> Obstaculo
getRelativeTile (_,m) i n = shiftN (-n) m !! i

-- | A função @getTileSize@ calcula o tamanho gráfico de uma tile.
getTileSize :: Map -> (Float,Float)
getTileSize (Map w off l) = (fromIntegral ww / fromIntegral w, fromIntegral wh / fromIntegral (length l - 1))
    where (ww,wh) = displayDimension
