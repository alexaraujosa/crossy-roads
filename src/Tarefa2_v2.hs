module Tarefa2_v2 where

import Render.Play (Map (..))
import LI12223
import Util

{- |
 A função @estendeMapa@ gera e adiciona uma nova linha válida ao topo (i.e. primeira linha, visto de cima para baixo) 
de um dado mapa.
-}
estendeMapa :: 
    Map    -- ^ O Mapa a ser estendido.
    -> Int  -- ^ Um número aleatório.
    -> Int  -- ^ Número a ser considerado.
    -> Map -- ^ O mapa estendido.
estendeMapa m@(Map w off lo) r n = 
    Map w off (fst (lineBuilder (fst o,[]) (if not $ null lo then head lo else fbo) r n) : lo) --undefined
    where
        -- Terrenos Válidos para a próxima linha
        pt = proximosTerrenosValidos m
        -- Os obstáculos válidos para cada tipo de terreno.
        po = map (\x -> (x, proximosObstaculosValidos w (x,[]))) pt
        -- Tuplo (Terreno,[Obstaculo]) escolhido através do número pseudo-aleatório @r@.
        o = getPRndElem po r 
        -- Tuplo (Terreno,[Obstaculo]) utilizado caso o Mapa seja vazio.
        fbo = getPRndElem (filter (/= o) po) (r + getPRand r !! fromInteger (scrambleNum (toInteger r)))

lineBuilder :: 
    (Terreno, [Obstaculo])              -- ^ A linha a ser criada.
    -> (Terreno, [Obstaculo])           -- ^ A linha anterior á linha a ser criada.
    -> Int                              -- ^ Uma seed aleatória.
    -> Int                              -- ^ Número a ser considerado.
    -> ((Terreno, [Obstaculo]),Int)     -- ^ Um tuplo contendo a linha gerada e a nova iteração do número aleatório fornecido.
lineBuilder (ct,co) (lt,[]) r _
    | ti == 0 = ((Rio nv,[]),r+1)
    | ti == 1 = ((Estrada nv,[]),r+1)
    | otherwise = ((Relva,[]),r+1)
    where
        nv = ceiling (fromIntegral $ r `div` 10)
        ti = getTerrenoConstr ct
lineBuilder (ct,co) (lt,lo) r n
    | ti == 0 = ((Rio nv,obsts),mod (head $ tail nrl) 100)
    | ti == 1 = ((Estrada nv,obsts),mod (head $ tail nrl) 100)
    | otherwise = ((Relva,obsts),mod (head $ tail nrl) 100)
    where
        lti = getTerrenoConstr lt
        ti = getTerrenoConstr ct
        seed = toInteger r
        rl = getPRand r

        ov = if getTerrenoConstr lt /= 2 then getTerrenoVel lt else 0
        cv = withinBoundsWA 1 5 $ ceiling (fromIntegral (div r 100))
        nv
            | getTerrenoConstr lt == 0 && getTerrenoConstr ct == 0 = if ov > 0 then -cv else cv
            | otherwise = cv
        e = map snd $ filterIndexed (\x _ -> x == if lti == 0 then Tronco else Nenhum) lo
        ei = e !! mod (rl !! fromInteger (scrambleNum $ toInteger (r + length e))) (length e)

        po = proximosObstaculosValidos (length lo) (ct,co)
        (_obsts,nrl) = mapWithCacheR 
            (\a b c -> 
                (tail a, 
                    if ti == 0 then
                        if length c >= 5 && all ((== Tronco) . (c !!)) [(length c) .. length c -5] then 
                            filter (/=Tronco) po !! mod (head a) (length $ filter (/=Tronco) po)
                        else po !! mod (head a) (length po)
                    else po !! mod (head a) (length po)
                )
            ) 
            (getPRand (rl !! fromInteger (scrambleNum $ toInteger (r + 66)))) 
            [1..(length lo)]
        
        -- Garante que existe pelo menos um caminho de continuação válido
        _obsts2 
            | any (\x -> _obsts !! x == if ti == 0 then Tronco else Nenhum) e = _obsts
            | otherwise = let (olh,_:olt) = splitAt ei _obsts in olh ++ [if ti == 0 then Tronco else Nenhum] ++ olt

        obsts
            | elem Nenhum _obsts2 = _obsts2
            | otherwise = let (olh,_:olt) = splitAt sii _obsts in olh ++ [Nenhum] ++ olt
                where
                    sis = filter (/= ei) [0..length _obsts2 - 1]
                    sii = sis !! mod (nrl !! fromInteger (scrambleNum $ toInteger (r + length sis))) (length sis)

        nr = head nrl        

{- |
 A função @proximosTerrenosValidos@ deve gerar a lista de terrenos passíveis de serem usados numa nova linha no topo do mapa 
dado.
-}
proximosTerrenosValidos :: 
    Map          -- ^ O mapa sobre o qual avaliar os próximos terrenos válidos.
    -> [Terreno] -- ^ A lista dos próximos terrenos válidos relativamente ao mapa fornecido.
proximosTerrenosValidos (Map _ _ []) = [Rio 0,Estrada 0,Relva] -- Mapa não possuí linhas, todos os terrenos são válidos.
proximosTerrenosValidos (Map _ _ o@((lt,_):t))
    | ti == 0 = if length (filter (filterTerreno 0 . fst) (take 4 o)) == 4 then [Estrada 0,Relva] else [Rio 0,Estrada 0,Relva]
    | ti == 1 = if length (filter (filterTerreno 1 . fst) (take 5 o)) == 5 then [Rio 0,Relva] else [Rio 0,Estrada 0,Relva]
    | otherwise = if length (filter (filterTerreno 2 . fst) (take 5 o)) == 5 then [Rio 0,Estrada 0] else [Rio 0,Estrada 0,Relva]
    where
        ti = getTerrenoConstr lt
        filterTerreno tid tr = getTerrenoConstr tr == tid

{- |
 A função @proximosObstaculosValidos@ gera a lista de obstáculos passíveis de serem usados para continuar uma dada linha do 
mapa.
-}
proximosObstaculosValidos :: 
    Int                         -- ^ A largura do mapa.
    -> (Terreno, [Obstaculo])   -- ^ A linha para a qual gerar obstáculos.
    -> [Obstaculo]              -- ^ Os tipos de obstáculos válidos para a linha.
proximosObstaculosValidos 0 _ = []
proximosObstaculosValidos w (t,o)
    | length o >= w = []
    | length o == w - 1 && notElem Nenhum o = if elem Nenhum o then [Nenhum,[Tronco,Carro,Arvore] !! ti] else [Nenhum]
    | otherwise = [Nenhum,[Tronco,Carro,Arvore] !! ti]
    where
        ti = getTerrenoConstr t
