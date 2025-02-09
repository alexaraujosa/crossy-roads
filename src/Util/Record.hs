{- |
Module      : Util.Record
Description : Módulo de manipulação de Records
Copyright   : Rafael Santos Fernandes <a104271@alunos.uminho.pt>

Módulo de manipulação de Records. Permite obter e modificar propriedades de Records dinâmicamente.
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}

module Util.Record where

import Data.Typeable (Typeable, typeOf)
import Data.Data (Data, gfoldl, gmapQi, ConstrRep(AlgConstr), toConstr, constrRep, constrFields, gcast, cast)
import Data.List (elemIndex)
import Control.Arrow ((&&&))

-- | Representa um tipo genérico de transformação.
newtype T x = T { unT :: x -> x }

-- | Extende uma query genérica para uma query específica.
extQ :: (Typeable a, Typeable b) => (a -> q) -> (b -> q) -> a -> q
extQ f g a = maybe (f a) g (cast a)

-- | Extende uma transformação genérica para um caso específico a um tipo.
extT :: (Typeable a, Typeable b) => (a -> a) -> (b -> b) -> a -> a
extT def ext = unT ((T def) `ext0` (T ext))

-- | Extensão de tipo extensível.
ext0 :: (Typeable a, Typeable b) => c a -> c b -> c a
ext0 def ext = maybe def id (gcast ext)

-- | Representa uma Transformação Inteira.
data Ti a = Ti Int a

-- | Mapeia uma estrutura segundo uma Transformação Inteira.
gmapTi :: Data a => Int -> (forall b. Data b => b -> b) -> a -> a
gmapTi i f x = case gfoldl k z x of { Ti _ a -> a }
    where
        k :: Data d => Ti (d->b) -> d -> Ti b
        k (Ti i' c) a = Ti (i'+1) (if i==i' then c (f a) else c a)
        z :: g -> Ti g
        z = Ti 0

---------------------------------------------------------------------------------------

-- | Obtém as chaves das entradas de dados num Record.
fieldNames :: (Data r) => r -> [String]
fieldNames rec = case (constrRep &&& constrFields) $ toConstr rec of {
        (AlgConstr _, fs) | not $ null fs -> fs;
        _ -> error "Not a record type";
    }

-- | Garante que uma String é uma chave de entrada de um Record.
fieldIndex :: (Data r) => String -> r -> Int
fieldIndex fieldName rec = case fieldName `elemIndex` fieldNames rec of {
        Just i  -> i;
        Nothing -> error $ "No such field: " ++ fieldName;
    }

-- | Modifica o valor de uma propriedade de um Record.
modifyField :: (Data r, Typeable v) => String -> (v -> v) -> r -> r
modifyField fieldName m rec = gmapTi i (e `extT` m) rec
    where
        i = fieldName `fieldIndex` rec
        e x = error $ "Type mismatch: " ++ fieldName ++ " :: " ++ (show . typeOf $ x) ++ ", not " 
            ++ (show . typeOf $ m undefined)

-- | Modifica uma propriedade de um Record.
setField :: (Data r, Typeable v) => String -> v -> r -> r
setField fieldName value = modifyField fieldName (const value)

-- | Obtém uma propriedade de um Record. Necesita de uma assinatura de tipo estrita.
getField :: (Data r, Typeable v) => String -> r -> v
getField fieldName rec = gmapQi i (e `extQ` id) rec
    where
        i = fieldName `fieldIndex` rec
        e x = error $ "Type mismatch: " ++ fieldName ++ " :: " ++ (show . typeOf $ x) ++ ", not " 
            ++ (show . typeOf $ e undefined)