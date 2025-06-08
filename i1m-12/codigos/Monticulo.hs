-- Monticulo.hs
-- TAD montículo e implementación.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 11 de Septiembre de 2010
-- ---------------------------------------------------------------------

-- Un montículo es un árbol binario en el que los valores de cada nodo es
-- menor o igual que los valores de sus hijos. Por ejemplo,
--         1
--        / \
--       /   \
--      2     6
--     / \   / \
--    3   8 9   7
-- es un montículo, pero
--         1
--        / \
--       /   \
--      3     6
--     / \   / \
--    4   2 9   7
-- no lo es.

module Monticulo
    (Monticulo,
     vacio,   -- Ord a => Monticulo a
     inserta, -- Ord a => a -> Monticulo a -> Monticulo a
     menor,   -- Ord a => Monticulo a -> a
     resto,   -- Ord a => Monticulo a -> Monticulo a
     esVacio, -- Ord a => Monticulo a -> Bool
     valido   -- Ord a => Monticulo a -> Bool
    ) where 

import Data.List (sort)

-- Implementación de montículos mediante árboles izquierdistas ("leftist
-- tree"). 
data Ord a => Monticulo a = Vacio
                          | M a Int (Monticulo a) (Monticulo a)
                            deriving Show

-- Ejemplos de montículos
--    ghci> m1
--    M 1 2 (M 4 1 (M 8 1 Vacio Vacio) Vacio) (M 6 1 Vacio Vacio)
--    ghci> m2
--    M 5 1 (M 7 1 Vacio Vacio) Vacio
--    ghci> m3
--    M 1 2 
--      (M 5 2 
--         (M 7 1 Vacio Vacio) 
--         (M 6 1 Vacio Vacio)) 
--      (M 4 1 
--         (M 8 1 Vacio Vacio) 
--         Vacio)
-- Gráficamente
--            m1             m2                m3
--        
--                                             (1,2) 
--            (1,2)          (5,1)            /     \
--           /     \        /                /       \
--        (4,1)   (6,1)  (7,1)           (5,2)        (4,1)
--       /                              /     \       /
--    (8,1)                          (7,1)   (6,1)  (8,1)
m1, m1', m2, m3 :: Monticulo Int
m1  = foldr inserta vacio [6,1,4,8]
m1' = foldr inserta vacio [6,8,4,1]
m2  = foldr inserta vacio [7,5]
m3 = mezcla m1 m2

-- vacio es el montículo vacío.
vacio :: Ord a => Monticulo a
vacio = Vacio

-- (rango m) es el rango del montículo m; es decir, la menor distancia
-- a un montículo vacío. Por ejemplo,
--    rango m1  ==  2
--    rango m2  ==  1
rango :: Ord a => Monticulo a -> Int
rango Vacio       = 0
rango (M _ r _ _) = r

-- (creaM x a b) es el montículo creado a partir del elemento x y los
-- montículos a y b. Se supone que x es menor o igual que el mínimo de
-- a y de b. Por ejemplo,
--    ghci> m1
--    M 1 2 (M 4 1 (M 8 1 Vacio Vacio) Vacio) (M 6 1 Vacio Vacio)
--    ghci> m2
--    M 5 1 (M 7 1 Vacio Vacio) Vacio
--    ghci> creaM 0 m1 m2
--    M 0 2 (M 1 2 (M 4 1 (M 8 1 Vacio Vacio) Vacio) (M 6 1 Vacio Vacio)) 
--          (M 5 1 (M 7 1 Vacio Vacio) Vacio)
--    ghci> creaM 0 m2 m1
--    M 0 2 (M 1 2 (M 4 1 (M 8 1 Vacio Vacio) Vacio) (M 6 1 Vacio Vacio)) 
--          (M 5 1 (M 7 1 Vacio Vacio) Vacio)
creaM :: Ord a => a -> Monticulo a -> Monticulo a -> Monticulo a
creaM x a b | rango a >= rango b = M x (rango b + 1) a b
            | otherwise          = M x (rango a + 1) b a

-- (mezcla m1 m2) es el montículo obtenido mezclando los montículos m1 y
-- m2. Por ejemplo,
--    ghci> mezcla m1 m2
--    M 1 2 
--      (M 5 2 
--         (M 7 1 Vacio Vacio) 
--         (M 6 1 Vacio Vacio)) 
--      (M 4 1 
--         (M 8 1 Vacio Vacio) 
--         Vacio)
mezcla :: Ord a =>  Monticulo a -> Monticulo a -> Monticulo a
mezcla h Vacio = h
mezcla Vacio h = h
mezcla h1@(M x _ a1 b1) h2@(M y _ a2 b2)
      | x <= y    = creaM x a1 (mezcla b1 h2)
      | otherwise = creaM y a2 (mezcla h1 b2)

-- (inserta x m) es el montículo obtenido añadiendo el elemento x al
-- montículo m. Por ejemplo, 
--    ghci> m1
--    M 1 2 (M 4 1 (M 8 1 Vacio Vacio) Vacio) (M 6 1 Vacio Vacio)
--    ghci> inserta 3 m1
--    M 1 2 
--      (M 4 1 (M 8 1 Vacio Vacio) Vacio) 
--      (M 3 1 (M 6 1 Vacio Vacio) Vacio)
inserta :: Ord a => a -> Monticulo a -> Monticulo a
inserta x m = mezcla (M x 1 Vacio Vacio) m

-- (menor m) es el menor elemento del montículo m. Por ejemplo, 
--   menor m1  ==  1
--   menor m2  ==  5
menor  :: Ord a => Monticulo a -> a
menor (M x _ _ _) = x
menor Vacio       = error "menor: monticulo vacio"

-- (resto m) es el montículo obtenido eliminando el menor elemento del
-- montículo m. Por ejemplo, 
--    ghci> m1
--    M 1 2 (M 4 1 (M 8 1 Vacio Vacio) Vacio) (M 6 1 Vacio Vacio)
--    ghci> resto m1
--    M 4 2 (M 8 1 Vacio Vacio) (M 6 1 Vacio Vacio)
resto :: Ord a => Monticulo a -> Monticulo a
resto Vacio       = error "resto: monticulo vacio"
resto (M x _ a b) = mezcla a b

-- (esVacio m) se verifica si m es el montículo vacío.
esVacio :: Ord a => Monticulo a -> Bool
esVacio Vacio = True
esVacio _     = False

-- (valido m) se verifica si m es un montículo; es decir, es un árbol
-- binario en el que los valores de cada nodo es menor o igual que los
-- valores de sus hijos. Por ejemplo, 
--    valido m1  ==  True
--    valido m2  ==  True
--    valido m3  ==  True
--    valido (M 3 5 (M 2 1 Vacio Vacio) Vacio)  ==  False
valido :: Ord a => Monticulo a -> Bool
valido Vacio = True
valido (M x _ Vacio Vacio) = True
valido (M x _ m1@(M x1 n1 a1 b1) Vacio) = 
    x <= x1 && valido m1
valido (M x _ Vacio m2@(M x2 n2 a2 b2)) = 
    x <= x2 && valido m2
valido (M x _ m1@(M x1 n1 a1 b1) m2@(M x2 n2 a2 b2)) = 
    x <= x1 && valido m1 &&
    x <= x2 && valido m2

-- (elementos m) es la lista de los elementos del montículo m. Por
-- ejemplo, 
--    elementos m1  ==  [1,4,8,6]
elementos :: Ord a => Monticulo a -> [a]
elementos Vacio       = []
elementos (M x _ a b) = x : elementos a ++ elementos b

-- (equivMonticulos m1 m2) se verifica si los montículos m1 y m2 tienen
-- los mismos elementos. Por ejemplo,
--    ghci> m1
--    M 1 2 (M 4 1 (M 8 1 Vacio Vacio) Vacio) (M 6 1 Vacio Vacio)
--    ghci> m1'
--    M 1 2 (M 4 1 Vacio Vacio) (M 6 1 (M 8 1 Vacio Vacio) Vacio)
--    ghci> equivMonticulos m1 m1'
--    True
equivMonticulos :: Ord a => Monticulo a -> Monticulo a -> Bool
equivMonticulos m1 m2 = 
    sort (elementos m1) == sort (elementos m2)

-- Los montículos son comparables por igualdad.
instance Ord a => Eq (Monticulo a) where
   (==) = equivMonticulos

-- ---------------------------------------------------------------------
-- Funciones auxiliares                                               --
-- ---------------------------------------------------------------------

-- (menorTodos x m) comprueba si x es menor que todos los elementos de m
menorTodos:: Ord a => a -> Monticulo a -> Bool
menorTodos x Vacio      = True
menorTodos x (M y n a b) = x <= y && valido (M y n a b)

-- (enMonticulo x m) se verifica si x es un elemento del montículo
-- m. Por ejemplo, 
--    enMonticulo 4 m1  ==  True
--    enMonticulo 5 m1  ==  False
enMonticulo x Vacio      = False
enMonticulo x (M y _ a b) 
    | x < y     = False
    | x == y    = True
    | otherwise = enMonticulo x a || enMonticulo x b



