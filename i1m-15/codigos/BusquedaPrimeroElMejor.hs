-- BusquedaPrimeroElMejor.hs
-- Búsqueda por primero el mejor.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 21 de Noviembre de 2010
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Importaciones                                                      --
-- ---------------------------------------------------------------------

-- Nota: Hay que elegir una implementación de las colas de prioridad.
-- import ColaDePrioridadConListas
import ColaDePrioridadConMonticulos

import Data.Array

-- ---------------------------------------------------------------------
-- Búsqueda por primero el mejor                                      --
-- ---------------------------------------------------------------------

-- (buscaPM s o e) es la lista de soluciones del problema de espacio de
-- estado definido por la función sucesores (s), el objetivo (o) y el
-- estado inicial (e), obtenidas buscando por primero el mejor.
buscaPM :: (Ord nodo) => 
           (nodo -> [nodo])   -- sucesores
           -> (nodo -> Bool)  -- esFinal
           -> nodo            -- nodo actual
           -> [(nodo,Int)]    -- solución
buscaPM sucesores esFinal x = busca' (inserta x vacia) 0
 where
   busca' c t
    | esVacia c = []
    | esFinal (primero c)  
        = ((primero c),t+1):(busca' (resto c)(t+1))
    | otherwise            
        = busca' (foldr inserta (resto c) (sucesores x)) (t+1)
          where x = primero c

-- ---------------------------------------------------------------------
-- El problema del 8 puzzle                                           --
-- ---------------------------------------------------------------------

-- Representación del problema:
-- ============================

-- Nota: La representación del problema está copiado de
-- BusquedaEnEspaciosDeEstados.hs 

-- Una posición es un par de enteros.
type Posicion = (Int,Int)

-- Un tablero es un vector de posiciones, en el que el índice indica el
-- elemento que ocupa la posición.
type Tablero  = Array Int Posicion

-- inicial8P es el estado inicial del 8 puzzle. En el ejemplo es
--      +---+---+---+
--      | 2 | 6 | 3 | 
--      +---+---+---+ 
--      | 5 |   | 4 | 
--      +---+---+---+ 
--      | 1 | 7 | 8 | 
--      +---+---+---+ 
inicial8P :: Tablero 
inicial8P = array (0,8) [(2,(1,3)),(6,(2,3)),(3,(3,3)),
                         (5,(1,2)),(0,(2,2)),(4,(3,2)),
                         (1,(1,1)),(7,(2,1)),(8,(3,1))]

-- final8P es el estado final del 8 puzzle. En el ejemplo es
--      +---+---+---+
--      | 1 | 2 | 3 | 
--      +---+---+---+ 
--      | 8 |   | 4 | 
--      +---+---+---+ 
--      | 7 | 6 | 5 | 
--      +---+---+---+ 
final8P :: Tablero
final8P = array (0,8) [(1,(1,3)),(2,(2,3)),(3,(3,3)),
                       (8,(1,2)),(0,(2,2)),(4,(3,2)),
                       (7,(1,1)),(6,(2,1)),(5,(3,1))]

-- (distancia p1 p2) es la distancia Manhatan entre las posiciones p1 y
-- p2. Por ejemplo,
--    distancia (2,7) (4,1)  ==  8
distancia :: Posicion -> Posicion -> Int
distancia (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)

-- (adyacente p1 p2) se verifica si las posiciones p1 y p2 son
-- adyacentes. Por ejemplo,
--    adyacente (3,2) (3,1)  ==  True
--    adyacente (3,2) (1,2)  ==  False
adyacente :: Posicion -> Posicion -> Bool
adyacente p1 p2 = distancia p1 p2 == 1

-- (todosMovimientos t) es la lista de los tableros obtenidos
-- aplicándole al tablero t todos los posibles movimientos; es decir,
-- intercambiando la posición del hueco con sus adyacentes. Por ejemplo, 
--    *Main> inicial8P
--    array (0,8) [(0,(2,2)),(1,(1,1)),(2,(1,3)),(3,(3,3)),(4,(3,2)),
--                 (5,(1,2)),(6,(2,3)),(7,(2,1)),(8,(3,1))]
--    *Main> todosMovimientos inicial8P
--    [array (0,8) [(0,(3,2)),(1,(1,1)),(2,(1,3)),(3,(3,3)),(4,(2,2)),
--                  (5,(1,2)),(6,(2,3)),(7,(2,1)),(8,(3,1))],
--     array (0,8) [(0,(1,2)),(1,(1,1)),(2,(1,3)),(3,(3,3)),(4,(3,2)),
--                  (5,(2,2)),(6,(2,3)),(7,(2,1)),(8,(3,1))],
--     array (0,8) [(0,(2,3)),(1,(1,1)),(2,(1,3)),(3,(3,3)),(4,(3,2)),
--                  (5,(1,2)),(6,(2,2)),(7,(2,1)),(8,(3,1))],
--     array (0,8) [(0,(2,1)),(1,(1,1)),(2,(1,3)),(3,(3,3)),(4,(3,2)),
--                  (5,(1,2)),(6,(2,3)),(7,(2,2)),(8,(3,1))]]
todosMovimientos :: Tablero -> [Tablero]
todosMovimientos t = [t//[(0,t!i),(i,t!0)] | i<-[1..8], adyacente (t!0) (t!i)] 

-- Los nodos del espacio de estados son listas de tableros [t_n,...,t_1]
-- tal que t_i es un sucesor de t_(i-1).
data Tableros = Est [Tablero] deriving Show

-- (sucesores8P e) es la lista de sucesores del estado e. Por ejemplo,
--    *Main> sucesores8P (Est [inicial8P])
--    [Est [array (0,8) [(0,(3,2)),(1,(1,1)),(2,(1,3)),(3,(3,3)),(4,(2,2)),
--                       (5,(1,2)),(6,(2,3)),(7,(2,1)),(8,(3,1))],
--          array (0,8) [(0,(2,2)),(1,(1,1)),(2,(1,3)),(3,(3,3)),(4,(3,2)),
--                       (5,(1,2)),(6,(2,3)),(7,(2,1)),(8,(3,1))]],
--    Est [array (0,8) [(0,(1,2)),(1,(1,1)),(2,(1,3)),(3,(3,3)),(4,(3,2)),
--                      (5,(2,2)),(6,(2,3)),(7,(2,1)),(8,(3,1))],
--         array (0,8) [(0,(2,2)),(1,(1,1)),(2,(1,3)),(3,(3,3)),(4,(3,2)),
--                      (5,(1,2)),(6,(2,3)),(7,(2,1)),(8,(3,1))]],
--    Est [array (0,8) [(0,(2,3)),(1,(1,1)),(2,(1,3)),(3,(3,3)),(4,(3,2)),
--                      (5,(1,2)),(6,(2,2)),(7,(2,1)),(8,(3,1))],
--         array (0,8) [(0,(2,2)),(1,(1,1)),(2,(1,3)),(3,(3,3)),(4,(3,2)),
--                      (5,(1,2)),(6,(2,3)),(7,(2,1)),(8,(3,1))]],
--    Est [array (0,8) [(0,(2,1)),(1,(1,1)),(2,(1,3)),(3,(3,3)),(4,(3,2)),
--                      (5,(1,2)),(6,(2,3)),(7,(2,2)),(8,(3,1))],
--         array (0,8) [(0,(2,2)),(1,(1,1)),(2,(1,3)),(3,(3,3)),(4,(3,2)),
--                      (5,(1,2)),(6,(2,3)),(7,(2,1)),(8,(3,1))]]]
sucesores8P :: Tableros -> [Tableros]
sucesores8P (Est(n@(t:ts))) = 
    filter (noEn ts) [ Est (t':n) | t' <- todosMovimientos t]
    where noEn ts (Est(t:_)) = not (elem (elems t) (map elems ts))

esFinal8P :: Tableros -> Bool
esFinal8P (Est (n:_)) = elems n == elems final8P

-- Heurísticas
-- ===========

-- (heur1 t) es la suma de la distancia Manhatan desde la posición de
-- cada objeto del tablero a su posición en el estado final. Por
-- ejemplo,
--    heur1 inicial8P  ==  12
heur1 :: Tablero  -> Int
heur1 b = 
    sum [distancia (b!i) (final8P!i) | i <- [0..8]]

-- Dos estados se consideran iguales si tienen la misma heurística.
instance Eq Tableros
    where Est(t1:_) == Est(t2:_) = heur1 t1 == heur1 t2

-- Un estado es menor o igual que otro si tiene una heurística menor o
-- igual. 
instance Ord Tableros where 
    Est (t1:_) <= Est (t2:_) = heur1 t1 <= heur1 t2

-- (buscaPM_8P) es la lista de las soluciones del 8 puzzle por búsqueda
-- primero el mejor. Por ejemplo,
--    ghci> head buscaPM_8P
--    (Est [array (0,8) [(0,(2,2)),(1,(1,3)),(2,(2,3)),(3,(3,3)),(4,(3,2)),
--                       (5,(3,1)),(6,(2,1)),(7,(1,1)),(8,(1,2))],
--          array (0,8) [(0,(2,1)),(1,(1,3)),(2,(2,3)),(3,(3,3)),(4,(3,2)),
--                       (5,(3,1)),(6,(2,2)),(7,(1,1)),(8,(1,2))],
--          array (0,8) [(0,(1,1)),(1,(1,3)),(2,(2,3)),(3,(3,3)),(4,(3,2)),
--                       (5,(3,1)),(6,(2,2)),(7,(2,1)),(8,(1,2))],
--          array (0,8) [(0,(1,2)),(1,(1,3)),(2,(2,3)),(3,(3,3)),(4,(3,2)),
--                       (5,(3,1)),(6,(2,2)),(7,(2,1)),(8,(1,1))],
--          array (0,8) [(0,(2,2)),(1,(1,3)),(2,(2,3)),(3,(3,3)),(4,(3,2)),
--                       (5,(3,1)),(6,(1,2)),(7,(2,1)),(8,(1,1))],
--          array (0,8) [(0,(2,1)),(1,(1,3)),(2,(2,3)),(3,(3,3)),(4,(3,2)),
--                       (5,(3,1)),(6,(1,2)),(7,(2,2)),(8,(1,1))],
--          array (0,8) [(0,(3,1)),(1,(1,3)),(2,(2,3)),(3,(3,3)),(4,(3,2)),
--                       (5,(2,1)),(6,(1,2)),(7,(2,2)),(8,(1,1))],
--          array (0,8) [(0,(3,2)),(1,(1,3)),(2,(2,3)),(3,(3,3)),(4,(3,1)),
--                       (5,(2,1)),(6,(1,2)),(7,(2,2)),(8,(1,1))],
--          array (0,8) [(0,(2,2)),(1,(1,3)),(2,(2,3)),(3,(3,3)),(4,(3,1)),
--                       (5,(2,1)),(6,(1,2)),(7,(3,2)),(8,(1,1))],
--          array (0,8) [(0,(1,2)),(1,(1,3)),(2,(2,3)),(3,(3,3)),(4,(3,1)),
--                       (5,(2,1)),(6,(2,2)),(7,(3,2)),(8,(1,1))],
--          array (0,8) [(0,(1,1)),(1,(1,3)),(2,(2,3)),(3,(3,3)),(4,(3,1)),
--                       (5,(2,1)),(6,(2,2)),(7,(3,2)),(8,(1,2))],
--          array (0,8) [(0,(2,1)),(1,(1,3)),(2,(2,3)),(3,(3,3)),(4,(3,1)),
--                       (5,(1,1)),(6,(2,2)),(7,(3,2)),(8,(1,2))],
--          array (0,8) [(0,(2,2)),(1,(1,3)),(2,(2,3)),(3,(3,3)),(4,(3,1)),
--                       (5,(1,1)),(6,(2,1)),(7,(3,2)),(8,(1,2))],
--          array (0,8) [(0,(3,2)),(1,(1,3)),(2,(2,3)),(3,(3,3)),(4,(3,1)),
--                       (5,(1,1)),(6,(2,1)),(7,(2,2)),(8,(1,2))],
--          array (0,8) [(0,(3,1)),(1,(1,3)),(2,(2,3)),(3,(3,3)),(4,(3,2)),
--                       (5,(1,1)),(6,(2,1)),(7,(2,2)),(8,(1,2))],
--          array (0,8) [(0,(2,1)),(1,(1,3)),(2,(2,3)),(3,(3,3)),(4,(3,2)),
--                       (5,(1,1)),(6,(3,1)),(7,(2,2)),(8,(1,2))],
--          array (0,8) [(0,(1,1)),(1,(1,3)),(2,(2,3)),(3,(3,3)),(4,(3,2)),
--                       (5,(2,1)),(6,(3,1)),(7,(2,2)),(8,(1,2))],
--          array (0,8) [(0,(1,2)),(1,(1,3)),(2,(2,3)),(3,(3,3)),(4,(3,2)),
--                       (5,(2,1)),(6,(3,1)),(7,(2,2)),(8,(1,1))],
--          array (0,8) [(0,(2,2)),(1,(1,3)),(2,(2,3)),(3,(3,3)),(4,(3,2)),
--                       (5,(2,1)),(6,(3,1)),(7,(1,2)),(8,(1,1))],
--          array (0,8) [(0,(2,1)),(1,(1,3)),(2,(2,3)),(3,(3,3)),(4,(3,2)),
--                       (5,(2,2)),(6,(3,1)),(7,(1,2)),(8,(1,1))],
--          array (0,8) [(0,(3,1)),(1,(1,3)),(2,(2,3)),(3,(3,3)),(4,(3,2)),
--                       (5,(2,2)),(6,(2,1)),(7,(1,2)),(8,(1,1))],
--          array (0,8) [(0,(3,2)),(1,(1,3)),(2,(2,3)),(3,(3,3)),(4,(3,1)),
--                       (5,(2,2)),(6,(2,1)),(7,(1,2)),(8,(1,1))],
--          array (0,8) [(0,(2,2)),(1,(1,3)),(2,(2,3)),(3,(3,3)),(4,(3,1)),
--                       (5,(3,2)),(6,(2,1)),(7,(1,2)),(8,(1,1))],
--          array (0,8) [(0,(2,1)),(1,(1,3)),(2,(2,3)),(3,(3,3)),(4,(3,1)),
--                       (5,(3,2)),(6,(2,2)),(7,(1,2)),(8,(1,1))],
--          array (0,8) [(0,(1,1)),(1,(1,3)),(2,(2,3)),(3,(3,3)),(4,(3,1)),
--                       (5,(3,2)),(6,(2,2)),(7,(1,2)),(8,(2,1))],
--          array (0,8) [(0,(1,2)),(1,(1,3)),(2,(2,3)),(3,(3,3)),(4,(3,1)),
--                       (5,(3,2)),(6,(2,2)),(7,(1,1)),(8,(2,1))],
--          array (0,8) [(0,(2,2)),(1,(1,3)),(2,(2,3)),(3,(3,3)),(4,(3,1)),
--                       (5,(3,2)),(6,(1,2)),(7,(1,1)),(8,(2,1))],
--          array (0,8) [(0,(2,1)),(1,(1,3)),(2,(2,3)),(3,(3,3)),(4,(3,1)),
--                       (5,(3,2)),(6,(1,2)),(7,(1,1)),(8,(2,2))],
--          array (0,8) [(0,(3,1)),(1,(1,3)),(2,(2,3)),(3,(3,3)),(4,(2,1)),
--                       (5,(3,2)),(6,(1,2)),(7,(1,1)),(8,(2,2))],
--          array (0,8) [(0,(3,2)),(1,(1,3)),(2,(2,3)),(3,(3,3)),(4,(2,1)),
--                       (5,(3,1)),(6,(1,2)),(7,(1,1)),(8,(2,2))],
--          array (0,8) [(0,(2,2)),(1,(1,3)),(2,(2,3)),(3,(3,3)),(4,(2,1)),
--                       (5,(3,1)),(6,(1,2)),(7,(1,1)),(8,(3,2))],
--          array (0,8) [(0,(2,1)),(1,(1,3)),(2,(2,3)),(3,(3,3)),(4,(2,2)),
--                       (5,(3,1)),(6,(1,2)),(7,(1,1)),(8,(3,2))],
--          array (0,8) [(0,(3,1)),(1,(1,3)),(2,(2,3)),(3,(3,3)),(4,(2,2)),
--                       (5,(2,1)),(6,(1,2)),(7,(1,1)),(8,(3,2))],
--          array (0,8) [(0,(3,2)),(1,(1,3)),(2,(2,3)),(3,(3,3)),(4,(2,2)),
--                       (5,(2,1)),(6,(1,2)),(7,(1,1)),(8,(3,1))],
--          array (0,8) [(0,(2,2)),(1,(1,3)),(2,(2,3)),(3,(3,3)),(4,(3,2)),
--                       (5,(2,1)),(6,(1,2)),(7,(1,1)),(8,(3,1))],
--          array (0,8) [(0,(1,2)),(1,(1,3)),(2,(2,3)),(3,(3,3)),(4,(3,2)),
--                       (5,(2,1)),(6,(2,2)),(7,(1,1)),(8,(3,1))],
--          array (0,8) [(0,(1,3)),(1,(1,2)),(2,(2,3)),(3,(3,3)),(4,(3,2)),
--                       (5,(2,1)),(6,(2,2)),(7,(1,1)),(8,(3,1))],
--          array (0,8) [(0,(2,3)),(1,(1,2)),(2,(1,3)),(3,(3,3)),(4,(3,2)),
--                       (5,(2,1)),(6,(2,2)),(7,(1,1)),(8,(3,1))],
--          array (0,8) [(0,(2,2)),(1,(1,2)),(2,(1,3)),(3,(3,3)),(4,(3,2)),
--                       (5,(2,1)),(6,(2,3)),(7,(1,1)),(8,(3,1))],
--          array (0,8) [(0,(2,1)),(1,(1,2)),(2,(1,3)),(3,(3,3)),(4,(3,2)),
--                       (5,(2,2)),(6,(2,3)),(7,(1,1)),(8,(3,1))],
--          array (0,8) [(0,(1,1)),(1,(1,2)),(2,(1,3)),(3,(3,3)),(4,(3,2)),
--                       (5,(2,2)),(6,(2,3)),(7,(2,1)),(8,(3,1))],
--          array (0,8) [(0,(1,2)),(1,(1,1)),(2,(1,3)),(3,(3,3)),(4,(3,2)),
--                       (5,(2,2)),(6,(2,3)),(7,(2,1)),(8,(3,1))],
--          array (0,8) [(0,(2,2)),(1,(1,1)),(2,(1,3)),(3,(3,3)),(4,(3,2)),
--                       (5,(1,2)),(6,(2,3)),(7,(2,1)),(8,(3,1))]],
--     78)
buscaPM_8P = buscaPM sucesores8P      
                     esFinal8P        
                     (Est [inicial8P])

-- (nSolucionesPM_8P) es el número de soluciones del 8 puzzle por
-- búsqueda primero el mejor. Por ejemplo,
--    nSolucionesPM_8P  ==  43
nSolucionesPM_8P = length ls
    where (((Est ls),_):_) = buscaPM sucesores8P      
                                     esFinal8P        
                                     (Est [inicial8P])
