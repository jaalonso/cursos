-- BusquedaEnEspaciosDeEstados.hs
-- Búsqueda en espacios de estados.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 21 de Noviembre de 2010
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Importaciones                                                      --
-- ---------------------------------------------------------------------

-- Nota: Hay que elegir una implementación de las pilas.
import PilaConListas
-- import PilaConTipoDeDatoAlgebraico

import Data.Array
import Data.List (sort)

-- ---------------------------------------------------------------------
-- Descripción de los problemas de espacios de estados                --
-- ---------------------------------------------------------------------

-- Las características de los problemas de espacios de estados son:
-- * un conjunto de las posibles situaciones o nodos que constituye
--   el espacio de estados; estos son las potenciales soluciones que se
--   necesitan explorar;
-- * un conjunto de movimientos de un nodo a otros nodos, llamados los
--   sucesores del nodo; 
-- * un nodo inicial;
-- * un nodo objetivo, que es la solución.

-- ---------------------------------------------------------------------
-- El problema del 8 puzzle                                           --
-- ---------------------------------------------------------------------

-- Para el 8-puzzle se usa un cajón cuadrado en el que hay situados 8 bloques
-- cuadrados.  El cuadrado restante está sin rellenar. Cada bloque tiene un
-- número. Un bloque adyacente al hueco puede deslizarse hacia él. El juego
-- consiste en transformar la posición inicial en la posición final mediante
-- el deslizamiento de los bloques.  En particular, consideramos el estado
-- inicial y final siguientes:
--
--      +---+---+---+                   +---+---+---+
--      | 2 | 6 | 3 |                   | 1 | 2 | 3 | 
--      +---+---+---+                   +---+---+---+ 
--      | 5 |   | 4 |                   | 8 |   | 4 | 
--      +---+---+---+                   +---+---+---+ 
--      | 1 | 7 | 8 |                   | 7 | 6 | 5 | 
--      +---+---+---+                   +---+---+---+ 
--                      
--      Estado inicial                  Estado final

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
--    ghci> inicial8P
--    array (0,8) [(0,(2,2)),(1,(1,1)),(2,(1,3)),(3,(3,3)),(4,(3,2)),
--                 (5,(1,2)),(6,(2,3)),(7,(2,1)),(8,(3,1))]
--    ghci> todosMovimientos inicial8P
--    [array (0,8) [(0,(3,2)),(1,(1,1)),(2,(1,3)),(3,(3,3)),(4,(2,2)),
--                  (5,(1,2)),(6,(2,3)),(7,(2,1)),(8,(3,1))],
--     array (0,8) [(0,(1,2)),(1,(1,1)),(2,(1,3)),(3,(3,3)),(4,(3,2)),
--                  (5,(2,2)),(6,(2,3)),(7,(2,1)),(8,(3,1))],
--     array (0,8) [(0,(2,3)),(1,(1,1)),(2,(1,3)),(3,(3,3)),(4,(3,2)),
--                  (5,(1,2)),(6,(2,2)),(7,(2,1)),(8,(3,1))],
--     array (0,8) [(0,(2,1)),(1,(1,1)),(2,(1,3)),(3,(3,3)),(4,(3,2)),
--                  (5,(1,2)),(6,(2,3)),(7,(2,2)),(8,(3,1))]]
todosMovimientos :: Tablero -> [Tablero]
todosMovimientos t = 
    [t//[(0,t!i),(i,t!0)] | i<-[1..8], adyacente (t!0) (t!i)] 

-- Los nodos del espacio de estados son listas de tableros [t_n,...,t_1]
-- tal que t_i es un sucesor de t_(i-1).
data Tableros = Est [Tablero] deriving (Eq, Show)

-- (sucesores8P e) es la lista de sucesores del estado e. Por ejemplo,
--    ghci> sucesores8P (Est [inicial8P])
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

-- ---------------------------------------------------------------------
-- El patrón de búsqueda en espacios de estados                       --
-- ---------------------------------------------------------------------

-- Nota: Se supone que el grafo implícito de espacios de estados es
-- acíclico. 

-- (buscaEE s o e) es la lista de soluciones del problema de espacio de
-- estado definido por la función sucesores (s), el objetivo (o) y el
-- estado inicial (e).
buscaEE:: (Eq node) => (node -> [node])    -- sucesores
                       -> (node -> Bool)   -- esFinal
                       -> node             -- nodo actual
                       -> [node]           -- soluciones
buscaEE sucesores esFinal x = busca' (apila x vacia) 
 where
   busca' p  
    | esVacia p        = [] 
    | esFinal (cima p) = cima p : busca' (desapila p)
    | otherwise        = busca' (foldr apila (desapila p) (sucesores x))
                         where x = cima p

-- ---------------------------------------------------------------------
-- Solución del 8 puzzle por búsqueda en espacios de estados          --
-- ---------------------------------------------------------------------

-- (esFinal8P e) se verifica si e es un estado final del 8 puzzle.
esFinal8P :: Tableros -> Bool
esFinal8P (Est (n:_)) = elems n == elems final8P

-- (buscaEE8P) es la lista de las soluciones del problema del 8
-- puzzle. Por ejemplo,
--    ghci> buscaEE8P
--      C-c C-cInterrupted.
-- No termina.
buscaEE8P :: [[Posicion]]
buscaEE8P = map elems ls
    where ((Est ls):_) = buscaEE sucesores8P esFinal8P (Est [inicial8P])

-- ---------------------------------------------------------------------
-- El problema de las n reinas                                        --
-- ---------------------------------------------------------------------

-- El problema de las n reinas consiste en colocar n reinas en un
-- tablero cuadrado de dimensiones n por n de forma que no se encuentren
-- más de una en la misma línea: horizontal, vertical o diagonal.

-- Las posiciones de las reinas en el tablero se representan por su
-- columna y su fila.
type Columna = Int
type Fila    = Int

-- Una solución del problema de las n reinas es una lista de
-- posiciones. 
type SolNR = [(Columna,Fila)]

-- (valida sp p) se verifica si la posición p es válida respecto de la
-- solución parcial sp; es decir, la reina en la posición p no amenaza a
-- ninguna de las reinas de la sp (se supone que están en distintas
-- columnas). Por ejemplo, 
--    valida [(1,1)] (2,2)  ==  False
--    valida [(1,1)] (2,3)  ==  True
valida :: SolNR -> (Columna,Fila) -> Bool
valida solp (c,r) = and [test s | s <- solp]
    where test (c',r') = and [c'+r'/=c+r,c'-r'/=c-r,r'/=r]

-- Los nodos del problema de las n reinas son ternas formadas por la
-- columna de la última reina colocada, el número de columnas del
-- tablero y la solución parcial de las reinas colocadas anteriormente. 
type NodoNR = (Columna,Columna,SolNR)

-- (sucesoresNR e) es la lista de los sucesores del estado e en el
-- problema de las n reinas. Por ejemplo,
--    ghci> sucesoresNR (1,4,[])
--    [(2,4,[(1,1)]),(2,4,[(1,2)]),(2,4,[(1,3)]),(2,4,[(1,4)])]
sucesoresNR :: NodoNR -> [NodoNR]
sucesoresNR (c,n,solp)
    = [(c+1,n,solp++[(c,r)]) | r <- [1..n] , valida solp (c,r)]

-- (esFinalNQ e) se verifica si e es un estado final del problema de las
-- n reinas. 
esFinalNQ :: NodoNR -> Bool
esFinalNQ (c,n,solp) = c > n

-- (buscaEE_NQ n) es la primera solución del problema de las n reinas,
-- por búsqueda en espacio de estados. Por ejemplo,
--    ghci> buscaEE_NQ 8
--    [(1,1),(2,5),(3,8),(4,6),(5,3),(6,7),(7,2),(8,4)]
buscaEE_NQ :: Columna -> SolNR
buscaEE_NQ n = s
    where ((_,_,s):_) = buscaEE sucesoresNR esFinalNQ (1,n,[])

-- (nSolucionesNQ n) es el número de soluciones del problema de las n
-- reinas, por búsqueda en espacio de estados. Por ejemplo,  
--    nSolucionesNQ 8  ==  92
nSolucionesNQ :: Columna -> Int
nSolucionesNQ n = 
    length (buscaEE sucesoresNR 
                    esFinalNQ 
                    (1,n,[]))

-- ---------------------------------------------------------------------
-- Problema de la mochila                                             --
-- ---------------------------------------------------------------------

-- Se tiene una mochila de capacidad de peso p y una lista de n objetos
-- para colocar en la mochila. Cada objeto i tiene un peso w_i y un
-- valor v_i. Considerando la posibilidad de colocar el mismo objeto
-- varias veces en la mochila, el problema consiste en determinar la
-- forma de colocar los objetos en la mochila sin sobrepasar la
-- capacidad de la mochila colocando el máximmo valor posible.

-- Los pesos son número enteros
type Peso = Int

-- Los valores son números reales.
type Valor = Float

-- Los objetos son pares formado por un peso y un valor
type Objeto = (Peso,Valor)

-- Una solución del problema de la mochila es una lista de objetos.
type SolMoch = [Objeto]

-- Los estados del problema de la mochila son 5-tupla de la forma
-- (v,p,l,o,s) donde v es el valor de los objetos colocados, p es el
-- peso de los objetos colocados, l es el límite de la capacidad de la
-- mochila, o es la lista de los objetos colocados (ordenados de forma
-- creciente según sus pesos) y s es la solución parcial.
type NodoMoch = (Valor,Peso,Peso,[Objeto],SolMoch)


-- (sucesoresMoch e) es la lista de los sucesores del estado e en el
-- problema de la mochila.
sucesoresMoch :: NodoMoch -> [NodoMoch]
sucesoresMoch (v,p,limite,objetos,solp)
    = [( v+v',
         p+p',
         limite,
         [o | o@(p'',_) <- objetos,(p''>=p')], 
         (p',v'):solp )
       | (p',v') <- objetos, 
         p+p' <= limite]

-- (esObjetivoMoch e) se verifica si e es un estado final el problema de
-- la mochila.
esObjetivoMoch :: NodoMoch -> Bool
esObjetivoMoch (_,p,limite,((p',_):_),_) = p+p'>limite

-- (buscaEE_Mochila os l) es la solución del problema de la mochila para
-- la lista de objetos os y el límite de capacidad l. Por ejemplo,
--    > buscaEE_Mochila [(2,3),(3,5),(4,6),(5,10)] 8
--    ([(5,10.0),(3,5.0)],15.0)
--    > buscaEE_Mochila [(2,3),(3,5),(5,6)] 10
--    ([(3,5.0),(3,5.0),(2,3.0),(2,3.0)],16.0)
--    > buscaEE_Mochila [(8,15),(15,10),(3,6),(6,13),(2,4),(4,8),(5,6),(7,7)] 35
--    ([(6,13.0),(6,13.0),(6,13.0),(6,13.0),(6,13.0),(3,6.0),(2,4.0)],75.0)
--    > buscaEE_Mochila [(2,2.8),(3,4.4),(5,6.1)] 10
--    ([(3,4.4),(3,4.4),(2,2.8),(2,2.8)],14.4)
buscaEE_Mochila :: [Objeto] -> Peso -> (SolMoch,Valor)
buscaEE_Mochila objetos limite = (sol,v) 
    where 
      (v,_,_,_,sol) = 
          maximum (buscaEE sucesoresMoch 
                           esObjetivoMoch  
                           (0,0,limite,sort objetos,[]))

mochila objetos limite = 
    [(sol,v) | (v,w,_,_,sol) <- buscaEE sucesoresMoch 
                                        esObjetivoMoch  
                                        (0,0,limite,sort objetos,[]),
                                        ((w==10)||(w==9)||(w==8)) ]

