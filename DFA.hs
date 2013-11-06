module DFA ( -- * Constructores
             nuevoDFA
           , nuevoDFATabla
           -- * Proyecciones
           , estados
           , alfabeto
           , transicion
           , inicial
           , finales
           -- * Operaciones
           , esFinal
           , siguiente
           ) where

import Data.List (elemIndices)
import Data.Maybe (fromJust)

-- Un autómata finito determinístico está dado por una quintupla
-- 〈Q,Σ,δ,qₒ,F〉 donde Q es un conjunto de estados, Σ es un conjunto
-- de símbolos, δ : Q × Σ → Q es la función de transición, q₀ ∈ Q
-- es el estado inicial y F ⊆ Q es el conjunto de estados finales.
-- En nuestra representación los conjuntos están dados por listas,
-- el constructor (nuevoDFA) se encarga de verificar que q₀ sea un
-- elemento de Q, que F esté incluido en Q y que a partir de q₀
-- solamente puedan alcanzarse estados de Q.

data DFA q s = DFA [q]
                   [s]
                   ((q,s) -> q)
                   q
                   [q]
                   
-- Constructor de DFAs que verifica las condiciones sobre el
-- estado inicial, los intermedios y los finales. La función de
-- transición no necesariamente es total, pero el constructor
-- nuevoDFATabla si garantiza eso.
nuevoDFA :: (Eq q, Eq s) => [q] -> [s] -> ((q,s) -> q) -> q -> [q] -> DFA q s
nuevoDFA estados alfabeto delta inicial finales = 
                if condInicial && condFinal 
                then DFA estados alfabeto delta inicial finales
                else error "q0 no está en Q o F no es subconjunto de Q."                                                 
        where condInicial = inicial `elem` estados
              condFinal   = all (`elem` estados) finales

-- Construcción de DFA a partir de una tabla dada como una relación
-- ternaria; nos aseguramos que la relación sea funcional respecto a
-- los dos primeros elementos de la tripla y que cubra todo Q × Σ.
nuevoDFATabla :: (Eq q, Eq s) => [q] -> [s] -> [(q,s,q)] -> q -> [q] -> DFA q s
nuevoDFATabla estados alfabeto tabla inicial finales = 
                if condTransicion && condIntermedio
                then nuevoDFA estados alfabeto transicion inicial finales
                else error "La tabla no está completa o no es determinista o se sale de Q"
          where tabla' = map (\(q,s,q') -> ((q,s),q')) tabla
                condTransicion = all funcional [(q,s) | q <- estados, s <- alfabeto]
                transicion (q,s) = fromJust $ lookup (q,s) tabla'
                funcional = (==1) . length . (`elemIndices` (map fst tabla'))
                condIntermedio = all (`elem` estados) (map snd tabla')
                
-- Proyección de estados de un DFA.
estados :: DFA q s -> [q]
estados (DFA q _ _ _ _) = q

-- Proyección del alfabeto.
alfabeto :: DFA q s -> [s]
alfabeto (DFA _ s _ _ _) = s

-- Proyección de la función de transición.
transicion :: DFA q s -> (q,s) -> q
transicion (DFA _ _ delta _ _) = delta

-- Proyección del estado inicial.
inicial :: DFA q s -> q
inicial (DFA _ _ _ q _) = q

-- Proyección de los estados finales.
finales :: DFA q s -> [q]
finales (DFA _ _ _ _ f) = f

-- Dado el autómata A, es el estado q final?
esFinal :: Eq q => DFA q s -> q -> Bool
esFinal a q = q `elem` finales a

-- Dado un autómata A, un estado y un símbolo devuelve el
-- siguiente estado.
siguiente :: (Eq q, Eq s) => DFA q s -> q -> s -> q
siguiente a q s = transicion a (q,s)

-- Un ejemplo tomado del apunte.
ejemplo1 :: DFA Int Char
ejemplo1 = nuevoDFATabla [0,1,2] ['a','b'] [ (0,'a',1) , (0,'b',0)
                                           , (1,'a',1) , (1,'b',2)
                                           , (2,'a',2) , (2,'b',0)
                                           ]
                                           0
                                           [1,2]


-- Ejercicio: @acepta a s@ decide si el autómata @a@ acepta la cadena @s@.
acepta :: (Eq q, Eq s) => DFA q s -> [s] -> Bool
-- Este caso base sirve para averiguar si el estado inicial es un estado final.
acepta a [] = esFinal a (inicial a)
-- Obtenemos el últimos estado al que se llega consumiendo la cadena con la
-- función avanzarEstados y preguntamos si es final.
acepta a (x:xs) = esFinal a (avanzarEstados a (inicial a) (x:xs))

-- Ejercicio: @traza a s@ devuelve la lista de estados que se visitaron
-- mientras se consumió la cadena @s@. 
traza :: (Eq q, Eq s) => DFA q s -> [s] -> [q]
-- En caso que la cadena sea vacía. Solo me quedo con el estado inicial.
traza a [] = (inicial a):[]
-- En el caso recursivo solo se llama a la función avanzarYguardarEstados
-- con parámetros a (inicial a) y (x:xs), que se encarga de hacer todo el
-- trabajo. La descripción de esta función se detalla más abajo.
traza a (x:xs) = avanzarYguardarEstados a (inicial a) (x:xs)

----- Funciones auxiliares -----
-- Esta función toma como input un DFA, un estado, y una cadena.
-- El output es el estado último al que se llega cuando terminamos de consumir
-- la cadena. (Es como aplicar todo el tiempo la función 'siguiente' teniendo
-- noción todo el tiempo del estado en el que me encuentro).
avanzarEstados :: (Eq q, Eq s) => DFA q s -> q -> [s] -> q
-- Si ya no hay elementos en la cadena, entonces el último estado que se
-- visitó fue q.
avanzarEstados a q [] = q
-- Caso recursivo. Llamamos a la misma función pero con el estado q actulizado,
-- i.e., el estado actual va a ser el resultado de aplicar `siguiente (a,q,x)`
-- y con la cola de la cadena con la cual realizamos la llamada anterior
-- (tail (x:xs) = xs).
avanzarEstados a q (x:xs) = avanzarEstados a (siguiente a q x) xs

-- Esta función va a devolver la lista de estados que hayamos vistado al
-- consumir una cadena.
avanzarYguardarEstados :: (Eq q, Eq s) => DFA q s -> q -> [s] -> [q]
-- Si ya no quedan elementos en la cadena, entonces el último estado que visité
-- fue en el que estoy actualmente (i.e. q).
avanzarYguardarEstados a q [] = [q]
-- Caso recursivo. Guardo el estado actual en la lista y hago una llamada
-- recursiva con el siguiente estado y el tail de la cadena.
avanzarYguardarEstados a q (x:xs) = q : (avanzarYguardarEstados a (siguiente a q x) xs)
