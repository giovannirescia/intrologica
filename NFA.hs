module NFA where

import Data.Maybe (maybe)
import Data.List (delete,(\\),union)

-- Un autómata finito no-determinístico está dado por una quintupla
-- 〈Q,Σ,δ,qₒ,F〉 donde Q es un conjunto de estados, Σ es un conjunto
-- de símbolos, δ : Q × Σ ∪ {ε} → P(Q) es la función de transición, q₀ ∈ Q
-- es el estado inicial y F ⊆ Q es el conjunto de estados finales.
-- En nuestra representación los conjuntos están dados por listas,
-- el constructor (nuevoDFA) se encarga de verificar que q₀ sea un
-- elemento de Q y que F esté incluido en Q.

data NFA q s = NFA [q]
                   [s]
                   ((q,Maybe s) -> [q])
                   q
                   [q]
            
nuevoNFA :: (Eq q) => [q] -> [s] -> ((q,Maybe s) -> [q]) -> q -> [q] -> NFA q s 
nuevoNFA estados alfabeto delta inicial finales = if condInicial && condFinal 
                                                  then NFA estados alfabeto delta inicial finales
                                                  else error "q0 no está en Q o F no es subconjunto de Q."
  where condInicial = inicial `elem` estados
        condFinal   = all (`elem` estados) finales



-- Construcción de NFA a partir de una tabla dada como una relación
-- ternaria; nos aseguramos que la relación sea funcional respecto a
-- los dos primeros elementos de la tripla y que cubra todo Q × Σ.
nuevoNFATabla :: (Eq q, Eq s) => [q] -> [s] -> [(q,Maybe s,[q])] -> q -> [q] -> NFA q s
nuevoNFATabla estados alfabeto tabla inicial finales = 
                if condTransicion
                then nuevoNFA estados alfabeto transicion inicial finales
                else error "La tabla no está completa o no es determinista o se sale de Q"
          where tabla' = map (\(q,s,q') -> ((q,s),q')) tabla
                condTransicion = and $ map enConjuntos tabla
                transicion (q,s) = maybe [] id $ lookup (q,s) tabla'
                condEstados q q' = all (`elem` estados) q' && q `elem` estados
                enConjuntos (q,Nothing,q') = condEstados q q'
                enConjuntos (q,Just s,q')  = condEstados q q' && s `elem` alfabeto


-- Proyección de estados de un DFA.
estados :: NFA q s -> [q]
estados (NFA qs _ _ _ _) = qs

-- Proyección del alfabeto.
alfabeto :: NFA q s -> [s]
alfabeto (NFA _ s _ _ _) = s

    
-- Proyección del estado inicial. 
inicial :: NFA q s -> q
inicial (NFA _ _ _ q0 _) = q0

-- Decide si el estado @q@ es final.
esFinal :: Eq q => NFA q s -> q -> Bool
esFinal (NFA _ _ _ _ f) q = q `elem` f

-- Devuelve la función de transición del autómata.
delta :: NFA q s -> ((q,Maybe s)) -> [q]
delta (NFA _ _ d _ _) = d

-- El conjunto de estados a los que accedemos a través de 
-- una transición ε. Notar que el mismo estado no está en la
-- lista resultado si no hay una transición ε explícita.
etrans :: Eq q => NFA q s -> q -> [q]
etrans a q = delta a (q,Nothing)

-- Clausura transitiva, no-reflexiva de transiciones ε. La
-- lista que le pasamos como argumento permite evitar ciclos.
clausura :: Eq q => NFA q s -> [q] -> q -> [q]
clausura a qs q = let qs' = etrans a q \\ qs
                      qs'' = concatMap (clausura a (q:qs)) qs'
                    in q `delete` (qs' `union` qs'')

-- Transiciones desde un estado con un símbolo dado, incluye las
-- que pueden hacerse desde los estados que se alcanzan desde el 
-- estado dado.
ptrans :: (Eq q,Eq s) => NFA q s -> s -> q -> [q]
ptrans a c q = delta a (q, Just c) ++ concatMap consumeSim (clausura a [] q)
  where consumeSim q' = delta a (q',Just c)

-- Ejercicio: Generalización de consumir: mientras que la primera asume el estado
-- inicial, en esta función tratamos de consumir la cadena desde cualquier
-- estado.
recorreDesde :: (Eq q,Eq s) => NFA q s -> [s] -> q -> [q]
recorreDesde a cs q = --completar

-- Ejercicio: Decide si un autómata no determinístico reconoce o no una palabra.
reconoce :: (Eq q,Eq s) => NFA q s -> [s] -> Bool
reconoce a s = -- completar
