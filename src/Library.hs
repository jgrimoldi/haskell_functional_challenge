module Library where
import PdePreludat hiding (foldr, sum, elem, all)
import Prelude (foldr, sum, elem, all, Foldable(..))

-- Desafio 1: Look and say, o https://es.wikipedia.org/wiki/Constante_de_Conway
--
-- Construir la siguiente sucesión:
-- 1
-- 11
-- 21
-- 1211
-- 111221
-- 312211
-- etc...
-- 

-- agruparElementosIguales :: Eq a => [a] -> [[a]]
-- agruparElementosIguales [] = []
-- agruparElementosIguales [x] = [[x]]
-- agruparElementosIguales (x:y:ys)
--     | x == y = [x,y] : agruparElementosIguales ys
--     | otherwise = [x] : [y] : agruparElementosIguales ys

-- agregaDigito :: Number -> Number -> Number
-- agregaDigito a b 
--  | a < 0     = a * 10 - b
--  | otherwise = a * 10 + b

-- foldl' :: (b -> a -> b) -> b -> [a] -> b
-- foldl' f z [] = z
-- foldl' f z (x : xs) = foldl' f (f z x) xs

length':: [a] -> Number
length' = sum . map (\x -> 1)

cuentaDigitos :: String -> String
cuentaDigitos string = show (length' string) ++ [head string]

esIgual :: Eq a => a -> a -> Bool
esIgual x y = x==y

agruparElementosIguales :: (Eq a) => [a] -> [[a]]
agruparElementosIguales []     = []
agruparElementosIguales (x:xs) = (x : takeWhile (esIgual x) xs) : agruparElementosIguales (dropWhile (esIgual x) xs)

auxiliar :: [Char] -> [Char]
auxiliar string = concatMap cuentaDigitos (agruparElementosIguales string)

--lookAndSay :: [Number]
lookAndSay =  iterate auxiliar "1"

-- Desafio 2: Un foldr para una flor
--
-- En computación, un "rose tree" es un árbol donde cada nodo puede tener una
-- cantidad ilimitada de nodos hijos.

data RoseTree a = RoseTree a [RoseTree a]

-- Entonces, si quisiesemos representar un arbol como este:
--            "hola"
--           /   |    \
--       "soy" "el" "arbol"
-- lo podríamos escribir como:

holaSoyElArbol :: RoseTree String
holaSoyElArbol = RoseTree "hola" [RoseTree "soy" [], RoseTree "el" [], RoseTree "arbol" []]

-- y para mostrar otro mas complicado,
--             5
--          / / \ \  \
--         2 3  1 7   9
--        / \   |   / | \
--      10  11  4  0  6  20 
-- lo podríamos escribir como:


otroArbol :: RoseTree Number
otroArbol = RoseTree 5 [RoseTree 2 [RoseTree 10 [],
                                    RoseTree 11 []],
                        RoseTree 3 [],
                        RoseTree 1 [RoseTree 4 []],
                        RoseTree 7 [],
                        RoseTree 9 [RoseTree 0 [],
                                    RoseTree 6 [],
                                    RoseTree 20 []]]

-- Algo que no vimos durante la cursada es que foldr en realidad es parte de una typeclass,
-- Foldable, o sea que no solo las listas se pueden foldear.
--
-- Como en el PdePreludat limitamos el tipo a que solo admita listas para hacerlo más simple,
-- en este ejercicio los imports están un poco tocados para traer el foldr que viene de haskell
-- en vez del del PdePreludat.
--
-- El desafío es hacer una implementación del foldr para esta estructura que haga pasar los tests.
--
-- Y algo interesante es que al implementar el foldr algunas funciones (como sum, all, any y elem)
-- las vamos a tener "gratis" para este tipo.

instance Foldable RoseTree where
  foldr funcion semilla (RoseTree valor []) = funcion valor semilla
  foldr funcion semilla roseTree = foldr funcion semilla (creaLista roseTree)

creaLista :: RoseTree a -> [a]
creaLista (RoseTree valor []) = [valor]
creaLista ( RoseTree valor lista ) = valor : concatMap creaLista lista

-- foldr' :: (a -> b -> b) -> b -> [a] -> b
-- foldr' _ semilla [] = semilla
-- foldr' funcion semilla (x:xs) = funcion x (foldr' funcion semilla xs)
