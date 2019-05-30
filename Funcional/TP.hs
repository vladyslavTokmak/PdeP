module TP where
import Text.Show.Functions
import Data.List

-------------------------------------
------------ ENTREGA 1 --------------
-------------------------------------

-- PUNTO 3.1.1
data Microprocesador = UnMicro {
    acumA :: Int,
    acumB :: Int,
    memoriaDatos :: [Int],
    progCounter :: Int,
    etiquetaError :: String, 
    programa :: [Microprocesador -> Microprocesador]} deriving Show

-----------------
-- PUNTO 3.1.2 --
-----------------

xt8088 = UnMicro {acumA = 0 , 
                  acumB = 0 , 
                  memoriaDatos = take 1024 (cycle [0]),
                  progCounter = 0, 
                  etiquetaError = "",
                  programa = programaVacio }

-----------------				  
-- PUNTO 3.2.1 --
-----------------

nop :: Microprocesador -> Microprocesador
nop micro = micro { progCounter = aumentarPC micro}

aumentarPC :: Microprocesador->Int
aumentarPC micro = (progCounter micro) + 1

aumentarContador = nop

-----------------
-- PUNTO 3.2.2 --
-----------------

tresNop :: Microprocesador->Microprocesador
tresNop = (nop.nop.nop)

-- En el caso de que se ingrese por consola directamente, seria de la siguiente manera:
-- TP> (nop.nop.nop) xt8088
-- UnMicro {acumA = 0, acumB = 0, memoriaDatos = [], progCounter = 3, etiquetaError = ""}

-- El concepto que interviene es el de composicion de funciones, el cual nos permite aplicar tres veces NOP
-- al microprocesador, ya que si simplemente ingresamos NOP por consola y le damos a Enter tres veces, 
-- el contador queda en 1 

-----------------
-- PUNTO 3.3.1 --
-----------------

lodv :: Int->Microprocesador->Microprocesador
lodv val micro = micro {acumA = val}

swap :: Microprocesador->Microprocesador
swap micro = micro {acumA = acumB micro , acumB = acumA micro}

sumarAcumuladores :: Microprocesador->Int
sumarAcumuladores micro  = (acumA micro) + (acumB micro)

add :: Microprocesador->Microprocesador
add micro = micro {acumA = sumarAcumuladores micro , acumB = 0 }

-----------------
-- PUNTO 3.3.2 --
-----------------

sumarDosNumeros :: Int->Int->Microprocesador->Microprocesador
sumarDosNumeros num1 num2 micro = (add .(lodv num2) . (swap . lodv num1)) micro

-----------------
-- PUNTO 3.4.1 --
-----------------

str :: Int->Int->Microprocesador->Microprocesador
str addr val micro = micro {memoriaDatos = colocarEnLista addr val (memoriaDatos micro)}

colocarEnLista :: Int->Int->[Int]->[Int]
colocarEnLista addr val lista = (take (addr-1) lista) ++ [val] ++ (drop (addr+1) lista)

divisionAcumuladores :: Microprocesador->Int
divisionAcumuladores micro = div (acumA micro) (acumB micro)

divide :: Microprocesador -> Microprocesador
divide micro |acumB micro == 0 = micro {etiquetaError = "DIVISION BY ZERO" }
             |otherwise = micro {acumA = div (acumA micro) (acumB micro) , acumB = 0}


indiceMemoria :: Int->Microprocesador->Int
indiceMemoria posicion micro = (!!) (memoriaDatos micro) (posicion -1)

lod :: Int->Microprocesador->Microprocesador
lod posicion micro = micro {acumA = indiceMemoria posicion micro}

-----------------
-- PUNTO 3.4.2 --
-----------------

dividir :: Int -> Int -> Microprocesador->Microprocesador
dividir num1 num2 micro = (divide. lod 1.swap.lod 2.str 2 num2.str 1 num1) micro

---------------
-- PUNTO 5.1 --
---------------

at8086 = UnMicro {acumA = 0,
                  acumB = 0,
                  memoriaDatos = [1..20],
                  progCounter = 0,
                  etiquetaError = "",
                  programa = programaVacio }

fp20 = UnMicro {acumA = 7,
                acumB = 24,
                memoriaDatos = [],
                progCounter = 0,
                etiquetaError = "",
                programa = programaVacio }

programCounter :: Microprocesador->Int
programCounter = progCounter 

acumuladorA :: Microprocesador->Int
acumuladorA = acumA

acumuladorB :: Microprocesador->Int
acumuladorB = acumB

memoria :: Microprocesador->[Int]
memoria = memoriaDatos

mensajeError :: Microprocesador->String
mensajeError = etiquetaError

-------------------------------------
------------ ENTREGA 2 --------------
-------------------------------------

---------------
-- PUNTO 3.1 --
---------------

sumar10y22 :: [Microprocesador -> Microprocesador]
sumar10y22 = [lodv 10,swap,lodv 22,add]

dividir2x0 :: [Microprocesador -> Microprocesador]
dividir2x0 = [str 1 2,str 2 0,lod 2,swap,lod 1,divide]

cargarPrograma :: [Microprocesador -> Microprocesador] -> Microprocesador -> Microprocesador
cargarPrograma prog micro = micro {programa = prog}

---------------
-- PUNTO 3.2 --
--------------- 

ejecutarInstruccion ::  Microprocesador -> (Microprocesador -> Microprocesador) -> Microprocesador
ejecutarInstruccion micro instruccion | etiquetaError micro == "" = (aumentarContador.instruccion) micro
                           | otherwise = micro

ejecutarPrograma :: Microprocesador -> Microprocesador
ejecutarPrograma micro = aplicarInststrucciones micro ejecutarInstruccion

---------------
-- PUNTO 3.3 --
---------------

ifnz :: Microprocesador -> Microprocesador
ifnz micro = aplicarInststrucciones micro acumAes0

acumAes0 :: Microprocesador -> (Microprocesador -> Microprocesador) -> Microprocesador
acumAes0 micro prog |acumA micro == 0 = micro
                    |otherwise = ejecutarInstruccion micro prog

programaifnz :: [Microprocesador -> Microprocesador]
programaifnz = [lodv 3, swap]

---------------
-- PUNTO 3.4 --
---------------

programaADepurar = [swap, nop, lodv 133, lodv 0, str 1 3, str 2 0]

depurarPrograma :: [Microprocesador -> Microprocesador] -> [Microprocesador -> Microprocesador]
depurarPrograma prog = filter (estanMemDatosYAcumEn0 xt8088) prog

estanMemDatosYAcumEn0 :: Microprocesador -> (Microprocesador -> Microprocesador) -> Bool
estanMemDatosYAcumEn0 micro instruccion = not ((estaMemDatosEn0.instruccion) micro && (estanAcumEn0.instruccion) micro)

estanAcumEn0 :: Microprocesador -> Bool
estanAcumEn0 micro = (acumA micro) == 0 && (acumB micro) == 0

estaMemDatosEn0 :: Microprocesador -> Bool
estaMemDatosEn0 micro = all (== 0) (memoriaDatos micro)

---------------
-- PUNTO 3.5 --
---------------

microDesorden :: Microprocesador
microDesorden = UnMicro {acumA = 0,
                         acumB = 0,
                         memoriaDatos = [2,5,1,0,6,9],
                         progCounter = 0,
                         etiquetaError = "",
                         programa = programaVacio }

{-
listaOrdenada :: Microprocesador -> Bool
listaOrdenada micro = memoriaDatos micro == sort(memoriaDatos micro) 
-}

esMemoriaOrdenada :: Microprocesador -> Bool
esMemoriaOrdenada micro = esListaOrdenada (memoriaDatos micro)

esListaOrdenada :: [Int] -> Bool
esListaOrdenada [x] = True
esListaOrdenada (x:(y:xs)) = x <= y && esListaOrdenada (y:xs)

-----------------
-- PUNTO 3.6.1 --
-----------------

memoriaInfinita = UnMicro {acumA = 0,
                           acumB = 0,
                           memoriaDatos = cycle [0],
                           progCounter = 0,
                           etiquetaError = "",
                           programa = programaVacio }

-----------------
-- PUNTO 3.6.2 --
-----------------

{-
	Lo que sucede es que el programa de 10 + 22 nos devuelve el procesador. 
	Como nos devuelve el procesador, eso imnplica que nos mostrara todo su contenido,
	por lo tanto cuando intente mostrar la memoria nunca terminara, y nosotros tendremos
	que frenar el programa para que se detenga la ejecucion del mismo.
-}

-----------------
-- PUNTO 3.6.3 --
-----------------

{-
	El programa se cuelga, ya que aplica la Eager Evaluation, porque en nuestro caso
	nuestra funcion para saber si la lista ordenada tiene la siguiente definicion:
	True && OtraCosa = OtraCosa , por lo tanto se quedara infinitamente comprando todos
	los elementos de la lista.
-}

--------------
-- AUXILIAR --
--------------

programaVacio :: [Microprocesador -> Microprocesador]
programaVacio = []

aplicarInststrucciones :: Microprocesador -> (Microprocesador -> (Microprocesador -> Microprocesador) -> Microprocesador) -> Microprocesador
aplicarInststrucciones micro fun = foldl (fun) micro (programa micro)

