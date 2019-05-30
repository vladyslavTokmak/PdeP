import Data.List

-- Datas

data Pirata = UnPirata {
  nombrePirata :: String,
  tesoros :: [Tesoro]
} deriving Show

type Tesoro = (String,Int)
nombreTesoro = fst
valor = snd

data BarcoPirata = UnBarcoPirata {
    nombreBarco :: String,
    tripulacion :: [Pirata]
} deriving Show

jackSparrow = UnPirata { nombrePirata = "Jack Sparrow", tesoros = [("Brujula",10000),("Frasco de arena",0)] }
davidJones = UnPirata { nombrePirata = "David Jones" , tesoros = [("Cajita musical",1)] }
anneBonny = UnPirata { nombrePirata = "Anne Bonny" , tesoros = [("Doblones",100),("Frasco de arena",1)]}

perlaNegra = UnBarcoPirata { nombreBarco = "Perla Negra", tripulacion = [jackSparrow,anneBonny] }

-- La cantidad de tesoros de un pirata

cantTesorosDe :: Pirata -> Int
cantTesorosDe pirata = (length.tesoros) pirata

--Otra opcion que tambien funciona
cantTesorosDe' = (length.tesoros)

-- Si un pirata es afortunado, lo cual es cierto si el valor total de 
-- su botín supera los 10000

esAfortunado :: Pirata -> Bool
esAfortunado pirata = ((>10000).sum.todosLosValores.tesoros) pirata

todosLosValores :: [Tesoro] -> [Int]
todosLosValores tesoro = map valor tesoro

-- Si dos piratas tienen un mismo tesoro, pero de valor diferente

--tieneMismoTesoro pirata otroPirata = any (estePresenteEn (tesoros otroPirata)) (tesoros pirata)

--estePresenteEn   

seaElMismoQue (nombre,valor) (otroNombre,otroValor) = nombre == otroNombre && valor /= otroValor

--tienenMismoTesoro items1 items2 = any (estePresenteEn items2) items1
--estePresenteEn items2 item = any (seaElMismoQue item) items2
--seaElMismoQue (nombre, valor) (nombre2, valor2) =  

-- El valor del tesoro más valioso de un pirata

valorTesoroMasValioso :: Pirata -> Int
valorTesoroMasValioso pirata = maximum ((todosLosValores.tesoros) pirata)

-- Como queda el pirata luego de adquirir un nuevo tesoro

adquiereTesoro :: Pirata -> Tesoro -> Pirata
adquiereTesoro pirata tesoro = pirata {tesoros = tesoro : tesoros pirata}

-- Como queda el pirata luego de perder todos los tesoros valiosos, 
-- que son los que tienen un valor mayor a 100.

--pierdeTesorosValiosos

-- Un pirata se incorpora a la tripulación de un barco

unirPirataABarco :: Pirata -> BarcoPirata -> BarcoPirata
unirPirataABarco pirata barco = barco{ tripulacion = pirata : tripulacion barco}

-- Un pirata abandona la tripulación de un barco

--abandonaBarco 