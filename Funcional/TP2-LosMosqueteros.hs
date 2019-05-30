import Data.List
import Text.Show.Functions

-- PUNTO 1 TP2

data Animal = UnAnimal {
    ci :: Int,
    especie :: String,
    capacidades :: [String]
} deriving Show

-- PUNTO 2

inteligenciaSuperior :: Int -> Animal -> Animal
inteligenciaSuperior incremento animal = animal { ci = (ci animal) + incremento}

pinkificar :: Animal -> Animal
pinkificar animal = animal { capacidades = [] }

superpoderes :: Animal -> Animal
superpoderes animal | esDeEspecie "elefante" animal = agregarCapacidad "no tenerle miedo a los ratones" animal
                    | esDeEspecie "raton" animal && (ci animal) > 100 = agregarCapacidad "hablar" animal
                    | otherwise = animal

esDeEspecie :: String -> Animal -> Bool
esDeEspecie unaEspecie animal = unaEspecie == (especie animal)

agregarCapacidad :: String -> Animal -> Animal
agregarCapacidad capacidad animal = animal { capacidades = capacidad : (capacidades animal)}

-- PUNTO 3

antropomorfico :: Animal -> Bool
antropomorfico animal = tieneCapacidad "hablar" animal && (ci animal) > 60

tieneCapacidad :: String -> Animal -> Bool
tieneCapacidad capacidad animal = elem capacidad (capacidades animal)

noTanCuerdo :: Animal -> Bool
noTanCuerdo animal = (tieneMasDe.capacidadesPinkiescas) animal

noTanCuerdo' :: Animal -> Bool
noTanCuerdo' = (tieneMasDe.capacidadesPinkiescas)

capacidadesPinkiescas :: Animal -> [String]
capacidadesPinkiescas animal = filter (pinkiesco) (capacidades animal)

tieneMasDe :: [String] -> Bool
tieneMasDe capacidades = (length capacidades) > 2

pinkiesco :: String -> Bool
pinkiesco capacidad = comienzaConHacer capacidad && terminaCon4oMenosLetras capacidad && tieneUnaVocal capacidad  

--coreguir nombre
comienzaConHacer :: String -> Bool
comienzaConHacer capacidad = (divideirCapacidad "hacer" capacidad) == "hacer"

divideirCapacidad :: String -> String -> String
divideirCapacidad palabra capacidad = take (length palabra) capacidad

terminaCon4oMenosLetras :: String -> Bool
terminaCon4oMenosLetras capacidad = length capacidad <= 10 && length capacidad >= 7

--cambiar tiene vocal y invertitirYTomar4Primeras talvez ya no sirva
tieneUnaVocal :: String -> Bool
tieneUnaVocal capacidad | any (== 'a') (drop 6 capacidad) = True
                        | any (== 'e') (drop 6 capacidad) = True
                        | any (== 'i') (drop 6 capacidad) = True
                        | any (== 'o') (drop 6 capacidad) = True
                        | any (== 'u') (drop 6 capacidad) = True
                        | otherwise = False

--tieneUnaVocal lista = any esVocal lista				

--cambiar o eliminar
invertitirYTomar4Primeras :: String -> String
invertitirYTomar4Primeras capacidad | length capacidad == 10 = take 4 (reverse capacidad)
                                    | length capacidad == 9 = take 3 (reverse capacidad)
                                    | length capacidad == 8 = take 2 (reverse capacidad)
                                    | length capacidad == 7 = take 1 (reverse capacidad)
                                    | otherwise = take 1 (reverse capacidad)


--PUNTO 4

data Experimento = UnExperimento{
    transformaciones :: [Animal -> Animal],
    criterioDeExito :: Animal -> Bool
} deriving Show

experimentoExitoso :: Animal -> Experimento -> Bool
experimentoExitoso animal experimento = (criterioDeExito experimento) (aplicarTransformaciones experimento animal)

aplicarTransformaciones :: Experimento -> Animal -> Animal
aplicarTransformaciones experimento animal = foldl (flip($)) animal (transformaciones experimento)

--PUNTO 5

--Filtra a los animales si tienen almenos una de las capacidades
informe1 animales listaCapacidades = filter (aux listaCapacidades) animales

aux listaCapacidades animales = any (flip elem (listaCapacidades)) (capacidades animales)

--aplicar experimento a cada animal

cambiarAnimales animales experimento = map (aplicarTransformaciones experimento) animales



--PUNTO 6

-- Si se podrian realizar experimentos sobre un animal con infinitas capacidades,
-- ya que las transformaciones que se le realizan al animal nunca usan la lista
-- de capacidedes del animal, sino que la añaden pequeños detalles.
-- En donde si habria problemas de los experimentos seria en el criterio de exito,
-- ya que en ellas es donde podria colgarse, aunque en la funcion de "antropomorfico"
-- se puede decir que por "lazy evaluation" si llegase a encotrar la capacidad "hablar"
-- automaticamente devolveria un True, aun cuando queden infinitas capacidades por evaluar,
-- pero en el caso de la funcion "noTanCuerdo", la funcion se colgaria permanentemente,
-- porque la funcion debe evaluar cada una de las capacidades (en nuestro codigo lo hace),
-- y como son infinitas nunca las terminaria de evaluar todas.

-- Aclaracion: realmente toda funcion que decimos que se cuelga por pasarle una lista infinita,
-- en algun momento terminaran de hacer lo que tenian que hacer. Esto se debe a que la lista 
-- infinita realmente es finita y terminaria una vez que se nos acabara la memoria.

--PUNTO 7

-- ANIMALES

dumbo = UnAnimal { ci = 100 , especie = "elefante" , capacidades = ["volar","divertir a la gente"]}

mickey = UnAnimal { ci = 200 , especie = "raton" , capacidades = ["hablar","bailar","cantar","divertir a la gente"]}

jerry = UnAnimal {ci = 150, especie = "raton", capacidades = ["hacer awrt", "hacer maiz", "hacer sopa"]}

animalTodoPoderoso = UnAnimal { ci = 100000 , especie = "desconosida" , capacidades = repeat ("capacidad desconocida") }

listaAnimales = [dumbo, mickey, jerry]

--EXPERIMENTOS

experimento1 = UnExperimento { transformaciones = [pinkificar,inteligenciaSuperior 10,superpoderes], criterioDeExito = antropomorfico }

experimento2 = UnExperimento { transformaciones = [inteligenciaSuperior 34,superpoderes], criterioDeExito = noTanCuerdo }

listaExperimentos = [experimento1,experimento2]