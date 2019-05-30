import Data.List

data Arma = 
 Baculo Float String |
 Arco Float Float Float |
 Espada Float String deriving Show

type Parte = (Float, Int)
defensa = fst
durabilidad = snd
 
data Personaje = UnPersonaje {
 vida:: Float,
 armadura:: [Parte],
 arma:: Arma} deriving Show

type Buff = Personaje -> Personaje

data Clan = UnClan {
 miembros::[Personaje],
 buffs :: [Buff]}

poderDefensa:: Personaje -> Float 
poderDefensa personaje = vida personaje + (sum.map defensa.filter sano.armadura) personaje

sano::Parte -> Bool
sano parte = durabilidad parte > 0

------- PARTE 2 ---------

poderAtaque :: Personaje -> Float
poderAtaque personaje = poder (arma personaje)

poder:: Arma -> Float
poder (Baculo inteligencia nombre) = inteligencia + genericLength nombre
poder (Arco rango base hilo) = rango * hilo + base
poder (Espada almas material) = almas * coeficiente material

coeficiente:: String -> Float
coeficiente "madera" = 2
coeficiente "metal" = 3
coeficiente _ = 1


---------- PARTE 3 --------------

alterarArmadura:: (Float->Float) -> Personaje -> Personaje
alterarArmadura f personaje = personaje{armadura = map (\(defensa, durabilidad) -> (f defensa, durabilidad)) (armadura personaje)}

frenesi::Buff
frenesi personaje = alterarArmadura (*1.2) personaje

mantoEtereo::Buff
mantoEtereo personaje = alterarArmadura (+3) personaje{vida = vida personaje - 100}

berserker::Buff
berserker personaje = mejoraArma (alterarArmadura (\x -> 2) personaje )

mejoraArma::Personaje ->Personaje
mejoraArma (UnPersonaje v a (Espada x "madera")) =
            UnPersonaje v a (Espada x "metal")
mejoraArma pers = pers

espejoKarma:: Buff -> Buff
espejoKarma buff = buff.buff

sucesionBuffsInesperados:: [Buff] -> Buff
sucesionBuffsInesperados buffs personaje = foldl potenciar personaje buffs
--sucesionBuffsInesperados buffs personaje = foldr ($) personaje buffs

-- 1)

potenciar::Personaje->Buff->Personaje
potenciar personaje buff = buff personaje
--potenciar = flip ($)

-- 2)
esInofensivo::Buff->[Personaje]->Bool
esInofensivo buff personajes = all (inofensivoPara buff) personajes

inofensivoPara::Buff->Personaje->Bool
inofensivoPara buff personaje = 
 poderAtaque personaje == poderAtaque (potenciar personaje buff) &&
 poderDefensa personaje == poderDefensa (potenciar personaje buff)
 
--inofensivoPara buff personaje = 
-- poderAtaque personaje == poderAtaque (buff personaje) &&
-- poderDefensa personaje == poderDefensa (buff personaje)

---------- PARTE 4 ----------------
--Por ejemplo, si la armadura tuviera 10 partes, todas con 10 de durabilidad y sufre un desgaste de intensidad 12, la primer parte queda con 0 durabilidad, la segunda con 4, la tercera 7, la cuarta 9, y de la quinta hasta el final todas intactas con 10 de durabilidad.

desgaste :: Int -> [Parte] -> [Parte]
desgaste intensidad [] = []
desgaste intensidad (parte:partes) = desgasteParte intensidad parte: desgaste (div intensidad 2) partes

desgasteParte:: Int-> Parte -> Parte
desgasteParte intensidad (x, durabilidad) = 
                         (x, max 0 (durabilidad - intensidad))


---------- PARTE 5 ----------------

maximoSegun criterio lista = foldl1 (maxSegun criterio) lista 
--f a b = foldl1 (g a) b 

maxSegun criterio b c 
 | criterio b > criterio c = b 
 | otherwise = c
--g a b c 
-- | a b > a c = b 
-- | otherwise = c

leGana:: Clan->Clan->Bool
leGana clan otroClan = total poderAtaque clan > total poderDefensa otroClan

total::(Personaje->Float) ->Clan->Float
total accion clan = (sum.map (poderConElMejorBuff accion clan).miembros) clan 

poderConElMejorBuff:: (Personaje->Float)-> Clan -> Personaje -> Float
poderConElMejorBuff accion clan personaje = 
 (accion.potenciar personaje.mejorBuffPara accion clan) personaje
--poderConElMejorBuff accion clan personaje = 
-- (accion. ($personaje).mejorBuffPara accion clan) personaje
 
mejorBuffPara:: (Personaje->Float) ->Clan -> Personaje -> Buff
mejorBuffPara accion clan personaje = maximoSegun (accion.potenciar personaje) (buffs clan)

