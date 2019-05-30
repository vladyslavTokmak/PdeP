import Data.List

type Pirata = (String,[Tesoros])
nombre = fst
tesoros = snd

type Tesoros = (String,Int)
nombreTesoro = fst
valorTesoro = snd

juan = ("Juan",[("asd",10),("qwe",20),("zxc",40),("rty",30),("poi",200)])
jackSparrow = ("Jack Sparrow",[("Brujula",10001),("Frasco de arena",0)])
davidJones = ("David Jones",[("Cajita musical",1)])
anneBonny = ("Anne Bonny",[("Doblones",100),("Frasco de arena",1)])

cantTesorosDe pirata = (length.tesoros) pirata

esAfortunado pirata = ((>10000).sum.todosLosValores) pirata 

todosLosValores pirata = map valorTesoro (tesoros pirata)

--mismoTesoro pirata otroPirata = 

valorTesoroMasValioso pirata = (maximum.todosLosValores) pirata

adquirirTesoro pirata tesoro = tesoro : (tesoros pirata)

--perderTesorosValiosos pirata = filter ((valorTesoro (tesoros pirata))>100) pirata

