import Text.Show.Functions

type Desgaste = Float
type Patente = String
type Fecha = (Int, Int, Int)
type Mecanico = Auto -> Auto
 
-- Definiciones base
anio :: Fecha -> Int
anio (_, _, year) = year
 
data Auto = Auto {patente :: Patente, desgasteLlantas :: [Desgaste], rpm :: Int, temperaturaAgua :: Int, ultimoArreglo :: Fecha} deriving (Show, Eq)

--EJEMPLOS DE AUTOS
honda :: Auto
honda = Auto "AT001LN" [0.5, 0.4,0.2,0] 1500 90 (20,02,2015)

ford :: Auto
ford = Auto "DJV214" [0.6, 0.5,0.6,0.1] 1700 70 (21,12,2016)

audi :: Auto
audi = Auto "DJV215" [0.1, 0.1,0.1,0] 1100 30 (22,03,2019)

fiat :: Auto
fiat = Auto "ERT371" [0.1, 0.4, 0.2, 0] 1500 100 (13,08,2012)

renault :: Auto
renault = Auto "DJK004" [0.2, 0.5, 0.6, 0.1] 2000 60 (15,09,2016)

volkswagen :: Auto
volkswagen = Auto "AA574OP" [0.1, 0.1, 0.1, 0] 1350 90 (06,02,2020)

mercedes :: Auto
mercedes = Auto "DFH029" [0.3,0.1,0.1,0] 1200 10 (24,02,2014)

chevrolet :: Auto
chevrolet = Auto "ALO591" [0.7,0.4,0.2,0.1] 1600 20 (22,09,2017)

---------------------------------------------------------------------------------
--Ejercicio 1
costoDeReparacion :: Auto -> Int
costoDeReparacion = evaluarPatente.patente
--honda = 12500 ; ford = 18000 ; audi = 20000 ; mercedes = 15000 ; chevrolet = 15000
--reanult = 18000 ; volkswagen = 12500

evaluarPatente :: Patente -> Int
evaluarPatente unaPatente
    |length unaPatente == 7 = 12500
    |tieneLasLetras "DJ" "NB" unaPatente = calculoPatental unaPatente
    |otherwise = 15000

tieneLasLetras :: String -> String -> Patente -> Bool
tieneLasLetras parLetras1 parLetras2 unaPatente = unaPatente >= parLetras1 && unaPatente <= parLetras2

calculoPatental :: Patente -> Int
calculoPatental unaPatente
    |last unaPatente == '4' = 3000 * length unaPatente
    |otherwise = 20000

--------------------------------------------------------------------------------
--Ejercicio 2
autoPeligroso :: Auto -> Bool
autoPeligroso = chequeoPrimeraLlanta.desgasteLlantas
--SON TRUE: ford y chevrolet

chequeoPrimeraLlanta :: [Desgaste] -> Bool
chequeoPrimeraLlanta desgastesDeLaLlanta = (head desgastesDeLaLlanta) > 0.5

necesitaRevision :: Auto -> Bool
necesitaRevision unAuto = anio (ultimoArreglo unAuto) <= 2015
--SON TRUE: mercedes, fiat, honda

--------------------------------------------------------------------------------
--Ejercicio 3

alfa :: Mecanico
alfa unAuto
  | rpm unAuto <= 2000 = unAuto
  | otherwise = unAuto {rpm = 2000}

bravo :: Mecanico
bravo unAuto = unAuto {desgasteLlantas = [0,0,0,0]} 

charly :: Mecanico
charly = (bravo . alfa)

tango :: Mecanico
tango unAuto = unAuto

zulu :: Mecanico
zulu unAuto = lima (unAuto {temperaturaAgua = 90})

lima :: Mecanico
lima unAuto = unAuto {desgasteLlantas = [0,0] ++ drop 2 (desgasteLlantas unAuto)}

--------------------------------------------------------------------------------
--Ejercicio 4

ordenadosTOC :: [Auto] -> Bool
ordenadosTOC autos = elementosParesSonPares (listaDeDesgastes autos) && elementosImparesSonImpares (listaDeDesgastes autos)
--[fiat, renault, volkswagen] => Ordenado

elementosParesSonPares :: [Int] -> Bool
elementosParesSonPares desgastes = elementosXsonX even elementosPares desgastes

elementosImparesSonImpares :: [Int] -> Bool
elementosImparesSonImpares desgastes = elementosXsonX odd elementosImpares desgastes

--Retorna True/False de acuerdo a si todos los elementos X son X o no. (X = Pares o Impares).
elementosXsonX :: (Int->Bool) -> ([Int]->[Int]) -> [Int] -> Bool
elementosXsonX funcion1 funcion2 desgastes = foldl (&&) True (map funcion1 (funcion2 desgastes))

--Recibe una lista y devuelve los elementos con posicion impar de esa lista.
elementosImpares :: [a] -> [a]
elementosImpares (x:xs) = x:elementosPares xs
elementosImpares _ = []

--Recibe una lista y devuelve los elementos con posicion par de esa lista.
elementosPares :: [a] -> [a]
elementosPares (_:xs) = elementosImpares xs
elementosPares _ = []

--Recibe un auto y devuelve la cantidad de desgaste del mismo.
cantidadDeDesgaste :: Auto -> Int
cantidadDeDesgaste auto = (round.((*10).sum)) (desgasteLlantas auto)

--Recibe una lista de autos y devuelve una lista con la cantidad de desgaste de cada uno de ellos.
listaDeDesgastes :: [Auto] -> [Int]
listaDeDesgastes autos = map cantidadDeDesgaste autos

--------------------------------------------------------------------------------
--Ejercicio 5

aplicarOrdenDeReparacion :: Fecha -> [Mecanico] -> Mecanico
aplicarOrdenDeReparacion fecha listaDeTecnicos unAuto= (renovarFechaDeReparacion fecha . aplicarTecnicos listaDeTecnicos) unAuto

aplicarTecnicos :: [Mecanico] -> Mecanico
aplicarTecnicos listaDeTecnicos unAuto = foldr ($) unAuto listaDeTecnicos

renovarFechaDeReparacion:: Fecha -> Mecanico
renovarFechaDeReparacion fecha unAuto = unAuto {ultimoArreglo = fecha}

--------------------------------------------------------------------------------
--Ejercicio 6

tecnicosEnCondiciones :: [Mecanico] -> Auto -> [Mecanico]
tecnicosEnCondiciones listaDeTecnicos unAuto = filter (loDejaEnCondiciones unAuto) listaDeTecnicos
--ford [alfa, bravo, charly, tango, zulu, lima] => [bravo, charly, zulu, lima]
--honda [alfa, bravo, charly, tango, zulu, lima] => [alfa, bravo, charly, tango, zulu, lima]

loDejaEnCondiciones :: Auto -> Mecanico -> Bool
loDejaEnCondiciones unAuto unMecanico = not (autoPeligroso (unMecanico unAuto))

costoTotal :: [Auto] -> Int
costoTotal listaAutos = sum (costosDeLasReparaciones listaAutos)
--[honda, ford, audi, mercedes] => 27500

costosDeLasReparaciones :: [Auto] -> [Int]
costosDeLasReparaciones listaDeAutos = map costoDeReparacion (filter necesitaRevision listaDeAutos)

--------------------------------------------------------------------------------
--Ejercicio 7
{-
    Parte 1) Si, podriamos obtener el primer tecnico que deja el auto en condiciones, porque haskell funciona con Lazy Evaluation. Por lo tanto, 
             ya con tener los elementos que requiere el take, haskell deja de trabajar buscando el resto del filter. Pero lo que no podriamos 
             saber es el nombre del mismo, ya que los tecnicos son funciones y no podemos saber el nombre de las mismas.
             Por ejemplo: take 1 (filter (even) [1..])
                Podriamos considerar con que en este ejemplo [1..] seria la lista infinita de tecnicos y even seria loDejaEnCondiciones
    
    Parte 2) 
         a-  No podriamos tener una lista infinita de autos ya que costosDeLasReparaciones nunca devolveria la lista de autos que necesitan revision
             porque el filter nunca terminaria de evaluar la lista de autos.
             Por ejemplo: (sum . filter (even)) [1..]
                En este ejemplo, sum necesita una lista definida para poder devolver la sumatoria de todos sus elementos, pero la funcion filter no podria
                darla porque nunca va a terminar de filtrar una lista infinita. 
         
         b-  Para este caso, la funcion deberia cambiarse agregando un take delante del filter: 
                costosDeLasReparaciones listaDeAutos = map costoDeReparacion ((take 3 . filter necesitaRevision) listaDeAutos)
            
             Y si aceptaria una lista infinita de autos, ya que seria un caso similar al punto 1
-}