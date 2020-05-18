type Desgaste = Float
type Patente = String
type Fecha = (Int, Int, Int)
 
-- Definiciones base
anio :: Fecha -> Int
anio (_, _, year) = year
 
data Auto = Auto {patente :: Patente, desgasteLlantas :: [Desgaste], rpm :: Int, temperaturaAgua :: Int, ultimoArreglo :: Fecha} deriving (Show, Eq)
---------------------------------------------------------------------------------
--Ejercicio 1
costoDeReparacion :: Auto -> Int
costoDeReparacion = evaluarPatente.patente

evaluarPatente :: Patente -> Int
evaluarPatente unaPatente
    |length unaPatente == 7 = 12500
    |tieneLasLetras "NB" "DJ" unaPatente = calculoPatental unaPatente
    |otherwise = 15000

tieneLasLetras :: String -> String -> Patente -> Bool
tieneLasLetras parLetras1 parLetras2 unaPatente = (take 2 unaPatente) == parLetras1 || (take 2 unaPatente) == parLetras2

calculoPatental :: Patente -> Int
calculoPatental unaPatente
    |last unaPatente == '4' = 3000 * length unaPatente
    |otherwise = 20000

--------------------------------------------------------------------------------
--Ejercicio 2
autoPeligroso :: Auto -> Bool
autoPeligroso = chequeoPrimeraLlanta.desgasteLlantas

chequeoPrimeraLlanta :: [Desgaste] -> Bool
chequeoPrimeraLlanta desgastesDeLaLlanta = (head desgastesDeLaLlanta) > 0.5

necesitaRevision :: Auto -> Bool
necesitaRevision unAuto = anio (ultimoArreglo unAuto) <= 2015

--------------------------------------------------------------------------------
--Ejercicio 3

alfa :: Auto -> Auto
alfa unAuto
  | rpm unAuto <= 2000 = unAuto
  | otherwise = unAuto {rpm = 2000}

bravo :: Auto -> Auto
bravo unAuto = unAuto {desgasteLlantas = [0,0,0,0]} 

charly :: Auto -> Auto
charly unAuto = alfa unAuto
--faltaria poner que hace lo mismo que bravo (sin repetir logica)

tango :: Auto -> Auto
tango unAuto = unAuto

zulu :: Auto -> Auto
zulu unAuto = unAuto {temperaturaAgua = 90}
--faltaria agregar que hace lo mismo que Lima

--lima :: Auto -> Auto
--lima unAuto = unAuto {(take 2 desgasteLlantas) = [0,0]}

--------------------------------------------------------------------------------
--Ejercicio 4

ordenadosTOC :: [Auto] -> Bool
ordenadosTOC autos = elementosParesSonPares (listaDeDesgastes autos) && elementosImparesSonImpares (listaDeDesgastes autos)

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
