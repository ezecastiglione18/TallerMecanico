type Desgaste = Int --CAMBIAR A FLOAT!!!!
type Patente = String
type Fecha = (Int, Int, Int)
 
-- Definiciones base
anio :: Fecha -> Int
anio (_, _, year) = year
 
data Auto = Auto {patente :: Patente, desgasteLlantas :: [Desgaste], rpm :: Int, temperaturaAgua :: Int, ultimoArreglo :: Fecha} deriving (Show, Eq)
---------------------------------------------------------------------------------
--Ejercicio 1
costoDeReparacion :: Auto -> Int
costoDeReparacion unAuto
    |length (patente unAuto) == 7 = 12500
    |tieneLasLetras "NB" "DJ" (patente unAuto) = calculoPatental (patente unAuto)
    |otherwise = 15000

tieneLasLetras :: String -> String -> Patente -> Bool
tieneLasLetras parLetras1 parLetras2 unaPatente = True

calculoPatental :: Patente -> Int
calculoPatental unaPatente
    |last unaPatente == '4' = 3000 * length unaPatente
    |otherwise = 20000

--------------------------------------------------------------------------------
--Ejercicio 2
autoPeligroso :: Auto -> Bool
autoPeligroso = chequeoPrimeraLlanta.desgasteLlantas

chequeoPrimeraLlanta :: [Desgaste] -> Bool
chequeoPrimeraLlanta desgastesDeLaLlanta = (head desgastesDeLaLlanta) > 1 --CAMBIAR A 0.5!!!!!

necesitaRevision :: Auto -> Bool
necesitaRevision unAuto = anio (ultimoArreglo unAuto) <= 2015