--------------
-- Punto 01 --
--------------

-- a)

data Pais = Pais {
    ingresoPerCapita :: Int,
    sectorPublico :: Int,
    sectorPrivado :: Int,
    recursosNaturales :: [Recurso],
    deuda :: Int
} 

type Recurso = String

-- b)

namibia :: Pais
namibia = Pais 4140 400000 650000 ["mineria", "ecoturismo"] 50

--------------
-- Punto 02 --
--------------

-- a)

type Estrategia = Pais -> Pais

prestar :: Int -> Estrategia
prestar cantidadDeMillones unPais = unPais { deuda = deuda unPais + (div (cantidadDeMillones * 150) 100) }

reducirPuestos :: Int -> Estrategia 
reducirPuestos cantidadPuestos unPais
    | (sectorPublico unPais) > 100 = disminuirIngresoPerCapita 20 . reducirPuestosSectorPublico cantidadPuestos $ unPais
    | otherwise                    = disminuirIngresoPerCapita 15 . reducirPuestosSectorPublico cantidadPuestos $ unPais

disminuirIngresoPerCapita :: Int -> Estrategia 
disminuirIngresoPerCapita unPorcentaje unPais = unPais { ingresoPerCapita = ingresoPerCapita unPais - div (unPorcentaje * ingresoPerCapita unPais) 100 }

reducirPuestosSectorPublico :: Int -> Estrategia  
reducirPuestosSectorPublico cantidadPuestos unPais = unPais { sectorPublico = sectorPublico unPais - cantidadPuestos }

explotar :: Recurso -> Estrategia
explotar unRecurso unPais = unPais { deuda = deuda unPais - 2, recursosNaturales = quitarRecurso unRecurso $ recursosNaturales unPais }

quitarRecurso :: Recurso -> [Recurso] -> [Recurso]
quitarRecurso unRecurso = filter (/= unRecurso)

blindaje :: Estrategia
blindaje unPais = reducirPuestos 500 . prestarProductoBrutoInterno $ unPais

prestarProductoBrutoInterno :: Pais -> Pais
prestarProductoBrutoInterno unPais = unPais { deuda = deuda unPais - mitadProductoBruto unPais }

mitadProductoBruto :: Pais -> Int
mitadProductoBruto unPais = div (productoBrutoInterno unPais) 2

productoBrutoInterno :: Pais -> Int
productoBrutoInterno unPais = ingresoPerCapita unPais * ( sectorPrivado unPais + sectorPublico unPais)

--------------
-- Punto 03 --
--------------

-- a)

type Receta = [Estrategia]

receta :: Receta
receta = [explotar "mineria", prestar 200]

-- b) 

aplicarReceta :: Receta -> Pais -> Pais
-- opción con foldl + lambda
-- aplicarReceta receta pais = foldl (\pais estrategia -> estrategia pais) pais receta
-- opción con foldr + $
aplicarReceta unaReceta unPais = foldr ($) unPais unaReceta

--------------
-- Punto 04 --
--------------

puedenZafar :: [Pais] -> [Pais]
puedenZafar = filter $ elem "Petroleo" . recursosNaturales

totalDeuda :: [Pais] -> Int
totalDeuda = foldr ((+) . deuda) 0

--------------
-- Punto 05 --
--------------

estaOrdenado :: Pais -> [Receta] -> Bool
estaOrdenado unPais [receta] = True
estaOrdenado unPais (receta1:receta2:recetas) 
    = revisarPBI receta1 unPais <= revisarPBI receta2 unPais && estaOrdenado unPais (receta2:recetas)
    where revisarPBI receta = productoBrutoInterno . aplicarReceta receta

--------------
-- Punto 06 --
--------------

recursosNaturalesInfinitos :: [String]
recursosNaturalesInfinitos = "Energia" : recursosNaturalesInfinitos
-- ¿qué sucede evaluamos la función 4a con ese país? --> NO TERMINA NUNCA
-- ¿y con la 4b? --> FUNCIONA PORQ NO NECESITA EVALUAR LOS RECURSOS
