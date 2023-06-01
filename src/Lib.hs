module Lib () where
--Datos del Enunciado
data Planeta = Planeta String Posicion (Int -> Int)

posicion (Planeta _ p _) = p
tiempo (Planeta _ _ t) = t

type Posicion = (Float, Float, Float)
coordX (x,_,_) = x
coordY (_,y,_) = y
coordZ (_,_,z) = z

-- De los astronautas sabemos el nombre, la edad terrestre y el planeta en el que están
data Astronauta = UnAstronauta String Int Planeta

nombre (Astronauta n _ _) = n
edad (Astronauta _ e _) = e
planeta (Astronauta _ _ p) = p

--Punto 1:
--a) 
distanciaEntre :: Planeta -> Planeta -> Float
distanciaEntre unPlaneta  =  (sqrt . sumaDeLaDiferenciaDeCuadrados unPlaneta) 

sumaDeLaDiferenciaDeCuadrados :: Planeta -> Planeta -> Float
sumaDeLaDiferenciaDeCuadrados unPlaneta otroPlaneta = aplicarRestaAlCuadradoACoordenadas (coordX . posicion) unPlaneta otroPlaneta +  aplicarRestaAlCuadradoACoordenadas (coordY . posicion) unPlaneta otroPlaneta + aplicarRestaAlCuadradoACoordenadas (coordZ . posicion) unPlaneta otroPlaneta

aplicarRestaAlCuadradoACoordenadas :: (Planeta -> Float) -> Planeta -> Planeta -> Float
aplicarRestaAlCuadradoACoordenadas coordenadas unPlaneta  = (restaAlCuadrado (coordenadas unPlaneta) . coordenadas)

restaAlCuadrado :: Float -> Float -> Float
restaAlCuadrado unValor  = ((^2) . subtract unValor)

--b)
tiempoDeViaje :: Float -> Planeta -> Planeta -> Float
tiempoDeViaje unaVelocidad unPlaneta = ((/ unaVelocidad) . distanciaEntre unPlaneta)
--Punto 2:

pasarTiempo :: Int -> Astronauta -> Astronauta
pasarTiempo unosAños unAstronauta = (flip cambiarEdad unAstronauta . (+) . (tiempo . planeta) unAstronauta) unosAños
 
cambiarEdad :: (Int -> Int) -> Astronauta -> Astronauta
cambiarEdad modificacion (Astronauta unNombre edad unPlaneta) = Astronauta unNombre (modificacion edad) unPlaneta

--Punto 3:
type Nave = Planeta -> Planeta -> Float
--a)
naveVieja :: Int -> Planeta -> Planeta -> Float
naveVieja cantidadDeTanques   = tiempoDeViaje (velocidadSegunTanques cantidadDeTanques)

velocidadSegunTanques :: Int -> Float
velocidadSegunTanques unaCantidad
    | unaCantidad < 6 = 10
    | otherwise       = 7
--b)
naveFuturista ::  Planeta -> Planeta -> Float
naveFuturista _ _ = 0
viajar ::  Planeta -> Planeta -> Nave -> Astronauta -> Astronauta
viajar origen destino unaNave  = (cambiarPlaneta destino . (aumentarEdad . round) (unaNave origen destino))

aumentarEdad :: Int -> Astronauta -> Astronauta
aumentarEdad unaCantidad = cambiarEdad ((+unaCantidad))

cambiarPlaneta :: Planeta -> Astronauta -> Astronauta
cambiarPlaneta unPlaneta (Astronauta unNombre unaEdad _) = Astronauta unNombre unaEdad unPlaneta

--Punto 4:
type Tripulacion = [Astronauta]
rescatarAstronauta :: Nave -> Astronauta -> Planeta -> Tripulacion -> Tripulacion
rescatarAstronauta unaNave unAstronauta destino unaTripulacion = ((viajeTripulacion destino (planetaDeOrigen unaTripulacion)  unaNave) . (incorporarTripulante (pasarTiempo  ((round . unaNave (planetaDeOrigen unaTripulacion)) destino)  unAstronauta)) . viajeTripulacion (planetaDeOrigen unaTripulacion) destino unaNave) unaTripulacion

planetaDeOrigen :: Tripulacion -> Planeta
planetaDeOrigen = (planeta . head)

incorporarTripulante :: Astronauta ->Tripulacion -> Tripulacion
incorporarTripulante unAstronauta = (unAstronauta :)

viajeTripulacion :: Planeta -> Planeta -> Nave -> Tripulacion -> Tripulacion
viajeTripulacion unPlaneta otroPlaneta unaNave = map (viajar unPlaneta otroPlaneta unaNave)

--Punto 5:
f ::(Ord b)=> (b -> e -> b) -> b -> (Int -> e -> Bool)-> [e]-> Bool
f a b c = any ((> b).a b).filter (c 10) 
