module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero


----------- PARTE 1 -----------

-- Nomu modelado con tuplas
-- type Nomu = (Bool,      Bool,                 Number,       String,      Number, Number) 
        --     (TieneAlas, TieneMultiplesBrazos, CantidadOjos, ColorDePiel, Vida,   Fuerza)


-- Usando typenames
type TieneAlas = Bool
type TieneMultiplesBrazos = Bool
type CantidadOjos = Number
type Vida = Number
type Fuerza = Number

type Nomu = (TieneAlas, TieneMultiplesBrazos, CantidadOjos, ColorDePiel, Vida, Fuerza)


-- Usando data y enums para ColorDePiel
data ColorDePiel = GRIS | AZUL | BLANCO | NEGRO deriving (Show)


-- Instanciamos algunos Nomus con tuplas
pepe :: Nomu
pepe = (True, False, 0, GRIS, 200, 100)

juan :: Nomu
juan = (True, True, 4, NEGRO, 1000, 3500)


-- Averiguamos si un Nomu tiene ojos
tieneOjos :: Nomu -> Bool
tieneOjos (_,_,cantidadOjos,_,_,_) = cantidadOjos > 0


-- Modelamos las categorías
data Categoria = HIGH_END | FUERTE | COMUN | PICHI deriving (Show)

categoriaConTuplas :: Nomu -> Categoria
categoriaConTuplas (_,_,_,_,_,fuerza)
    | fuerza > 10000 = HIGH_END
    | fuerza > 3000 = FUERTE
    | fuerza > 1000 = COMUN
    | otherwise = PICHI


----------- PARTE 2 -----------

-- Ahora modelamos todo con data
data DataNomu = UnNomu {
    tieneAlas :: TieneAlas,
    tieneMultiplesBrazos :: TieneMultiplesBrazos,
    cantidadOjos :: CantidadOjos,
    colorDePiel :: ColorDePiel,
    vida :: Vida,
    fuerza :: Fuerza
} deriving (Show)

rocco :: DataNomu
rocco = UnNomu True True 3 GRIS 200 1000

arturo :: DataNomu
arturo = UnNomu {
    tieneAlas = True, 
    tieneMultiplesBrazos = False, 
    cantidadOjos = 0, 
    colorDePiel = BLANCO, 
    vida = 900, 
    fuerza = 5000
}

categoriaConData :: DataNomu -> Categoria
categoriaConData nomu
    | fuerza nomu > 10000 = HIGH_END
    | fuerza nomu > 3000 = FUERTE
    | fuerza nomu > 1000 = COMUN
    | otherwise = PICHI

tieneOjosConData :: DataNomu -> Bool
tieneOjosConData nomu = cantidadOjos nomu > 0 


----------- PARTE 3 -----------
-- Ahora modelamos los poderes
data Poder = UnPoder {
    cantidadDeCuracionPorUso :: Number,
    cantidadDeDañoPorUso :: Number,
    rangoDeAtaque :: Number,
    probabilidadDeDanioCritico :: Number
} deriving (Show)

-- Creamos algunos poderes
superRegeneracion :: Poder
superRegeneracion = UnPoder 1000 0 5 0 

superFuerza :: Poder
superFuerza = UnPoder 100 3000 15 0.5

proyectilesDeAire :: Poder
proyectilesDeAire = UnPoder 0 2500 500 0.15


-- Modelamos y creamos un Nomu con poderes
type Poderes = [Poder]

data NomuConPoderes = UnNomuConPoderes{
    tieneAlas :: TieneAlas,
    tieneMultiplesBrazos :: TieneMultiplesBrazos,
    cantidadOjos :: CantidadOjos,
    colorDePiel :: ColorDePiel,
    vida :: Vida,
    fuerza :: Fuerza,
    poderes :: Poderes
} deriving(Show)

mateo :: NomuConPoderes
mateo = UnNomuConPoderes True True 3 GRIS 200 1000 [superRegeneracion, superFuerza, proyectilesDeAire]


-- Averiguamos la probabilidad de daño crítico del último poder conseguido por un Nomu
probabilidadDeCritico :: NomuConPoderes -> Number
probabilidadDeCritico nomu = probabilidadDeDanioCritico (last (poderes nomu))

-- Averiguamos si un poder es cuerpo a cuerpo
esUsadoCuerpoACuerpo :: Poder -> Bool
esUsadoCuerpoACuerpo poder = rangoDeAtaque poder < 100

-- Averiguamos si un poder es solamente de curación
esSolamenteDeCuracion :: Poder -> Bool
esSolamenteDeCuracion poder = (cantidadDeCuracionPorUso poder > 0) && (cantidadDeDañoPorUso poder == 0)
