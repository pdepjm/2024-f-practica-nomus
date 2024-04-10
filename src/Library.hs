module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

type TieneAlas = Bool
type TieneMultiplesBrazos = Bool
type CantidadOjos = Number
type Vida = Number
type Fuerza = Number
data ColorDePiel = GRIS | AZUL | BLANCO | NEGRO deriving(Show)

data Categoria = HIGH_END | FUERTE | COMUN | PICHI deriving(Show) --estos deriving para que se vea en consola si haces pepe de consulta

---Parte 1

type Nomu = (TieneAlas, TieneMultiplesBrazos, CantidadOjos, ColorDePiel, Vida, Fuerza) 

pepe::Nomu
pepe = (True, False, 0, GRIS, 200, 100)

juan::Nomu
juan = (True, True, 4, NEGRO, 1000, 3500)

categoriaConTuplas :: Nomu -> Categoria
categoriaConTuplas (_,_,_,_,_,fuerza)
    | fuerza > 10000 = HIGH_END
    | fuerza > 3000 = FUERTE
    | fuerza > 1000 = COMUN
    |otherwise = PICHI

noTieneOjos :: Nomu -> Bool
noTieneOjos (_,_,cantidadOjos,_,_,_) = cantidadOjos == 0

---Parte 2


data DataNomu = UnNomu{
    tieneAlas :: TieneAlas,
    tieneMultiplesBrazos :: TieneMultiplesBrazos,
    cantidadOjos :: CantidadOjos,
    colorDePiel :: ColorDePiel,
    vida :: Vida,
    fuerza :: Fuerza
} deriving(Show)

rocco::DataNomu
rocco = UnNomu True True 3 GRIS 200 1000

arturo::DataNomu
arturo = UnNomu{tieneAlas = True, tieneMultiplesBrazos = False, cantidadOjos = 0, colorDePiel = BLANCO, vida = 900, fuerza = 5000}

categoriaConData :: DataNomu -> Categoria
categoriaConData nomu
    | fuerza nomu > 10000 = HIGH_END
    | fuerza nomu > 3000 = FUERTE
    | fuerza nomu > 1000 = COMUN
    |otherwise = PICHI

noTieneOjosConData :: DataNomu -> Bool
noTieneOjosConData nomu = cantidadOjos nomu == 0 

--- Parte 3

data Poder = UnPoder{
    cantidadDeCuracionPorUso :: Number,
    cantidadDeDañoPorUso :: Number,
    rangoDeAtaque :: Number,
    probabilidadDeDañoCritico :: Number
} deriving (Show)


superRegeneracion :: Poder
superRegeneracion = UnPoder 1000 0 5 0 

superFuerza :: Poder
superFuerza = UnPoder 100 3000 15 0.5

proyectilesDeAire :: Poder
proyectilesDeAire = UnPoder 0 2500 500 0.15

type Poderes = [Poder]

{- data DataNomuConPoderes = UnNomuConPoderes{
    tieneAlas :: TieneAlas,
    tieneMultiplesBrazos :: TieneMultiplesBrazos,
    cantidadOjos :: CantidadOjos,
    colorDePiel :: ColorDePiel,
    vida :: Vida,
    fuerza :: Fuerza,
    poderes :: Poderes
} deriving(Show)

mateo::DataNomuConPoderes
mateo = UnNomuConPoderes True True 3 GRIS 200 1000 [superRegeneracion, superFuerza, proyectilesDeAire]

probabilidadDeCritico :: DataNomuConPoderes-> Number
probabilidadDeCritico nomu = probabilidadDeDañoCritico (last(poderes nomu))   -} -- hay que comentar el DataNomu de mas arriba antes de descomentar este para que no tire error de definiciones!!

esUsadoCuerpoACuerpo :: Poder -> Bool
esUsadoCuerpoACuerpo poder = rangoDeAtaque poder < 100

esSolamenteDeCuracion :: Poder -> Bool
esSolamenteDeCuracion poder = (cantidadDeCuracionPorUso poder > 0) && (cantidadDeDañoPorUso poder <= 0)
