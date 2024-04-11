module Library where
import PdePreludat

----------- PARTE 1 -----------
data Nomu = UnNomu {
    tieneAlas :: Bool,
    tieneMultiplesBrazos :: Bool,
    cantDeOjos :: Number,
    colorDePiel :: ColorDePiel,
    vida :: Number,
    fuerza :: Number,
    poderes :: [Poder]          -- Se lo agregamos en la parte 2
} deriving (Show)

data ColorDePiel = Gris | Azul | Blanco | Negro deriving (Show)

puedeVer :: Nomu -> Bool
puedeVer nomu = cantDeOjos nomu > 0

data Categoria = Comun | Fuerte | HighEnd | Pichi deriving (Show)

categoria :: Nomu -> Categoria
categoria nomu
    | fuerza nomu > 10000 = HighEnd
    | fuerza nomu > 3000 = Fuerte
    | fuerza nomu > 1000 = Comun
    | otherwise = Pichi


----------- PARTE 2 -----------
data Poder = UnPoder {
    cantCuracion :: Number,
    cantDanio :: Number,
    rango :: Number,
    probCritico :: Number
} deriving (Show)

-- Instanciamos algunos poderes
superRegeneracion :: Poder
superRegeneracion = UnPoder 1000 0 5 0 

superFuerza :: Poder
superFuerza = UnPoder 100 3000 15 0.5

proyectilesDeAire :: Poder
proyectilesDeAire = UnPoder 0 2500 500 0.15


-- Instanciamos algunos Nomus
mateo :: Nomu
mateo = UnNomu True True 3 Gris 200 1000 [superRegeneracion, superFuerza, proyectilesDeAire]

arturo :: Nomu
arturo = UnNomu {
    tieneAlas = True, 
    tieneMultiplesBrazos = False, 
    cantDeOjos = 0, 
    colorDePiel = Blanco, 
    vida = 900, 
    fuerza = 5000,
    poderes = [superRegeneracion]
}

-- Averiguamos la probabilidad de daño crítico del último poder conseguido por un Nomu
probabilidadDeCritico :: Nomu -> Number
probabilidadDeCritico nomu = probCritico (last (poderes nomu))

-- Averiguamos si un poder es cuerpo a cuerpo
esCuerpoACuerpo :: Poder -> Bool
esCuerpoACuerpo poder = rango poder < 100

-- Averiguamos si un poder es solamente de curación
esSoloDeCuracion :: Poder -> Bool
esSoloDeCuracion poder = cantCuracion poder > 0 && cantDanio poder == 0
