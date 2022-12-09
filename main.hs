-- solucion del ejercicio usando haskell.
-- se generan datos tipo Record para guardar la informacion de entrada y salida.
-- en el futuro deberá modificar para devolver listas de colecciones llave/valor

import Data.Time.Clock
import Data.Time.Calendar
import Data.Char

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List as List
import Data.Monoid
import Text.Printf


data Person = Person {
        firstName :: String,
        lastName :: String,
        country :: String,
        continent :: String,
        age :: Integer,
        language :: String
    }  deriving (Show)

data UserName = UserName {
        username :: String
    } deriving (Show)

data DataOutput = DataOutput {
        outPerson :: Person,
        outUsername :: UserName
    } deriving (Show)

list1 = [
        Person{firstName="Emily", lastName="N.", country="Ireland", continent="Europe", age=30, language="ruby"},
        Person{firstName="Nor", lastName="E.", country="Malaysia", continent="Asia", age=20, language="haskell"}
    ]

-- IO devuelve valores que pueden tener side effects
getActualYear :: IO Integer
getActualYear = do
    now <- getCurrentTime
    let (year, month, day) = toGregorian $ utctDay now
    return year


getBornYear :: Integer -> Integer -> Integer
getBornYear age actualyear = actualyear - age
-- liftM (\x -> x-age) getActualYear


getUserName :: String -> String -> Integer -> String
getUserName firstName lastName bornYear = 
    let fname = map toLower firstName
        lname = take 1 $ map toLower lastName
    in (\year -> fname ++ lname ++ show(year)) bornYear

generateUserName :: Integer -> Person -> String
generateUserName actualYear personData =
    let firstname = firstName personData
        lastname = lastName personData
        userage = age personData
        bornyear = getBornYear userage actualYear
    in getUserName firstname lastname bornyear

main :: IO ()
main = do
    actualYear <- getActualYear
--    let usernames = map (\uname -> UserName{username=generateUserName actualYear uname}) list1
    let usernames = map (\uname -> DataOutput { outPerson = uname, outUsername = UserName{username=generateUserName actualYear uname}}) list1
    print(usernames)
