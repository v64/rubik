import CubieCube
import FaceCube

main :: IO ()
main = do
    let facelets = "DUDLUDLFDRBRFRBLUUBUBLFRLDFFFUDDBRDRBFUBLUULDFRLRBLBRF"
    let fc = toFaceCube facelets
    putStrLn $ show fc
    let cc = toCubieCube fc
    putStrLn $ show cc
--    let c  = toCoordCube cc
--    putStrLn $ show c
