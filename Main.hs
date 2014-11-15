import FaceCube

main :: IO ()
main = do
    let facelets = "DUDLUDLFDRBRFRBLUUBUBLFRLDFFFUDDBRDRBFUBLUULDFRLRBLBRF"
    let fc = strToFaceCube facelets
    putStrLn $ show fc
    let cc = toCubieCube fc
    putStrLn $ show cc
