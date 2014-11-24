import CubieCube
import FaceCube

main :: IO ()
main = do
    let facelets = "DUDLUDLFDRBRFRBLUUBUBLFRLDFFFUDDBRDRBFUBLUULDFRLRBLBRF"
    -- let facelets = "LFUDUDFLBUBFURBDURRFLRFFBUFDLLDDBLLBDRDBLUFFRRRBRBLUDU"
    -- let facelets = "UFBBUFUUBLRLURBRBDBRDLFBRFFFDDDDRFFBFDRLLULRUUULLBDRLD"
    -- let facelets = "UFBUUBFRBLDUURRDUUDDDLFBBBRULFDDLBDLRFLRLFRFLRRFBBUFLD"
    -- let facelets = "BLFBULURDLDDFRLURRBFFUFDRULBBFBDUDDUUDRBLFLRDRULFBLFRB"

    let fc = toFaceCube facelets
    putStrLn $ show fc
    let cc = toCubieCube fc
    putStrLn $ show cc
    let c  = toCoordCube cc
    putStrLn $ show c

    -- let cube1 = "UFBUUBFRBLDUURRDUUDDDLFBBBRULFDDLBDLRFLRLFRFLRRFBBUFLD"
    -- let cube2 = "BLFBULURDLDDFRLURRBFFUFDRULBBFBDUDDUUDRBLFLRDRULFBLFRB"
    -- let cc1   = toCubieCube $ toFaceCube cube1
    -- let cc2   = toCubieCube $ toFaceCube cube2
    -- putStrLn $ show $ edgeMultiply cc1 cc2
