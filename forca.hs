import System.Random
import Data.Char

main = do
    palavras <- readFile "palavras.txt" >>= (return .filter((>=5) . length) . lines)
    n <- randomRIO (1, length palavras)
    let palavra = palavras !! (n-1)
    ciclo palavra []

ciclo :: String -> [Char] -> IO ()
ciclo palavra letras = do
    putStrLn $ unlines [
        esconderPalavra palavra letras,
        "",
        "1) Adivinhar letra.",
        "2) Adivinhar palavra."
        ]
    opcao <- getLine
    case opcao of
        "1" -> do
            putStrLn "Escreve uma letra: "
            letra <- getLine
            if length letra == 1 && isAlpha (head letra) then
                ciclo palavra (head letra : letras)
            else do
                putStrLn "Letra Inválida!"
                ciclo palavra letras
        "2" -> do
            putStrLn "Introduz uma palavra: "
            p <- getLine
            if p == palavra then
                putStrLn "Congrats motherfucker"
            else
                putStrLn $ "Get fucked bitch. A palavra é" ++ palavra

esconderPalavra :: String -> [Char] -> String
esconderPalavra palavra letras = map (\letra -> if letra `elem` letras then letra else '_') palavra