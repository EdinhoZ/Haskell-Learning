cumprimentarIO :: IO ()
cumprimentarIO = do
    putStr "Como te chamas?"
    nome <- getLine
    let cumprimento = cumprimentar nome
    putStrLn cumprimento

cumprimentar :: String -> String
cumprimentar nome = "Olá " ++ nome ++ " !"