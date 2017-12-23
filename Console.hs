module Console where

    import Formula
    import Utils
    import System.Exit
    import System.Console.ANSI    
    import System.IO.Unsafe
    import System.Directory
    import System.IO
    import Data.List.Split
    

    type Handler = [String] -> Formula -> IO(Formula)

    baseFormula :: Formula
    baseFormula = Formula { typeFormula = "horn",
                            nbClauses   = 0,
                            nbVars      = 0,
                            clauses     = [] }

    data Command = Command {
	    name :: String,        -- Nom de la commande
	    description :: String, -- Description de la commande
	    exits :: Bool,         -- Drapeau pour sortir de la boucle 
				           -- Vrai pour quit, faux pour les autres
	    run :: Handler         -- Le code à executer			 
    }

    commands :: [String]
    commands = [":quit", ":q", ":clear", ":load", ":formula", ":f", ":solve", ":s" ]

    quit :: Command
    quit = Command {
	    name = "quit",
	    description = "USAGE: [:quit | :q] : close the program",
	    exits = True,
	    run = handlerQuit
    }

    clear :: Command
    clear = Command {
        name = "clear",
        description = "USAGE: [:clear] : clear screen",
        exits = False,
        run = handlerClear
    }

    load :: Command
    load = Command {
        name = "load",
        description = "USAGE: [:load file type] : load a formula (dimacs format)",
        exits = False,
        run = handlerLoad
    }

    resol :: Command
    resol = Command {
        name = "solve",
        description = "USAGE: [:solve | :s] : try to solve current formula",
        exits = False,
        run = handlerSolve
    }

    formula :: Command
    formula = Command {
        name = "formula",
        description = "USAGE: [:formula | :f] : display the current formula",
        exits = False,
        run = handlerFormula
    }


    handlerQuit :: Handler
    handlerQuit _ f = do
        exitWith ExitSuccess
        return f
    
        
    handlerClear :: Handler
    handlerClear _ f = do
        clearScreen
        return f
    

    handlerLoad :: Handler
    handlerLoad (file:(t:xs)) f = do
        if ((fromIo (doesFileExist file)) == True) then
            do
                f <- toIo (getFormula file t)
                printfln "current formula : "
                printfln (show f)
                return f
        else
            do
                printf "file '"
                printf file
                printfln "' doesn't exists."
                return f

    handlerLoad _ f = do
        printfln "Invalid arguments."
        return f

    handlerSolve :: Handler
    handlerSolve xs f = do
        putStrLn "tring to solve : "
        m <- solve (fromIo((run formula) xs f))
        putStrLn "model found : " 
        putStrLn (show m)
        return f


    handlerFormula :: Handler
    handlerFormula _ f = do
        putStrLn (show f)
        return f


    getCommand :: String -> Command
    getCommand c
        | c == ":quit" || c == ":q" = quit
        | c == ":clear" = clear
        | ((take 6 c) == ":solve" || (take 1 c) == ":s") = resol
        | ((take 5 c) == ":load"  || (take 1 c) == ":l") = load
        | c == ":formula" || c == ":f" = formula


    getCommandName :: String -> String
    getCommandName c = head (splitOn " " c)
    
    getArgs :: String -> [String]
    getArgs c = tail (splitOn " " c)
    
    isCommand :: String -> Bool
    isCommand c = contains c commands


    console :: Formula -> IO ()
    console f = do
        printf "@SatSolver $> "
        l <- getLine
        printf "\n"
        c <- toIo (getCommandName l)

        if (isCommand c) == True then
            do
                a <- toIo (getArgs l)
                cmd <- toIo (getCommand c)
                f <- (run cmd) a f
                console f
        else
            do
                printf "Invalid command.\n"
                console f


    launcher :: IO()
    launcher = do 
        console baseFormula 


    