module Formula (
    Clause, 
    Dimacs,
    Type,
    Litteral,
    Clauses,
    Model,
    Solver,
    Formula ( .. ),
    getFormula,
    solve,
    myFormula
) where

    import Utils

    import Data.List.Split
    import Control.Monad
    import System.IO.Unsafe

    type Clause   = [Int]
    type Dimacs   = [String]
    type Type     = String
    type Litteral = Int
    type Clauses  = [Clause]
    type Model    = [Int]    

    type Solver   = (Clauses -> Int -> Model -> IO Model)

    data Formula = Formula {
        typeFormula :: String, 
        nbClauses   :: Int,
        nbVars      :: Int,
        clauses     :: Clauses		 
    }

    typesList :: [String]
    typesList = ["horn", "krom", "3sat", "sat"]

    typeValid :: String -> Bool
    typeValid t = contains t typesList

    emptyClause :: Clauses -> Bool
    emptyClause [[-1]] = True
    emptyClause _ = False

    getSolver :: Formula -> Solver
    getSolver f 
        | (typeFormula f) == "horn" = hornSat


    readDimacs :: FilePath -> Dimacs
    readDimacs f = splitOn "\n" (unsafePerformIO (readFile f))

    getNbClauses :: Dimacs -> Int
    getNbClauses (x:xs) = (read (l !! 3)) :: Int
        where
            l = splitOn " " x

    getNbVars :: Dimacs -> Int 
    getNbVars (x:xs) = (read (l !! 2)) :: Int
            where
                l = splitOn " " x
    
    getClauses :: Dimacs -> Clauses
    getClauses [] = []
    getClauses (x:xs) = c:(getClauses xs)
        where 
            c = init (toClause (splitOn " " x))

    toClause :: [String] -> Clause
    toClause [] = []
    toClause (x:xs) = v:(toClause xs)
        where
            v = (read x)::Int

    getFormula :: FilePath -> Type -> Formula
    getFormula f t = Formula {typeFormula = t,
                              nbClauses   = getNbClauses dimacs,
                              nbVars      = getNbVars dimacs,
                              clauses     = getClauses (tail dimacs)}
        where 
            dimacs = readDimacs f

    emptyClauses :: Clauses
    emptyClauses = []

    setTrue :: Litteral -> Clauses -> Clauses
    setTrue _ [] = []
    setTrue l (x:xs)
        | (contains l x)     = setTrue l xs
        | ((contains (-l) x) && (length x == 1)) = [[-1]]
        | (contains (-l) x)  = (remove (-l) x):(setTrue l xs)


    setFalse :: Litteral -> Clauses -> Clauses
    setFalse _ [] = []
    setFalse l (x:xs)
        | ((contains l x) && (length x == 1)) = [[-1]]
        | (contains l x)     = (remove (l) x):(setFalse l xs)
        | (contains (-l) x)  = setFalse l xs 

    setLitteral :: Litteral -> Clauses -> Clauses
    setLitteral l clauses
            |(l > 0) = setTrue l clauses
            |(l < 0) = setFalse (-l) clauses

   
    unitPropagation :: Clauses -> IO(Clauses)

    unitPropagation [] = do
        c <- toIo (emptyClauses)
        return c

    unitPropagation c = do
        a <- toIo(findUnitaryList c)
        if a /= Nothing then
            do
                l <- toIo (justAToA a)
                if l > 0 then
                    do
                        c <- toIo (setTrue l c)
                        unitPropagation c
                else 
                    do
                        c <- toIo (setFalse (-l) c)
                        unitPropagation c
        else 
            do
                return c

    
    hornSat :: Solver
    hornSat c n m = do

        v <- toIo (findPositiveVar c)

        if v /= Nothing then
            do
                m <- toIo (set 1 m ((justAToA v)-1))
                c <- toIo (setTrue (justAToA v) c)
                if (emptyClause c) == True then
                    do
                        return [-1]
                else 
                    do
                        hornSat c n m
        else 
            do
                return m
         

    kromSat :: Solver
    kromSat c n m = do
        c <- unitPropagation c
        return m


    solve :: Formula -> IO Model
    solve f = do

        if (typeValid (typeFormula f) == True) then
            do
                s <- toIo (getSolver f)        
                m <- (s (clauses f) (nbVars f) (replicate (nbVars f) 0))
                return m
        else
            do
                m <- toIo([0])
                return m



    myFormula = getFormula "formula.fnc" "horn"
    unitaryFormula = getFormula "formula_unitary_clause.fnc" "horn"

    instance Show Formula where
        show f = "Formula"                             ++ "\n" ++  
                 "type      :: " ++ (typeFormula f)    ++ "\n" ++
                 "nbVars    :: " ++ show (nbVars f)    ++ "\n" ++
                 "nbClauses :: " ++ show (nbClauses f) ++ "\n" ++
                 "clauses   :: " ++ show (clauses f)   ++ "\n"

    