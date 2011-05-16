{-# LANGUAGE FlexibleContexts #-}
import Groebner
import Ordering
import Monomial
import Polynomial

data XY = X | Y deriving (Eq, Ord, Show)

instance Enumerable XY where
    enumerate = [X, Y]

x, y :: Ord (Monomial XY o) => Polynomial Rational XY o
x = variable X
y = variable Y

ideal :: Ord (Monomial XY o) => [Polynomial Rational XY o]
ideal = [x ^ 10 + x ^ 9 * y ^ 2, y ^ 8 - x ^ 2 * y ^ 7]

basis :: Ord (Monomial XY o) => [Polynomial Rational XY o]
basis = groebner ideal

main = do putStrLn $ "Ideal: "
                       ++ show (ideal :: [Polynomial Rational XY Lex])
          putStrLn $ "Lex basis: "
                       ++ show (basis :: [Polynomial Rational XY Lex])
          putStrLn $ "Revlex basis: "
                       ++ show (basis :: [Polynomial Rational XY RevLex])
          putStrLn $ "Deglex basis: "
                       ++ show (basis :: [Polynomial Rational XY DegLex])
          putStrLn $ "Degrevlex basis: "
                       ++ show (basis :: [Polynomial Rational XY DegRevLex])