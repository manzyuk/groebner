{-# LANGUAGE FlexibleContexts #-}
import Groebner
import Ordering
import Monomial
import Polynomial

data XY = X | Y deriving (Eq, Ord, Show)

instance Enumerable XY where
    enumerate = [X, Y]

x, y :: Ord (Monomial o XY) => Polynomial Rational o XY
x = variable X
y = variable Y

ideal :: Ord (Monomial o XY) => [Polynomial Rational o XY]
ideal = [x ^ 10 + x ^ 9 * y ^ 2, y ^ 8 - x ^ 2 * y ^ 7]

basis :: Ord (Monomial o XY) => [Polynomial Rational o XY]
basis = groebner ideal

main = do putStrLn $ "Ideal: "
                       ++ show (ideal :: [Polynomial Rational Lex XY])
          putStrLn $ "Lex basis: "
                       ++ show (basis :: [Polynomial Rational Lex XY])
          putStrLn $ "Revlex basis: "
                       ++ show (basis :: [Polynomial Rational RevLex XY])
          putStrLn $ "Deglex basis: "
                       ++ show (basis :: [Polynomial Rational DegLex XY])
          putStrLn $ "Degrevlex basis: "
                       ++ show (basis :: [Polynomial Rational DegRevLex XY])