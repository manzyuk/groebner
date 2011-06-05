{-# LANGUAGE FlexibleContexts, TypeOperators, TemplateHaskell #-}
import Groebner
import Ordering
import Monomial
import Polynomial
import Types
import Variable

$(defineVariables ["X", "Y"])

ideal :: Ord (Monomial (X :<: Y) o) => [Polynomial Rational (X :<: Y) o]
ideal = [x ^ 10 + x ^ 9 * y ^ 2, y ^ 8 - x ^ 2 * y ^ 7]

basis :: Ord (Monomial (X :<: Y) o) => [Polynomial Rational (X :<: Y) o]
basis = groebner ideal

main = putStr . unlines $
       [ "Ideal:"
       , ppr (ideal :: [Polynomial Rational (X :<: Y) Lex])
       , "Lex basis:"
       , ppr (basis :: [Polynomial Rational (X :<: Y) Lex])
       , "Revlex basis:"
       , ppr (basis :: [Polynomial Rational (X :<: Y) RevLex])
       , "Deglex basis:"
       , ppr (basis :: [Polynomial Rational (X :<: Y) DegLex])
       , "Degrevlex basis:"
       , ppr (basis :: [Polynomial Rational (X :<: Y) DegRevLex])
       , "(Lex, Lex) basis:"
       , ppr (basis :: [Polynomial Rational (X :<: Y) (Lex, Lex)])
       ]
    where
      ppr :: Ord (Monomial (X :<: Y) o)
          => [Polynomial Rational (X :<: Y) o] -> String
      ppr = unlines . map (("  " ++) . show)