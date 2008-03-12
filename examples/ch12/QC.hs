import Prettify -- needs to export constructors to be testable
import Test.QuickCheck

instance Arbitrary Char where
    arbitrary = oneof $ map return $ ['A'..'Z'] ++ ['a' .. 'z'] ++ " ~!@#$%^&*()"
  --  arbitrary = chr `fmap` choose (0,255)
    coarbitrary = undefined

-- TODO, probabilities for Empty.
instance Arbitrary Doc where
    arbitrary = oneof [ return Empty
                      , do x <- arbitrary
                           return (Char x)
                      , do x <- arbitrary
                           return (Text x)
                      , return Line
                      , do x <- arbitrary
                           y <- arbitrary
                           return (Concat x y)
               --     , do x <- arbitrary
               --          y <- arbitrary
               --          return (Union x y)
                      ]

    coarbitrary = undefined

-- nice properties
prop_empty_id x =
    Empty <> x == x
  &&
    x <> Empty == x

prop_text_id x =
    pretty 100 (Text x) == x

prop_append x y =
    pretty n (Text x <> Text y) == x ++ y
  where
    n = 100

-- test monoid laws.

--
-- False, due to Union case.
--
prop_concat_append x y =
    pretty n (x <> y) == pretty n x ++ pretty n y
  where
    n = 100

{-

    the left of each Union is always the same width as, or wider than, the right.

-}

{-

Falsifiable, after 25 tests:
Concat (Char 'p') (Union Empty (Text "r"))
Union (Text "huM") (Text "H~mHrCX")

-}


{-
*Main Test.QuickCheck> quickCheck prop_concat_append
Falsifiable, after 15 tests:
Union (Text "") (Union (Concat (Text "") (Char 'o')) (Concat Empty Line))
Char 'v'
-}

{-
    *Main Test.QuickCheck> quickCheck prop_empty_id
    OK, passed 100 tests.
-}

{-
    Verbose check:

    Concat (Concat (Text "rfpqnotlwktrkxnivorky xc") (Char 'k')) (Union Empty (Text "vcozhnqeupgomkv zqugfygo"))
    88:
    Empty
    89:
    Text "hyyx"
    90:
    Line
    91:
    Char 'c'
    92:
    Text "ntgv s yytub"
    93:
    Char 'q'
    94:
    Text "kapxyqlnzzcdqupa"
    95:
    Concat (Union (Union Empty Line) (Char 'h')) (Char 'o')
    96:
    Concat (Concat (Char 'b') (Text "mavhza eybzgbrpnuryahr")) (Union (Text "aamwokb") (Text "ytt pqhf"))
    97:
    Char 'y'
    98:
    Char 'w'
    99:
    Text ""
    OK, passed 100 tests.

-}
