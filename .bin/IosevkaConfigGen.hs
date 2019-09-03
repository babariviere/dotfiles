{-# LANGUAGE RecordWildCards, Arrows #-}

import Numeric
import Data.Char

import Control.Monad
import Data.Monoid ((<>))
import Data.List (nub, sort, reverse)

data RepeatBounds = RB
  { str :: String
  , min :: Int
  , max :: Int
  } deriving Show

repeatFromTo s (min,max) = map (\n -> concat $ replicate n s) [min .. max]

tripleOp left mid right = [left <> mid, left <> mid <> right, mid <> right]

extended xs = map concat $ mapM renderGroup xs
  where renderGroup RB{..} = repeatFromTo str (min,max)

reverseArrow = reverse . map rev
  where
    rev xs =
      case xs of
        '>' -> '<'
        '<' -> '>'
        x -> x

angleArrows = rights <> map reverseArrow rights
  where
    rights = nub $
      [rgLeftHyphen, rgLeftHyphen', rgLeftEquals, rgLeftEquals'] >>= extended
    rgLeftHyphen = [RB "-" 1 2, RB ">" 1 3, RB "-" 0 2]
    rgLeftHyphen' = [RB "-" 0 2, RB ">" 1 3, RB "-" 1 2]
    rgLeftEquals = [RB "=" 1 2, RB ">" 1 3, RB "=" 0 2]
    rgLeftEquals' = [RB "=" 0 2, RB ">" 1 3, RB "=" 1 2]
    rgLeftTilde = [RB "~" 1 2, RB ">" 1 3, RB "-" 0 2]
    rgLeftTilde' = [RB "~" 1 2, RB ">" 1 3, RB "-" 0 0]

doubleEnded left mid right min max = do
  body <- mid `repeatFromTo` (min,max)
  pure (left ++ body ++ right)

rightEnded right mid = doubleEnded "" mid right
leftEnded left mid = doubleEnded left mid ""

hyphenDouble = doubleEnded "<" "-" ">" 1 5
equalsDouble = doubleEnded "<" "=" ">" 1 5
asteriskDouble = doubleEnded "<" "*" ">" 2 5

htmlCommentStart = leftEnded "<!" "-" 2 3

colons = repeatFromTo ":" (2,4)
dots = repeatFromTo "." (2,4)

equals = ["==", "!=", "===", "!=="]

tripleOps =
  concatMap
    (\mid -> tripleOp "<" mid ">")
    ["$", ".", "*", "\\", "/", "\"", "'", "^", "&", "%", "@", "#", "+", "-", "!", "?", "|", ":"]

lensOps =
  concat
    [ prefix' ":" "+-="
    , postfix' ":" "+-="
    , prefix' "=" "^+-*/%"
    , postfix' "=" "^+-*/%"
    ]
  where
    prefix str = map (str ++)
    prefix' str = prefix str . lift
    postfix str = map (++ str)
    postfix' str = postfix str . lift
    lift = map (: [])

addReversed xs = nub $ xs <> map reverseArrow xs

monadics = addReversed [">=>", ">->",">-->",">==>"]
composition = addReversed [">>", ">>>"]

logic = ["/\\", "\\/"]

semigroups = ["<>"] <> tripleOp "<" "+" ">"

allSeqs =
    [ ("Double-ended hyphen arrows", hyphenDouble)
    , ("Double-ended equals arrows",equalsDouble)
    , ("Double-ended asterisk operators",asteriskDouble)
    , ("HTML comments",htmlCommentStart)
    , ("Three-char ops with discards",tripleOps)
    , ("Colons",colons)
    , ("Equals",equals)
    , ("Arrow-like operators",angleArrows)
    , ("Monadic operators", monadics)
    , ("Composition operators", composition)
    , ("Lens operators",lensOps)
    , ("Logical", logic)
    , ("Semigroup/monoid operators", semigroups)
    ]

toToml = foldM_ go puaStart allSeqs
  where
    go start (sectionHeader, ligs) = do
      putStrLn "# -----------------------------------------"
      putStrLn $ "# " <> sectionHeader
      putStrLn "# -----------------------------------------\n"
      forM_ (enumerate start ligs) $ \(ix, lig) -> do
        putStrLn "[[iosevka.compLig]]"
        putStrLn $ "unicode = " <> show ix <> " # " <> toHex' "0x" ix
        putStrLn "featureTag = 'XHS0'"
        putStrLn $ "sequence = " <> show lig
        putStrLn ""
      return $ start + length ligs

toElisp = foldM_ go puaStart allSeqs
  where
    go start (sectionHeader, ligs) = do
      putStrLn $ "\n;; " <> sectionHeader <> " ----------------"
      forM_ (enumerate start ligs) $ \(ix, lig) ->
        putStrLn $ "(" <> show lig <> " . " <> toHex' "#X" ix <> ")"
      return $ start + length ligs

enumerate start = zip [start ..]
puaStart = 0xE100
toHex n = showHex n ""
toHex' prefix = (prefix <>) . toHex
