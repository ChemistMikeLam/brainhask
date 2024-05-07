{-# LANGUAGE GADTSyntax #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{- |
Module: Brainhask.Code
Description:
    Datatypes for representing Brainfuck code.
Copyright:
    (c) 2024 ChemistMikeLam
License: MPL-2.0
Maintainer:
    43129403+ChemistMikeLam@users.noreply.github.com
Stability:
    incomplete, experimental
portability:
    portable

Datatypes for representing Brainfuck code.
-}
module Brainhask.Code
    ( Code (..)
    , SingleCode (..)
    ) where

{- |
Datatype for representing Brainfuck code.
Each constructor represents 1 code unit.
-}
data Code where
    -- | Wraps a single character of Brainfuck code
    Single
        :: {-# UNPACK #-} !SingleCode
        -- ^ The character of Brainfuck code
        -> Code
        -- ^ Wrapped code unit

    -- | Wraps the character pair @\'[\'@ and @\']\'@, and everything in between
    Loop
        :: {-# UNPACK #-} ![Code]
        -- ^ List of all codes in between the paired square brackets
        -> Code
        -- ^ Wrapped into one code unit

-- | @since 0.1.0.0
deriving stock instance Eq Code

{- |
Derives @'Show'@ to ease debugging in GHCi.

Do not use this instance to pretty-print Brainfuck code.
Instead, use @'Brainhask.ParserPrinterInterface.prettyPrint'@.

@since 0.1.0.0
-}
deriving stock instance Show Code

{- |
Datatype for representing a single character of Brainfuck code.
-}
data SingleCode where
    -- | Represents character @\'+\'@
    Inc :: SingleCode
    -- | Represents character @\'-\'@
    Dec :: SingleCode
    -- | Represents character @\'\<\'@
    Lsh :: SingleCode
    -- | Represents character @\'\>\'@
    Rsh :: SingleCode
    -- | Represents character @\',\'@
    Inp :: SingleCode
    -- | Represents character @\'.\'@
    Out :: SingleCode

-- | @since 0.1.0.0
deriving stock instance Enum SingleCode

-- | @since 0.1.0.0
deriving stock instance Bounded SingleCode

-- | @since 0.1.0.0
deriving stock instance Eq SingleCode

{- |
Derives @'Show'@ to ease debugging in GHCi.

Do not use this instance to pretty-print Brainfuck code.
Instead, use @'Brainhask.ParserPrinterInterface.prettyPrint'@.

@since 0.1.0.0
-}
deriving stock instance Show SingleCode
