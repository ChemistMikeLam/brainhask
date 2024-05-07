{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{- |
Module: Brainhask.ErrorInterface
Description:
    Interface for errors in this package.
Copyright:
    (c) 2024 ChemistMikeLam
License: MPL-2.0
Maintainer:
    43129403+ChemistMikeLam@users.noreply.github.com
Stability:
    incomplete, experimental
portability:
    portable

Typeclass specifying the interface for error reporting in this package.
-}
module Brainhask.ErrorInterface
    ( ErrorInterface (..)
    ) where

import Data.Kind
    ( Type
    )

import Control.Exception
    ( Exception
    )

{- |
Typeclass specifying the interface for error reporting in this package.

The concrete representation for the errors is an implementation detail and should not be depended on.
However, the concrete representation should be an instance of @'Exception'@ class.

Functions in this class takes in error messages of type @'Maybe' 'String'@, and return a concrete error object.

Invariant: if @'Just' s@ is passed to the functions, returning error object @e@, then

> s `isSubsequenceOf` show e == True
-}
class
    ( Exception (ErrorRepr e)
    ) =>
    ErrorInterface (e :: Type)
    where
    -- | The concrete representation for the errors.
    data ErrorRepr e :: Type

    {- |
    Signalling parse errors.
    Usually thrown by instances of @'Brainhask.ParserPrinterInterface.ParserPrinterInterface'@.

    Unbalanced square brackets are the only syntax errors in Brainhask.
    However, parser implementations might throw this error for other situations.
    -}
    parseError
        :: Maybe String
        -- ^ Possibly empty custom error message
        -> ErrorRepr e
        -- ^ Returned error object

    {- |
    Signalling error when trying to obtain input from empty input buffer.
    Usually thrown by instances of @'Brainhask.RunnerInterface.RunnerInterface'@.
    -}
    emptyInputError
        :: Maybe String
        -- ^ Possibly empty custom error message
        -> ErrorRepr e
        -- ^ Returned error object

    {- |
    Signalling error when tyring to interpret empty code.
    Usually thrown by instances of @'Brainhask.RunnerInterface.RunnerInterface'@.
    -}
    emptyCodeError
        :: Maybe String
        -- ^ Possibly empty custom error message
        -> ErrorRepr e
        -- ^ Returned error object

