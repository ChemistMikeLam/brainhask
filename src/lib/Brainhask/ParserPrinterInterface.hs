{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{- |
Module: Brainhask.ParserPrinterInterface
Description:
    Interface for parsing and pretty-printing Brainfuck code.
Copyright:
    (c) 2024 ChemistMikeLam
License: MPL-2.0
Maintainer:
    43129403+ChemistMikeLam@users.noreply.github.com
Stability:
    incomplete, experimental
portability:
    portable

Typeclass specifying the interface for parsing and pretty-printing Brainfuck code.
-}
module Brainhask.ParserPrinterInterface
    ( ParserPrinterInterface (..)
    ) where

import Brainhask.Code
    ( Code
    )

import Brainhask.ErrorInterface
    ( ErrorInterface
    , ErrorRepr
    )

import Data.Kind
    ( Type
    )

import Data.String
    ( IsString
    )

{- |
Typeclass specifying the interface for parsing and pretty-printing Brainfuck code.

The type of string is implementation-dependent.
However, it would always be an instance of @'IsString'@, so converting to other string types should be easy.

Invariants:

1. For all @'StringRepr' p@,

    prop> parseWithComment = parseNoComment . stripComment

2. and (idempotence of @'stripComment'@)

    prop> stripComment . stripComment = stripComment

3. If there is no parse error, then

    prop> fmap prettyPrint . parseNoComment = Right

4. and (this follows from 1 and 3)

    prop> fmap prettyPrint . parseWithComment = Right . stripComment

5. For any valid @['Code']@,

    prop> parseNoComment . prettyPrint = Right

6. and

    prop> parseWithComment . prettyPrint = Right
-}
class
    ( IsString (StringRepr p)
    ) =>
    ParserPrinterInterface (p :: Type)
    where
    -- | The type of string to be parsed from / printed to. Must be instance of @'IsString'@.
    data StringRepr p :: Type

    -- | Strips all comment caharacters, leaving only the 8 active characters (@+.<>.,[]@). Idempotent.
    stripComment
        :: StringRepr p
        -- ^ Input string, possibly with comment(s)
        -> StringRepr p
        -- ^ Output string, stripped of all comments

    {- |
    Parses a string with no comments into @['Code']@.
    Gives @'Brainhask.ErrorInterface.parseError'@ if the string has comments or unbalanced square brackets.

    To avoid unnecessary parse errors, use @'parseWithComment'@ unless the string is provably free of comments.
    -}
    parseNoComment
        :: ( ErrorInterface e
           )
        => StringRepr p
        -- ^ Input string, assumed to be comment-free
        -> Either (ErrorRepr e) [Code]
        -- ^ Either parse error or the parsed list of code

    {- |
    Parses a string, possibly with comments, into @['Code']@.
    Gives @'Brainhask.ErrorInterface.parseError'@ only if there is unbalanced square brackets.

    Default implementation is just @'parseNoComment' . 'stripComment'@.
    Instances might want to redefine to increase efficiency (eliminate 1 pass over the string).
    If redefined, care must be paid to abide by class invariants.
    -}
    parseWithComment
        :: ( ErrorInterface e
           )
        => StringRepr p
        -- ^ Input string, possibly with comment(s)
        -> Either (ErrorRepr e) [Code]
        -- ^ Either parse error or the parsed list of code
    parseWithComment = parseNoComment . stripComment

    -- | Pretty-prints @['Code']@ into a string. Reverse of the parse functions.
    prettyPrint
        :: [Code]
        -- ^ Input code
        -> StringRepr p
        -- ^ Pretty-printed code string

    {-# MINIMAL
        stripComment
        , parseNoComment
        , prettyPrint
        #-}
