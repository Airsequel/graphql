{-# LANGUAGE OverloadedStrings #-}

module Language.GraphQL.Type.Directive
    ( selection
    ) where

import qualified Data.HashMap.Strict as HashMap
import Language.GraphQL.AST.Core

-- | Directive processing status.
data Status
    = Skip -- ^ Skip the selection and stop directive processing
    | Include Directive -- ^ The directive was processed, try other handlers
    | Continue Directive -- ^ Directive handler mismatch, try other handlers

-- | Takes a list of directives, handles supported directives and excludes them
--   from the result. If the selection should be skipped, returns 'Nothing'.
selection :: [Directive] -> Maybe [Directive]
selection = foldr go (Just [])
  where
    go directive' directives' =
        case (skip . include) (Continue directive') of
            (Include _) -> directives'
            Skip -> Nothing
            (Continue x) -> (x :) <$> directives'

handle :: (Directive -> Status) -> Status -> Status
handle _ Skip = Skip
handle handler (Continue directive) = handler directive
handle handler (Include directive) = handler directive

-- * Directive implementations

skip :: Status -> Status
skip = handle skip'
  where
    skip' directive'@(Directive "skip" (Arguments arguments)) =
        case HashMap.lookup "if" arguments of
            (Just (Boolean True)) -> Skip
            _ -> Include directive'
    skip' directive' = Continue directive'

include :: Status -> Status
include = handle include'
  where
    include' directive'@(Directive "include" (Arguments arguments)) =
        case HashMap.lookup "if" arguments of
            (Just (Boolean True)) -> Include directive'
            _ -> Skip
    include' directive' = Continue directive'
