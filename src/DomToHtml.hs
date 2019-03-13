{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module DomToHtml (nodeToHtml) where

import ClassyPrelude
import Data.Char
import qualified Text.XML as X
import Text.XML (Name, nameLocalName, Node(..))

data PrintTree a = Node a [PrintTree a] a | Empty deriving (Eq,Show)

nodeToHtml :: Node -> Text
nodeToHtml = renderTree 2 . nodeToTree

renderTree :: Int -> PrintTree Text -> Text
renderTree n tree =
    intercalate "\n" (treeLines tree)
  where
    treeLines :: PrintTree Text -> [Text]
    treeLines (Node start nodes end) =
        let nested = filter
                (\x ->
                    case x of
                        Empty -> False
                        otherwise -> True)
                nodes
        in  if nested == []
                then
                    [start ++ end]
                else
                    let inner = join $ fmap treeLines nested :: [Text]
                        innerShifted = fmap (nSpaces n ++) inner
                    in  [start] ++ innerShifted ++ [end]

nSpaces 0 = ""
nSpaces !n = " " ++ nSpaces (n-1)

nodeToTree :: Node -> PrintTree Text
nodeToTree (NodeElement el) = elementToTree el
nodeToTree (NodeContent t) =
    if all isSpace t
        then Empty
        else Node t [] ""

elementToTree :: X.Element -> PrintTree Text
elementToTree (X.Element name attrs nodes) =
    Node
        ("<" ++ showName name ++ " " ++ attributesToText attrs ++ ">")
        (fmap nodeToTree nodes)
        ("</" ++ showName name ++ ">")

attributesToText :: Map Name Text -> Text
attributesToText m =
    intercalate
        ", "
        (fmap
            (\(name,text) ->
                showName name ++ "=\"" ++ text ++ "\"")
            xs)
  where
    xs = mapToList m

showName = nameLocalName
