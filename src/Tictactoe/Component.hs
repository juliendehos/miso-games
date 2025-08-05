{-# LANGUAGE OverloadedStrings #-}

module Tictactoe.Component (mkComponent) where

import Miso

type Model = ()
type Action = ()

mkComponent :: Component m Model Action
mkComponent = 
  component () update view
  where
    update () = pure ()
    view _ =
      div_ [] [ "tictactoe component" ]

