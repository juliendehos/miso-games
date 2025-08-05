{-# LANGUAGE OverloadedStrings #-}

module App.View where

import Miso

import App.Model
import App.Update

viewModel :: Model -> View Model Action
viewModel model = 
  div_ [] 
    [ h1_ [] [ "miso-games" ]
    , p_ [] [ "TODO" ]
    ]

