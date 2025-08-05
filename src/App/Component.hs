
module App.Component where

import Miso

import App.Model
import App.Update
import App.View

mkComponent :: Model -> App Model Action
mkComponent model = 
  (component model updateModel viewModel) 
    { events = defaultEvents <> pointerEvents
    }

