import Html.App as Html

import Model
import View

main =
  Html.program
      { init = Model.init
      , view = View.view
      , update = Model.update
      , subscriptions = Model.subscriptions
      }
