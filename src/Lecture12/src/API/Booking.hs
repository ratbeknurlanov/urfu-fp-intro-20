module API.Booking where

import Servant.API
import DB.Booking

type BookingAPI
  = "api" :> "checkout" :> Capture "id" BookingId :> Get '[JSON] BookingResponse
  :<|>
    ("api" :> "refund" :> Capture "id" BookingId :> Get '[JSON] ())