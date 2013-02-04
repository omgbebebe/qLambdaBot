module QBotNetwork where

import Text.JSON

{-
getAll object JSON command
request -> {"cmd": "getAllUnits", "params": ""}
response -> [{"arb_id": 1, "arb_pos": [1,1,0], "arb_health": 100}
            ,{"arb_id": 2, "arb_pos": [2,2,0], "arb_health": 110}
            , ...
            ,{"arb_id": n, "arb_pos": [x,y,z], "arb_health": h}
            ]
-}

data Command = UnknownCommand
             | GetAllUnits
             | GetGameVersion
             | GetApiVersion
             deriving (Show)
                      
instance JSON Command where
  readJSON object = do obj    <- readJSON object
                       c      <- valFromObj "cmd" obj :: Result String
--                       p      <- valFromObj "params" obj :: Result String
                       let v = case c of
                             "getAllUnits" -> GetAllUnits
                             "getApiVersion" -> GetApiVersion
                             "getGameVersion" -> GetGameVersion
                             _ -> UnknownCommand
                       return v
                       
  showJSON GetAllUnits      = makeObj [("cmd", showJSON "getAllUnits")]
  showJSON GetApiVersion    = makeObj [("cmd", showJSON "getApiVersion")]
  showJSON GetGameVersion   = makeObj [("cmd", showJSON "getGameVersion")]

data Request = GameVersion
             | NetworkVersion
             | Players
             | AllUnits
             | MapName
             deriving (Show, Eq, Enum)

data Action = ARB_Build
            | ARB_Research
            | ARB_Attack
            | ARB_Move
             deriving (Show, Eq, Enum)

type Position = (Int, Int, Int)

data Unit = Unit{_id       :: Int
                ,_pos      :: Position
                ,_health   :: Int
                }
          deriving (Show)
instance Eq Unit where
  (==) u u' = (==) (_id u) (_id u')
  
instance JSON Unit where
  readJSON object = do obj <- readJSON object
                       i <- valFromObj "arb_id" obj
                       p <- valFromObj "arb_pos" obj
                       h <- valFromObj "arb_health" obj
                       return (Unit{_id=i,_pos=p,_health=h})
                       
  showJSON (Unit i p h) = makeObj [("arb_id",showJSON i)
                                  ,("arb_pos",showJSON p)
                                  ,("arb_health",showJSON h)
                                  ]

idUnit = Unit{_id=(-1),_pos=((-1),(-1),(-1)),_health=(-1)}