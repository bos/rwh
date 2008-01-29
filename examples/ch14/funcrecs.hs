{-- snippet all --}
data CustomColor =
  CustomColor {red :: Int,
               green :: Int,
               blue :: Int}
  deriving (Eq, Show, Read)

data FuncRec =
    FuncRec {name :: String,
             colorCalc :: Int -> (CustomColor, Int)}

plus5func color x = (color, x + 5)
purple = CustomColor 255 0 255

plus5 = FuncRec {name = "plus5", colorCalc = plus5func purple}
always0 = FuncRec {name = "always0", colorCalc = \_ -> (purple, 0)}
{-- /snippet all --}

