type Vector = (Double, Double)

{-- snippet Shape --}
data Shape = Circle Vector Double
           | Poly [Vector]
{-- /snippet Shape --}
