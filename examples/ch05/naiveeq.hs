{-- snippet color --}
data Color = Red | Green | Blue

colorEq :: Color -> Color -> Bool
colorEq Red Red = True
colorEq Red _ = False
colorEq Green Green = True
colorEq Green _ = False
colorEq Blue Blue = True
colorEq Blue _ = False
{-- /snippet color --}

