{-- snippet custom --}
data CustomColor = CustomColor Int Int Int
        deriving (Eq, Show, Read)
{-- /snippet custom --}

{-- snippet extract --}
color2string :: CustomColor -> String
color2string (CustomColor red green blue) =
    "red: " ++ show red ++ ", green: " ++ show green ++ ", blue: "
    ++ show blue
{-- /snippet extract --}
