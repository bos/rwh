{-- snippet BookInfo --}
data BookInfo = Book Int String [String]
                deriving (Show)
{-- /snippet BookInfo --}

{-- snippet BookReview --}
data BookReview = BookReview BookInfo Customer String
{-- /snippet BookReview --}

{-- snippet Customer --}
data Customer = Customer {
      customerID      :: Int
    , customerName    :: String
    , customerAddress :: [String]
    } deriving (Show)
{-- /snippet Customer --}

{-- snippet bookInfo --}
bookInfo = Book 31337 "Algebra of Programming"
           ["Richard Bird", "Oege de Moor"]
{-- /snippet bookInfo --}

{-- snippet customer1 --}
customer1 = Customer 271828 "J. Random Hacker"
            ["65535 Syntax Ct",
             "Milpitas, CA 95134",
             "USA"]
{-- /snippet customer1 --}

{-- snippet customer2 --}
customer2 = Customer {
              customerID = 271828
            , customerName = "Jane Q. Citizen"
            , customerAddress = ["1048576 Disk Drive",
                                 "Milpitas, CA 95134",
                                 "USA"]
            }
{-- /snippet customer2 --}

{-- snippet ShoppingCart --}
data ShoppingCart = ShoppingCart Customer [BookInfo]
                    deriving (Show)
{-- /snippet ShoppingCart --}

{-- snippet accessors --}
bookID      (Book id title authors) = id
bookTitle   (Book id title authors) = title
bookAuthors (Book id titla authors) = authors
{-- /snippet accessors --}

{-- snippet niceAccessors --}
nicerID      (Book id _     _      ) = id
nicerTitle   (Book _  title _      ) = title
nicerAuthors (Book _  _     authors) = authors
{-- /snippet niceAccessors --}
