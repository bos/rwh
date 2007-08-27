{-- snippet all --}
import Data.List
import qualified Data.Map as Map
import System.IO
import Text.Printf

{- | The primary piece of data this program will store.
   It represents the fields in a POSIX /etc/passwd file -}
data PasswdEntry = PasswdEntry {
    userName :: String,
    password :: String,
    uid :: Integer,
    gid :: Integer,
    gecos :: String,
    homeDir :: String,
    shell :: String}
    deriving (Eq, Ord)

{- | Define how we get data to a 'PasswdEntry'. -}
instance Show PasswdEntry where
    show pe = printf "%s:%s:%d:%d:%s:%s:%s" 
                (userName pe) (password pe) (uid pe) (gid pe)
                (gecos pe) (homeDir pe) (shell pe)

{- | Converting data back out of a 'PasswdEntry'. -}
instance Read PasswdEntry where
    readsPrec _ value =
        case split ':' value of
             [f1, f2, f3, f4, f5, f6, f7] ->
                 -- Generate a 'PasswdEntry' the shorthand way:
                 -- using the positional fields.  We use 'read' to convert
                 -- the numeric fields to Integers.
                 [(PasswdEntry f1 f2 (read f3) (read f4) f5 f6 f7, [])]
             x -> error $ "Invalid number of fields in input: " ++ show x
        where 
        {- | Takes a delimiter and a list.  Break up the last based on the
        -  delimeter. -}
        split :: Eq a => a -> [a] -> [[a]]

        -- If the input is empty, the result is a list of empty lists.
        split _ [] = [[]]
        split delim str =
            let -- Find the part of the list before delim and put it in
                -- "before".  The rest of the list, including the leading 
                -- delim, goes in "remainder".
                (before, remainder) = span (/= delim) str
                in
                before : case remainder of
                              [] -> []
                              x -> -- If there is more data to process,
                                   -- call split recursively to process it
                                   split delim (tail x)

-- Convenience aliases; we'll have two maps: one from UID to entries
-- and the other from username to entries
type UIDMap = Map.Map Integer PasswdEntry
type UserMap = Map.Map String PasswdEntry

{- | Converts input data to maps.  Returns UID and User maps. -}
inputToMaps :: String -> (UIDMap, UserMap)
inputToMaps inp =
    (uidmap, usermap)
    where
    uidmap = Map.fromList . map (\pe -> (uid pe, pe)) $ entries
    usermap = Map.fromList . 
              map (\pe -> (userName pe, pe)) $ entries
    -- Convert the input String to [PasswdEntry]
    entries = map read (lines inp)

{-- /snippet all --}
