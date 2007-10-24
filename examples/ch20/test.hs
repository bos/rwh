import RunProcess

main = do
    runIO $ (("/tmp/foo.sh", [""]) -|- ("grep", ["^s"])) -|- ("cat", ["-"])
    --runIO $ ("/tmp/foo.sh", [""]) -|- ("grep", ["^s"]) -|- ("wc", ["-l"])
    --runIO $ ("/tmp/foo.sh", [""]) -|- ("grep", ["^s"])
    --runIO $ ("ls", ["/usr/bin"]) -|- ("grep", ["^s"])
