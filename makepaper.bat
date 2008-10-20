if not exist obj mkdir obj
ghc --make Paper.hs -o paper -odir obj -hidir obj
