set shell := ["powershell.exe", "-c"]

run: 
    cabal install --installdir="./build" --overwrite-polic=always; .\build\ray-tracing-haskell.exe > image.ppm +RTS -ls; 