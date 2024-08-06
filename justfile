set shell := ["powershell.exe", "-c"]

run: 
    cabal run --verbose=0 > image.ppm; 


measure: 
    Measure-Command { cabal run --verbose=0 > image.ppm }