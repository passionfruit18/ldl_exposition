#!/bin/bash

cp -f ../src/* .
ghc --make LDLInterpreter -main-is LDLInterpreter -o ldli
./ldli test.ldl