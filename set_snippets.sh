#!/bin/bash 
# elpa/elpy-20220627.1416/
FILPA=$(pwd)
cd elpa
ELPY=$(ls -d elpy*)
LONGPA=${ELPY}/snippets/python-mode/
cd ..
for file in snippets/*; do ln -s "$(pwd)/${file}" "${FILPA}/elpa/${LONGPA}"; done
