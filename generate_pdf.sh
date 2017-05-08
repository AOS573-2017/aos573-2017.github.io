#!/bin/bash

for file in _lectures/*.md _assignments/*.md
do
pandoc -V geometry:margin=1in ${file} -o ${file}.pdf
done
