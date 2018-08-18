#!/bin/sh

python -m unittest discover

TESTS=$(find inputs -name '*.pf' -print)
mkdir -p outputs
for file in $TESTS
do
   name=$(basename $file | sed 's/\..*$//')

   ../pFUnitParser.py ${file} outputs/${name}.F90
   diff outputs/${name}.F90 expectedOutputs/${name}.F90
done
