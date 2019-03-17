python -m unittest discover --start-directory ../funit

mkdir -p outputs
for file in simple beforeAfter TestA TestCaseA MpiTestCaseB ParameterizedTestCaseB MpiParameterizedTestCaseC
do
   ../funitproc inputs/${file}.pf outputs/${file}.F90
   diff outputs/${file}.F90 expectedOutputs/${file}.F90
done
