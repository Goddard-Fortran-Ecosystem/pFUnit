export PYTHONPATH=..:$PYTHONPATH
python -m unittest discover ../funit/tests

mkdir -p outputs
for file in simple beforeAfter TestA TestCaseA MpiTestCaseB ParameterizedTestCaseB MpiParameterizedTestCaseC
do
   ../fuproc inputs/${file}.pf outputs/${file}.F90
   diff outputs/${file}.F90 expectedOutputs/${file}.F90
done
