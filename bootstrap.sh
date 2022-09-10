#!/bin/bash 
if [ $# -lt 2 ]; then 
	echo 
	echo "Incorrect command-line argument"
	echo "Correct usage :"
	echo "./run.bash <puzzle name> <dictionary name>"
	exit 1
else
# Retrieve the puzzle name from the input argument.
	puzname=$(basename $1)	
fi
# Delete the left over files from previous run.
rm $puzname.* temp *.enc

# Cleanly build the project.
echo "Cleanup and build the project"
echo
echo "\$ocamlbuild -clean"
echo "\$ocamlbuild -libs str encode.byte"
ocamlbuild -clean
ocamlbuild -libs str encode.byte
if [ $? -ne 0 ]; then
	echo "Failed to build the project. Exitting ..."
	exit 1
fi
echo "----------------------------------"

echo "Encode the puzzle using input directory."
echo
echo "./encode.byte $1 $2"
time ./encode.byte $1 $2
if [ $? -ne 0 ]; then
	echo "Failed ecnode the puzzle using input directory. Exitting ..."
	exit 1
fi
echo "----------------------------------"

echo "Convert the puzzle encoding to DIMACS format."
echo "perl ../data/convertToDIMACS $puzname.enc > $puzname.cnf"
time perl ../data/convertToDIMACS $puzname.enc > $puzname.cnf
if [ $? -ne 0 ]; then
	echo "Failed to convert the puzzle encoding to DIMACS format. Exitting ..."
	exit 1
fi
echo "----------------------------------"

echo "Solve the SAT encoded puzzle."
echo "./zchaff $puzname.cnf > $puzname.output"
time ./zchaff $puzname.cnf > $puzname.output
if [ $? -ne 0 ]; then
	echo "Failed to solve the SAT encoded puzzle. Exitting ..."
	exit 1
fi
echo "----------------------------------"

echo "Convert the Zchaff output from DIMACS format to regular format."
echo "../data/readOutput $puzname.output $puzname.key > $puzname.ans"
time perl ../data/readOutput $puzname.output $puzname.key > $puzname.ans
if [ $? -ne 0 ]; then
	echo "Failed to convert the zchaff output from DIMACS format to regulare format. Exitting..."
	exit 1
fi
echo "----------------------------------"
