#!/bin/bash

verbose=false
tests=`ls input/`

if [ "$1" == "-g" ]; then
	verbose=true
fi

for test in ${tests[@]}
do
	echo "---- Zacinam test: "$test" "
	input="input/"${test}
	./dka-2-mka -t input/$test
done


