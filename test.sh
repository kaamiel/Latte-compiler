#!/bin/bash

failed=0
total=0

make
if [ $? -ne 0 ]
then
    echo -e "\033[1;31mmake failed\033[0m"
    exit 1
fi

mkdir -p test-outputs

for f in ./tests/bad/*.lat
do
    ((++total))
    fff=${f##*/}
    echo -e "\n\n\033[1;34m$f\033[0m"
    if [ "$1" == "-v" ]
    then
        echo "========================================"
        cat $f
        echo "========================================"
    fi

    ./latc_llvm $f
    if [ $? -ne 0 ]
    then
        echo -e "\033[1;32m$fff ok\033[0m"
    else
        echo -e "\033[1;31m$fff failed\033[0m"
        ((++failed))
    fi
done

for f in ./tests/good/*.lat
do
    ((++total))
    ff=${f%.lat}
    fff=${f##*/}
    echo -e "\n\n\033[1;34m$f\033[0m"
    if [ "$1" == "-v" ]
    then
        echo "========================================"
        cat $f
        echo "========================================"
    fi


    ./latc_llvm $f
    if [ $? -eq 0 ]
    then
        if [ -f "${ff}.input" ]
        then
            lli "${ff}.bc" < "${ff}.input" > "./test-outputs/${fff}.out"
        else
            lli "${ff}.bc" > "./test-outputs/${fff}.out"
        fi

        if [ $? -eq 0 ] && diff "${ff}.output" "./test-outputs/${fff}.out" > /dev/null 2>&1
        then
            echo -e "\033[1;32m$fff ok\033[0m"
        else
            echo -e "\033[1;31m$fff failed\033[0m"
            ((++failed))
        fi
    else
        echo -e "\033[1;31m$fff failed\033[0m"
        ((++failed))
    fi
done

echo -e "\n\nfailed $failed / $total tests\n"
