for i in examples/*/*.c
do
    gcc -w $i         #compile with gcc
    ./a.out                #run it
    expected=$?            #get exit code
    ./hcc $i               #compile with hcc
    base="${i%.*}"
    $base                  #run the thing we assembled
    actual=$?              #get exit code
    echo -n "$i:    "
    if [ "$expected" -ne "$actual" ]
    then
        echo "FAIL"
    else
        echo "OK"
    fi
    rm $base
done

#cleanup
rm a.out
