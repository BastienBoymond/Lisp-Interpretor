cd test
success=0
failure=0
for file in $(find scm/ -name "*.scm")
do
    ../glados < $file > output1
    diff output1 res/$(basename $file)
    if [ $? -eq 0 ]
    then
        echo "Test $file passed"
        success=$((success+1))
    else
        echo "Test $file failed"
        failure=$((failure+1))
    fi
done

echo "Tests passed: $success"
echo "Tests failed: $failure"
exit $failure