echo "SUCCESS EXPECTED:"
for file in sample-correct/sample*.c
do
	echo $file ":" `./parser/Testdeklaracja $file | grep -e "Parse Successful!" -e "Failed..."`
done

echo "FAIL EXPECTED:"
for file in sample-incorrect/sample*.c
do
	echo $file ":" `./parser/Testdeklaracja $file | grep -e "Parse Successful!" -e "Failed..."`
done
