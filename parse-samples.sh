echo "SUCCESS EXPECTED:\n"
for i in `seq 1 13`
do
	echo $i ":" `./parser/Testdeklaracja ./sample-correct/sample$i.c | grep -e "Parse Successful!" -e "Failed..."`
done

echo "FAIL EXPECTED:\n"
for i in `seq 1 10`
do
	echo $i ":" `./parser/Testdeklaracja ./sample-incorrect/sample$i.c | grep -e "Parse Successful!" -e "Failed..."`
done
