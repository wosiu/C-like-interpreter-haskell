for i in `seq 1 11`
do
	echo $i ":" `./parser/Testdeklaracja ./sample-correct/sample*.c | grep -e "Parse Successful!" -e "Failed..."`
done

