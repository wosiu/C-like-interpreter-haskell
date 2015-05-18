echo "SUCCESS EXPECTED:"
for file in good/good*.c
do
	echo $file ":" 
	./interpreter $file 
done

echo "FAIL EXPECTED:"
for file in bad/bad*.c
do
	echo $file ":"
	./interpreter $file 
done
