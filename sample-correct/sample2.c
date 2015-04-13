int main() {
	int i = 0;
	for(i = 0;i<5;i++) {
	}
	for(i = 0;;) {
		break;
	}
	for(;i<5;) {
		break;
	}
	for(i = 0;i<5;) {
		break;
	}
	for(;;i++) {
		break;
	}
	for(i=0;;i++) {
		break;
	}
	for(;i<10;i++) {
		continue;
	}
	for(;;) {
		break;
	}
	while ( i<20 ) {
		i+=2;	
	}

	return 0;
}
