int a;
int b = 3;
a = (3 + 2) * 2 / 2;
bool c = true;
bool d = c && (2 < b);
int e = -b + (-1);

print(a);
print(b);
print(c);
print(d);
print(e);

////////////////////
int i = 0;
int g = 0;
for(; i <= 4; ) {
	g += 2;
	i++;
}

for( i = 0; i <= 4; ++i) 
	g -= 2;

print(g);

///////////////////

i = 1;
a = 0;
while( i++ < 5 ) a += 2;

//////////////////
a = 0;
{
	a = 1;
}
print(a);


/////////////////
int f1() {
	a = 2;
}
print(a);
f1();
print(a);


////////////////
int f2() { a = 3; return a + 1; }
b = f2();
print(a);
print(b);


/*
a = 10000;
int f(int a, int b) { a = 0; return a + b; }
print(f(1,1));
print(a);
*/

///////////////
int z = 0;
int h() {
	print(z);
	int g() {
		z++;
	        h();
	}
	if ( z < 3 ) {
		g();
	}
}
h();


// Function by default return 0 if return statement not presented
int g() {}
if (g() == 0) 
	print(1);
else
	print(0);




