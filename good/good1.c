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


////////////
a = 2;

switch (a) {
	case 1: print(1); break;
	case 2: print(2); break;
	case 3: print(3); break;
	default: print(4);
}

////////////
string str1 = "asd";
string str2;
str2 = str1 + "a";
print(str1 == str2);
print(str1);


//////////////
int tab1[] = {1, 2, 3};
print(tab1[1]);

int tab2[3];
tab2[0] = 1;
print(tab2[0]);
print(tab2[1]);
