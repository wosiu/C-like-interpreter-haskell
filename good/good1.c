int a;
int b = 3;
a = (3 + 2) * 2 / 2;
bool c = true;
bool d = c && (2 < b);
int e; 
bool e = !c; // it is allowed to cover variable in the same scope
int f = -b + (-1);


int i = 0;
int g = 0;
for(i = 1; i <= 4; i++) {
	g += 2;
}

for(i = 1; i <= 4; i++) 
	g -= 2;

int i = 1;
int a = 0;
while( i++ < 5 ) a += 2;


int a = 0;

{
	a = 2;
	int i;
}


int a = 1;
int f() {
	a = 2;
}
print(a);


int a = 1;
int f() { a = 2; return 3; }
int b = f();


int a = 1;    
int f() { int a = 2; print(a); f(); }
f();


int a = 10000;
int f(int a, int b) { a = 0; return a + b; }
print(f(1,1));
print(a);



int z = 0;
int f() {
	print(z);
	int g() {
		z++;
	        f();
	}
	if ( z < 3 ) {
		g();
	}
}
f();


// Function by default return 0 if return statement not presented
int g() {}
if (g() == 0) 
	print(1)
else
	print(0)




