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

////////////////
int f3(int param) { return param; }
print(f3(2));

/*	
a = 10000;
int f(int a, int b) { a = 0; return a + b; }
print(f(1,1));
print(a);
*/

///////////////
int infRec() {
	infRec();
}


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

/////////////
auto a1 = 2;
auto a2 = false;
auto a3 = "asd";
print(a1);
print(a2);
print(a3);

auto ar2 = {1, 2, 3};

/////////////
auto fauto1() {
	return true;
}
print(fauto1());

auto fauto2() {}
print(fauto2()); //void functions by default returns string VOID

/////////////
bool noreturn() {}
print( noreturn() ); // if no return statement return default value for each type, for bool it is false


////////////
int t1; bool t2; string t3;
(t1, t2, t3) = (5, true, "tuples are cool!");
print(t1);
print(t2);
print(t3);

auto autoTuple = (6, false, "auto tuples are even more cool!");
(t1, t2, t3) = autoTuple; 
print(t1);
print(t2);
print(t3);

(int,bool) tupleFunc() {
	return (1,true); 
}
(t1,t2) = tupleFunc();
print(t1);
print(t2);


int t1; bool t2;
auto autoTupleFunc() { 
	return (2,false); 
}
(t1,t2) = autoTupleFunc();

print(t1);
print(t2);

//// static check of auto func decl - bug fixed:

auto af2() {
	bool a ;
	a = af2();
	return true;
}

auto af3() {
	return true;
}

bool bfa = af3();


////////////////////////
int arr2D[][] = {{1,2,3},{4,5}};
print (arr2D[0][2]);
print (arr2D[1][1]);

bool arr3D[2][2][2];
arr3D[1][1][0] = false;
arr3D[1][1][1] = true;
print(arr3D[1][1]);

auto auto2d = {{1,2,3},{4,5}};
print (auto2d[1][0]);

int arr2D2[2][2];
int arr1D[] = {1,2,3};
arr2D2[1] = arr1D;
print( arr2D2 );

(int,(bool,int)) u = (2,(true,3));

///////////////////////
a = 0;
b = 0;
int rf(int q, int w) { q++;w++; }
rf(a&, b);
print(a);
print(b);

/////////////////////
int arrr[] = {1,1};
rf(arrr[0]&, arrr[1]);
print(arrr[0]);
print(arrr[1]);


/////////////////// tuple more
int a = 1;
int b = 2;
int c = 3;
int d = 4;
(a,b) = (c,d);
print(a);

auto e = (3); //this is int
e = 2;
int f = (3); // ..works as well

auto g = (a); //this is int
g = 3;
