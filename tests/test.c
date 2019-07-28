int hello(int a, int b) {
	return a + b;
}

int main() {
	int a;
	int b;
	int* c;
	int** d;
	int* e;
	int f;
	int* g;
	a = 1;
	f = [-5, 42];
	g = &f;
	b = [-15, 3];
	c = &a;
	d = &c;
	e = &a;
	if (b <= -1) {
	    if (a + b == 0) {//Le a / b ne marche pas à cause du backward !
	        a = 0;
	    } else {
	        e = g;
	        a = 3;
	    }
	} else {
	    b = 3;
	}
	b = ~b;
	*e = 42;

	a = hello(a, b);
	return 0;
}
