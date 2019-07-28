int fun1(int a) {
	return a + 1;
}

int fun2(int a) {
	int b;
	b = fun1(a);
	return b + 41;
}

int main() {
	int a;
	a = fun2(0);
	return a;
}
