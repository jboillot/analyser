int test(int *myPtr) {
	*myPtr = 42;
}

int main() {
	int a = -1;
	int b = &a;
	b = test(b);
	return a;
}
