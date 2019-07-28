int test(int a) {
	if (a == 0) {
		while (true) {
			a = 2;
		}
	} else {
		return a;
	}
}

int main() {
	int a;
	a = 1;
	a = test(a);
	return 0;
}
