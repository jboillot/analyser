int main() {
	int a;
	a = [0,42];
	int b;
	b = 0;
	while (a < 10) {
		while (b < 5) {
			b = b + 1;
			a = a + 2;
			if (a + b == 15) {
				a = 0;
			}
		}
		a = a + 1;
	}
	return 0;
}
