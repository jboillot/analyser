int main() {
	int a;
	int b;
	a = 0;
	b = 0;
	while (a < 10) {
		if (a < 5) {
			b = b+1;
		} else {
			b = b+2;
		}
		a = a + 1;
	}
	return 0;
}
