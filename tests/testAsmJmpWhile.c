int main() {
	int a;
	a = [0,2];
	asm { JMP @dansWhile };
	a = a / 0;
	while (a < 2) {
		asm { dansWhile: };
		if (a == 0) {
			return 1;
		}
		a = a + 1;
	}
	return 0;
}
