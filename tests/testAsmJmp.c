int main() {
	int a;
	a = [0,2];
	asm { monLabel: };
	a = a + 1;
	if (a == 1) {
		asm { JMP @monLabel };
	}
	return 0;
}
