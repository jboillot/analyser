int main() {
	int a;
	if (a > 0) {
		if (a < 5) {
			asm { JMP @lab2 };
		} else {
			a = 0;
		}
		asm { JMP @lab };
	} else {
		asm { lab: };
		a = 42;
		asm { lab2: };
	}
	return a;
}
