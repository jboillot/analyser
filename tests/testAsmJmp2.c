int main() {
	int a;
	a = 1;
	while (true) {
		asm { JMP @lab };
	}
	a = 4;
	asm { lab: };
	return 0;
}
