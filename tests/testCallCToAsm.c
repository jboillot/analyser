int test() {
	asm {
		maFonction:
		POP edx
		POP eax
		PUSH edx
		RET
	};
}

int main() {
	int a;
	a = maFonction(42);
	return 0;
}
