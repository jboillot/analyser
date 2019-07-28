int main() {
	int a = 0;
	asm {
		PUSH 42
		POP a
	};
	return 0;
}
