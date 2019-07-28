int main() {
	int a;
	int b;
	a = 0;
	asm {
		MOV a, [b]
	};

	return 0;
}
