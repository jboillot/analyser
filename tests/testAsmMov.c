int main() {
	int a;
	int b;
	a = 0;
	asm {
		MOV b, a
	};

	return 0;
}
