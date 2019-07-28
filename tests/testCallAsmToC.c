int test(int a, int b, int c) {
	return a + b + c;
}

int main() {
	int a;
	asm {
		PUSH 1
		PUSH 2
		PUSH 3
		CALL test
		MOV a, eax
	};
	return a;
}
