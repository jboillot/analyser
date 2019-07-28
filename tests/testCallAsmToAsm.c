int bref() {
	int a;
	asm {
		JMP @other

		maFonction:
		POP edx
		POP ebx
		MOV eax, ebx
		PUSH edx
		RET

		other:
		PUSH 42
		CALL @maFonction
		MOV a, eax
	};
	return a;
}

int main() {
	int a;
	a = bref();
	return a;
}
