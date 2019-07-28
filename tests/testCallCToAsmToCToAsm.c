int identity2(int a) {
	int b;
	b = identity3(a);
	return b;

	asm {
		identity3:
		POP edx
		POP eax
		PUSH edx
		RET
	};

	asm {
		identity1:
		POP edx
		POP eax
		PUSH edx
		PUSH eax
		CALL identity2
		RET
	};
}

int main() {
	int a;
	a = identity1(42);
	return a;
}
