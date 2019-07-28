int test() {
	asm {
		otherFunction://(int a)
		POP eax
		JMP @finAppel
	};
	
	asm {
		maFonction://(int a)
		POP edx
		PUSH @otherFunction
		RET
	};

	asm {
		maFonctionPrincipale://()
		PUSH 42
		CALL @maFonction
		finAppel:
		RET
	};
}

int main() {
	int a;
	a = maFonctionPrincipale();
	return a;
}
