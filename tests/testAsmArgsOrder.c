int test() {
	asm {
		maFonction:
		POP edx
		POP ebx
		POP eax
		PUSH edx
		RET
	};
	//L'équivalent serait :
	//int maFonction(int a, int b) {
	//	return a;
	//}
}

int main() {
	int a;
	a = maFonction(0,-1);
	return a;
}
