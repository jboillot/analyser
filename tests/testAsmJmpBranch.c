int main() {
	int a;
	if (a <= 0) {
		if (a >= -3) {
			return 0;
		} else {
			a = 1;
			asm { JMP @finBoucle };
		}
	} else {
		a = 2;
		asm { finBoucle: };
	}

	return a;
}
