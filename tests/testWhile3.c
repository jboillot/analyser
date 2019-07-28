// On a ici une sur-approximation à causes des intervalles.
// À la fin on a un code retour dans [10,11] au lieu de {11}.
int main() {
	int a = 2;
	while (a < 10) {
		a += 3;
	}
	return a;
}
