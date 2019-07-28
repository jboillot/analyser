int facto(int n) {
	if (n == 0) {
		return 1;
	} else {
		int last;
		last = facto(n-1);
		return n * last;
	}
}

int main() {
	int i;
	i = facto(10);
	return 0;
}
