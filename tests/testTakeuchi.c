int takeuchi(int x, int y, int z) {
	if (y < x) {
		int buf1;
		int buf2;
		int buf3;
		int buf;
		buf1 = takeuchi(x-1, y, z);
		buf2 = takeuchi(y-1, z, x);
		buf3 = takeuchi(z-1, x, y);
		buf = takeuchi(buf1, buf2, buf3);
		return buf;
	} else {
		return y;
	}
}

int main() {
	int a;
	int b;
	a = takeuchi(5, 25, 30);
	b = takeuchi(10, 15, 20);
	return a + b;
}
