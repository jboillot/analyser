int ackermann(int m, int n) {
	if (m == 0) return n + 1;
	if (n == 0) {
		int buf;
		buf = ackermann(m-1, 1);
		return buf;
	}
	int buf;
	int buf2;
	buf2 = ackermann(m, n-1);
	buf = ackermann(m-1, buf2);
	return buf;
}

int main() {
	int buf;
	buf = ackermann(3, 3);
	return buf;
}
