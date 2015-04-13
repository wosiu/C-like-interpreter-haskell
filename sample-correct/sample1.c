int main() {
	int b() {
		int c() {
			return b();		
		}
		return c();	
	}
	return b();
}
