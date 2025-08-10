public function isIsogram(string sentence) returns boolean {
    int[] parts = sentence
        .toLowerAscii()
        .toCodePointInts()
        .sort()
        .filter(n => n > 96);
    foreach int i in 1..< parts.length() {
        if (parts[i] == parts[i - 1]) {
            return false;
        }
    }
    return true;
}
