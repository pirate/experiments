class Trie:
    """Ttrie with insert, search, and startsWith methods."""
    def __init__(self):
        self.root = defaultdict()

    def insert(self, word):
        current = self.root
        for letter in word:
            current = current.setdefault(letter, {})
        current.setdefault("_end")

    def search(self, word):
        current = self.root
        for letter in word:
            if letter not in current:
                return False
            current = current[letter]
        if "_end" in current:
            return True
        return False

    def startsWith(self, prefix):
        current = self.root
        for letter in prefix:
            if letter not in current:
                return False
            current = current[letter]
        return True

# Now test the class

test = Trie()
test.insert('helloworld')
test.insert('ilikeapple')
test.insert('helloz')

print test.search('hello')
print test.startsWith('hello')
print test.search('ilikeapple')
