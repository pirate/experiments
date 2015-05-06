import sys

class ostream:
    text = []

    def __init__(self, file):
        self.file = file

    def __lshift__(self, obj):
        self.text.append(str(obj))
        return self

    def __repr__(self):
        self.file.write(''.join(self.text))
        return ''

cout = ostream(sys.stdout)
endl = '\n'

def printf(fmt_str, *argv):
    print fmt_str % argv

x, y = 'hello', 'world'

cout << x << " " << y << endl
