import random
import string

n = 300

letters = string.ascii_uppercase
nonewlines = ''.join(random.choice(letters) for i in range(n*n))

def insert_newlines(s, every):
    lines = []
    for i in range(0, len(s), every):
        lines.append(s[i:i+every])
    return '\n'.join(lines)


output = insert_newlines(nonewlines, n)
print(output)
print('\nTHIS IS A RANDOM TEST NO WORRIES HERE', end='')
# f = open('/home/helen/Desktop/pfp-wordsearch/data/dictionary.txt', 'r')
# print(f.read())


with open('/home/helen/Desktop/pfp-wordsearch/data/dictionary.txt') as f:
    for line in f:
        print(line.strip().upper(), end=' ')