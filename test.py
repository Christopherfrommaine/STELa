from random import randint
from math import log2

def ackermann(set):
    return sum(2 << ackermann(i) for i in set)


def bitpred(i, j):
    return ((1 << j) & i) != 0

def inv_ackermann(i):
    o = set()
    for j in range(64):
        if bitpred(i, j):
            o.add(inv_ackermann(j))
    return frozenset(o)


def max_else_n(list, else_val):
    if len(list) == 0:
        return else_val
    else:
        return max(list)

def ackermann_log(set):
    values = [log2(ackermann_log(i) + 2) for i in set]
    return sum(values)


def random_set(depth):
    o = set()
    for _ in range((randint(0, depth) if depth > 0 else 0) + (0 if randint(0, 3) == 1 else 1)):
        o.add(random_set(depth - 1))
    return frozenset(o)

seen = {}
for i in range(10000):
    s = random_set(2)
    # s = inv_ackermann(i)
    a = ackermann_log(s)
    if a in seen.keys():
        assert s == seen[a]
    else:
        seen.update({a: s})
    
    print(f"{s} | {a}")

def setify(x, depth):
    if depth == 0:
        return frozenset([x])
    return frozenset([setify(x, depth - 1)])
print(ackermann_log(setify(frozenset(), 20)))
