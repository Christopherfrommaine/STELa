## Language Definitions:
# '{a b c}' denotes a set
# '=' sets a variable to an expression
# 'in' keyword checks for membership
# 'none' keyword is an internal marker which the empty set contains (i.e. {} is a set containing none) which is used for filtering. This is only under-the-hood
# 'for all a' creates an infinite set based on the universe of all values of a (i.e. all possible sets and sets of sets)
# 'print' will evaluate a variable and print it as a set
# 'display' will attempt to interpret a set as vector of Ackermann-encoded numbers corresponding to UTF-8 characters

# 'a@b' is the application of a map 'a' to the set 'b'
# 'a b c' is evaluated with b as a binary operator to (b@a)@c
# unless within a set: '{a b c}'. Use '{(a b c)}' instead.
# All evaluations are as lazy as possible,
# only being actually computed for membership checks (is that needed???) and printing

## Basic Definitions
# (A, B) == A, B  == ,@A@B == {{A} {A B}} == a pair of A and B
, = {(a, {(b, {{a} {a b}}) for all b}) for all a};

0 = {};
1 = {0};

## Binary Logic
return_first = {(a, {(b, a) for all b}) for all a};
return_second = {(a, {(b, b) for all b}) for all a};
if = {(0, return_first) (1, return_second)};
not = {(0, 1), (1, 0)};
or  = {(0, {(0, 0) (1, 1)}) (1, {(0, 1) (1, 1)})};
and = {(0, {(0, 0) (1, 0)}) (1, {(0, 0) (1, 1)})};
xor = {(0, {(0, 0) (1, 1)}) (1, {(0, 1) (1, 0)})};

## Set Operations
| = {(a, {(bool, if@bool@a@none) for all bool}) for all a};  # 'such that' operator
U = {(A, {(B, {(e | ((e in A) or  (e in B))) for all e}) for all B}) for all A};  # Union
& = {(A, {(B, {(e | ((e in A) and (e in B))) for all e}) for all B}) for all A};  # Intersection
^ = {(A, {(B, {(e | ((e in A) xor (e in B))) for all e}) for all B}) for all A};  # Symmetric Difference
? = {(a, {(b, and@((a in b), (b in a))) for all b}) for all a};  # equality

## Basic Arithmetic
S = {(n, n U {n}) for all n};
+ = {(0, {(n, n) for all n})} U {(S@m, {(n, S@(m + n)) for all n}) for all m};
* = {(0, {(n, 0) for all n})} U {(S@m, {(n, (m * n) + n) for all n}) for all m};

## Higher Order Operations
map = {(f, {(S, {(f@element | (element in S)) for all element}) for all S}) for all f};
filter = {(pred, {(S, {(e | ((e in S) and pred@e)) for all e}) for all S}) for all pred};
# fold is impossible due to sets being unordered (though I could define an order based on trees)
all = {(pred, {(S, (S ? filter@pred@S)) for all S}) for all pred};
any = {(pred, {(S, not@(0 ? filter@pred@S)) for all S}) for all pred};

## Lists and Vectors
# {} linked_list_append 1 linked_list_append 2 linked_list_append 3 = (3, (2, (1, {})))
linked_list_append = {(list, {(element, (element, list)) for all element}) for all list};

# finds the maximum value of a set of numbers; equivalent to folded_union({e1 e2 e3 e4... }) = e1 U e2 U e3 U e4 ...
folded_max = {(S, {(x | any@{(e, x in e) for all e}@S) for all x}) for all S};
# vector[a, b, c] = {(0, a) (1, b) (2, c)}
vector_length = {(vector, folded_max@(map@return_first@vector)) for all vector};
vector_append = {(vector, {(element, (vector_length@vector, element)) for all element}) for all vector};

## Lambda Calculus
Y_combinator = {(f, f@({(x, f@(x@x)) for all x}@({(x, f@(x@x)) for all x}))) for all f};