# NOTE
Currently this language is **UNDER CONSTRUCTION** and being developed currently. It is not finished and cannot be used to run programs yet.

# STELa
STELa (*S*et *T*heory *E*so-*La*ng) is a mathematics-inspired programming language. It's based on the simple idea that everything (yes, **everything**) is a set:

Sets are sets, numbers are sets, functions are sets, lists are sets, strings are sets, *everything is a set*.

# Getting Started
The STELa parser and interpreter are built in Rust. To run a program, install rust, clone this repo, and run `cargo r -- foo.stela` in the STELa directory, passing the path to a file ending in `.st`.

The simplest program in STELa is printint out an empty set:

```stelaela
print {};
```

For some fuller examples, see the `examples/` directory or check out the `src/prelude.st` which is run at the beginning of any STELa program unless the `--no-prelude` option is specified.

# Language Design
## Syntax
To create a set, simply put any number of space-seperated elements between two braces:
```stela
a = {b c d};
```

To define a function, simply create an infinite set which maps all possible inputs to their respective output. STELa is lazily evaluated and uses pattern matching to try to find a match.
```stela
square = {(x, x * x) for all x};
```

To apply a map to a value, use the `@` symbol:
```stela
2 = {{}, {{}}};
four = square@2;
```

Notice that numbers (other than 0 and 1) are not included in the language. They must be manually defined or found using the successor function `S` (helpfully included in the prelude!).

In the definition for a map, it may look like we are using a tuple for an ordered pair, but remember that *everything is a set*. In reality, a comma is just a binary operator which maps `a, b` to `{{a b} {a}}`

STELa has syntax sugar where any three sets together are evaluated as if the middle set is a binary operator. That is,

```
a b c => (b@a)@c
```

This allows you to define binary operators easily, and in fact all operators (such as `+`, `*`, `U` (union), `|` (filter), etc) are defined in the prelude and aren't considered core language features.

This does not work in the case of sets like:
```stela
{a b c}
```

Instead, write:
```stela
{(a b c)}
```

to be interpreted as
```stela
{b@a@c}
```

See `examples/` for more examples of how this syntax can be used in practice.

## Keywords

There are only five special keywords:
- `in` checks membership.
- `for all` lets you define an infinite set for all possible values of a variable.
- `print` prints a set in set notation to standard out/
- `display`, when given a vector, interprets each element as an Ackermann code between 0-255 and displays that as a UTF-8 string.
- `none` is for **INTERNAL USE ONLY**. Don't use it please. It is simply removed from a set during evaluation. This is necessary to implement filter, but an implementation of filter is included in the prelude, so there is no good reason to use none.

Everything else can be done with sets alone. Looping can be done with recursion, and a general Y-combinator is included in the prelude.

## Russel's Paradox
Some of you set theorists can probably see one major flaw in the languages design. It is exemplified by the following statement:

```stela
R = {s | not(s in s) for all s};
print R in R;
```
In other words, "does the set of all sets that do not contain themselves contain itself?". This is, of course, paradoxical. Attempting to evaluate this will lead to unknown behavior and should be avoided.
