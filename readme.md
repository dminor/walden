Walden
======

Walden is a little language in the Smalltalk and Self family. Its syntax is
close to Smalltalk, but like Self, it uses objects as prototypes rather than
classes. Like my first language [Scoundrel](https://github.com/dminor/scoundrel)
my primary goal was to learn more about interpreters and to improve my Rust.
Scoundrel is purely functional, so I wanted Walden to be imperative and
object-oriented. Since scoundrel is interpreted by walking the abstract
syntax tree, I wanted Walden to have a virtual machine.

I chose a prototype based object system partly because it was syntactically
simpler and partly just to get a better understanding of how one works. Both
JavaScript and Lua use prototype based object systems, but to me, in both
cases the syntax hides what should be conceptually simple.

Walden is named after Henry David Thoreau's *Walden*.

Keywords
--------

The following are reserved keywords: *true*, *false*, and *nil*.

Values
------

### Block

A block is a lambda expression. Each block consists of one or more statements
separated by periods, except for the last statement in a block where the
period can be omitted. They optionally accept one or more arguments
and define one or more local variables. They are evaluated by sending the
value, value: or value:value: messages, depending upon the number of arguments
accepted.

```
"Returns 1"
[1.] value.

"Takes an argument, returns 2"
[:x| x + 1] value: 1.

"Returns a function which increments the argument by 1.
[:x|[:y|x + y]] value: 1.

"Takes an argument and defines a local variable"
[:x||y| y := x + 1. y].
```

A block can also be set to be a member of an object. In this case, when called,
a special *self* member is set, which is the object associated with the method.
Members of *self* are accessed by prefixing the member name with an @ sign, as
in Ruby.

```
o := Object clone.
o set: 'val' to: 0.
o set: 'inc' to: [
  @val := @val + 1.
]
o set: 'getVal' to [@val].
"Returns 1"
o inc getVal.
```

Besides the value messages, blocks also respond to the whileTrue: message which
is used to implement iteration.
```
"Computes 5!"
fact := 1.
n := 5.
[ n > 1 ] whileTrue: [
  fact := fact * n;
  n := n - 1.
].
```

Blocks also respond to a disassemble message which outputs the virtual machine
opcodes used to define it.
```
[1 + 2] disassemble.
" Prints the following:
@0 [ |  ]
  0: const 1
  1: const 2
  2: srcpos 1 4
  3: add
  4: ret
"
```

### Boolean

Booleans take the values `true` and `false`. They accept the and:, not: and or:
messages which implement boolean operations, and the messages ifFalse:, ifTrue:
and ifTrue:ifFalse, which implement conditionals.
```
"Returns false"
(1 < 2) and: (3 < 2)

"Returns true"
(1 < 2) or: (3 < 2)

"Returns 2"
(2 < 3) ifFalse: [1] ifTrue: [2].
```

### Nil

Nil is an object which responds to no messages. It is the prototype of the
root object and the value of undefined variables.

### Number

Numbers are 64 bit floats. They respond to the `+`, `-`, `*`, and `/` messages
which implement the usual arithmetic functions. Like Smalltalk, messages are
evaluated left to right and do not follow the usual order of operations.

```
"Evaluates to 6"
1 + 5 * 2
```

Unlike Smalltalk it is not possible to define additional binary operators.

### Object

Like JavaScript, only a single prototype object is used. I've never been a big
fan of mixins in other languages, and in
[Programming as an Experience: The Inspiration for Self](http://bibliography.selflanguage.org/programming-as-experience.html)
the Self designers say that supporting multiple inheritance added a lot of
complexity (and bugs!) to the language. The prototype of an object can be
accessed by sending it the prototype message.

The clone message creates a copy of the object receiving the message. New
objects are created by cloning the root Object.

```
Object clone.
```

Objects respond to set:to: and override:with: messages. The difference is
that set:to: first searchs the object's prototypes for the member and updates
it if it exists, while override:with: always sets the member on the object
receiving the message.
```
animal := Object clone.
animal override: 'noise' with: '<silence>'.
animal override: 'speak' with [@noise.].
dog := animal clone.
dog override: 'noise' with: 'bark bark'.
cat := animal clone.
cat override: 'noise' with: 'meow'.

"Returns 'bark bark'"
dog speak.

"Returns 'meow'"
cat speak.

cat2 := cat clone.
"Both cats will now say 'meeooowwww'"
cat2 set: 'noise' to: 'meeooowwww'.
```

The assignment operator ```:=``` is shorthand for sending the set:to: message
to an object.

Unlike Self, it is necessary to explicitly define accessor and setter methods
for a member.

### String

A string is a list of characters enclosed by single quotes. They respond to
the split: and startsWith: messages.

```
"Split returns an iterator over the parts of the string"
parts := 'hello world' split: ' '.
"Returns 'hello'"
parts next.
"Returns 'world'"
parts next.
"Returns nil"
```

The Virtual Machine
-------------------

Walden uses a stack based virtual machine. One stack is used for message
arguments and arithmetic calculations, and a second call stack is used to keep
track of block calls.

The opcodes for the virtual machine are listed below:
* Add - pops and adds two values from the stack.
* Div - pops and divides two values from the stack.
* Const - pushes a constant value to the stack.
* Mul - pops and multiplies two values from the stack.
* Sub - pops and subtracts two values from the stack.
* Less - pops and compares two values from the stack.
* LessEqual - pops and compares two values from the stack.
* Equal - pops and compares two values from the stack.
* NotEqual - pops and compares two values from the stack.
* Greater - pops and compares two values from the stack.
* GreaterEqual - pops and compares two values from the stack.
* Call - calls a block.
* Dup - duplicates the last value on the stack.
* Lookup - looks up a member on an object.
* Pop - pops the last object from the stack.
* Ret - returns from a block call.
* Srcpos - sets the position in the source associated with the opcode.
* Swap - swaps the last and second last values on the stack (currently unused.)
* This - returns the block on the top of the call stack.

Since conditionals and iterations are handled by sending messages, no branch
instructions are required. Interestingly, in Smalltalk, conditionals are used
in the virtual machine and defining a message like ifTrue: on a non-boolean
object will result in a runtime error.

The single most difficult thing to implement in the virtual machine was
lexical scoping and closures. Since blocks are also objects, it is possible to
use their prototype as an environment in which to store arguments and local
variables. To get lexical scoping to work, it is necessary to properly set up
the prototype inheritance when the block is created. I ended up using the
callstack to keep track of the active blocks and set up the prototypes
accordingly.

The other difficulty was that syntactically, a block is always created
separately from the object for which it may eventually become a method. This
was handled by setting a special *self* member variable on the block at the
time it is called, and using the @ prefix to access members on that self object.
If I had supported multiple prototypes I could have done away with this and
simply had the object be a prototype of the block along with the prototype
which serves as its environment.
