hsBF
====

a BF interpreter written in Haskell

---

My BF definition:
-----------------

As best I can tell the Grammar is

```
S -> A~
A -> aA | a | [A]
a -> < | > | + | - | , | . | ~ | {empty Stmbol}
```

Character	Symantic Meaning:
---------------------------

```
  >	increment the data pointer (to point to the next cell to the right).
  <	decrement the data pointer (to point to the next cell to the left).
  +	increment (increase by one) the byte at the data pointer.
  -	decrement (decrease by one) the byte at the data pointer.
  .	output the byte at the data pointer. (ASCII value for integer)
  ,	accept one byte of input, storing its value in the byte at the data pointer.
  [	if the byte at the data pointer is zero, then instead of moving the instruction pointer forward to the next command, jump it forward to the command after the matching ] command.
  ]	if the byte at the data pointer is nonzero, then instead of moving the instruction pointer forward to the next command, jump it back to the command after the matching [ command.
  ~ End of file symbol.  I find it useful to have a halt for debugging.  But this is not a part of the launguage.
```

---

The machine is supposed to be able to go forward or backward infinitely. 
And each memory location stores (big i) Integers. 

Realisticly, the integers and memory is limited by the computer this is running on.
