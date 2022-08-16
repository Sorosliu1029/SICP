You have seen before the Scheme’s interaction with the user involves three interlinked functionalities:

The reader turns the user’s typed input into the proper internal representations of numbers, character strings, symbols, Boolean value, lists, etc.
The evaluator applies Scheme’s rules of evaluation to find the value of an expression that has been entered by the user.
The printer turns the internal representation back into a meaningful form for the user.
For a simple example, suppose the user types the characters “3.14159265358979323846” into Scheme’s top-level REPL (read./eval/print loop). The reader recognizes that this sequence of characters is a valid syntactic representation of a number, and constructs the floating point internal representation of (an approximation) of the numbers the user meant to type. Scheme’s evaluator then applies it normal rules of evaluation; it discovers that the expression is a number, which is self-evaluating, and therefore returns the number as its value. The printer then converts this internal representation of the number to a form that is presented to the user. Here is a transcript of this interaction:

3.14159265358979323846
;Value: 3.141592653589793

Note that what got typed back is not exactly what got typed in. This is a result of limitations in the internal representations used by Scheme. In this example, the IEEE floating point standard used by Scheme does not support as many significant digits of precision as the user typed in.

The reader understands the purely syntactic rules of Scheme, and can determine whether what the user typed is meant to be a number (as in the above example) or a Boolean value (#t or #f) or a character string (“I am inside double-quote marks”.) or a symbol (e.g., + or square). It also knows the syntactic rule for quotation:

‘<anything>

is read as

(quote <anything>)

and it understands the syntax of list structure and dotted-pair notation. Thus,

(+ 2 3)

is read as a list of three elements, the symbol +, the number 2 and the number 3. Note, however, that the reader knows nothing about the rules of evaluation, and thus cares not at all whether the user has typed (quote 18) or (sqrt 18).

The reader’s rules are generally intuitive, and never ambiguous. However, you might find something like the following odd:

3.1.3
;Unbound variable: 3.1.3

This is because a number cannot have two periods, so the reader reads “3.1.3” as a symbol. The evaluator then finds that there is no value associated with this symbol.