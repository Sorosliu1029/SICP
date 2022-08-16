We have recently introduced quotation as a means of allowing our programs to work with symbols and also as a convenient way to type lists into scheme. For example, we can try

(define colors ‘(red orange yellow green blue indigo violet))
(member? ‘orange colors)

For the first line, the reader produces the expression

(define colors (quote (red orange yellow green blue indigo violet)))

The evaluator then applies the rule of evaluation for the special form define, which says to evaluate the second argument to define and then associate the symbol that is the first argument with the resulting value. The value of (quote (red orange yellow green blue indigo violet)) is just (red orange yellow green blue indigo violet) by the rule for evaluating the special form quote.

From these examples, you can actually tell a profound fact about the implementation of Scheme: the expressions being evaluated by the evaluator are represented internally as list structure. This is why typing in a list that is quoted leads to a value that is indeed a list. And you have the reader to thank for having created it when it recognized that (roughly), stuff surrounded by parentheses is meant to represent a list.
