---
author: Carlo Hamalainen
date: "2019-02-09T12:44:45Z"
title: An expression evaluator in CSV
url: /2019/02/09/an-expression-evaluator-in-csv/
---
Many business problems boil down to reading data from somewhere, transforming it, and writing it somewhere else.

We could implement the transformations in code, but non-technical users might like to see and edit the rules without having to deploy a new build.

Here's an example rule:

  1. Read the value at `https://example.com/foo/bar/x`, refer to it as `val`.
  2. Return `10*(1.0/val)`.

Or, a more complicated rule:

  1. Read the value at `https://example.com/foo/bar/x`, refer to it as `val_x`.
  2. Read the value at `https://example.com/foo/bar/y`, refer to it as `val_y`.
  3. Return the average of `1.0/val_x` and `1.0/val_y`.

The question is how to flatten this out into a format suitable for a CSV file, to allow users to easily update the rules using Excel.

One approach would be to implement an expression [DSL](https://en.wikipedia.org/wiki/Domain-specific_language), but this gets a bit painful when the input space is cells in a spreadsheet or CSV file. There are also questions about encoding the order of evaluation.

[Reverse Polish notation](https://en.wikipedia.org/wiki/Reverse_Polish_notation) is a simple way to encode arbitrary mathematical formulas in a flat sequence. Here's how to write the first example:

    CONSTANT 1
    GET_DATA /foo/bar/x
    DIV
    CONSTANT 10
    MUL

This sequence of operations will do the following:

  1. Push 1 onto the stack.
  2. Get data from the URL, push onto the stack.
  3. Perform the divide operation (1.0 divided by the value we got from the URL), and push that onto the stack.
  4. Push 10 onto the stack.
  5. Multiply the result from step 3 by 10.

We might use the following format in a CSV file. The ORDER column ensures the correct sequencing of operations.

| ID     | OUTPUT_ID | ORDER | OP       | CONST | DATA_SOURCE |
|--------|-----------|-------|----------|-------|-------------|
| KEY001 | RULE001   | 0     | CONSTANT | 1     |             |
| KEY002 | RULE001   | 1     | GET_DATA |       | /foo/bar/x  |
| KEY003 | RULE001   | 2     | DIV      |       |             |
| KEY004 | RULE001   | 3     | CONSTANT | 10    |             |
| KEY005 | RULE001   | 4     | MUL      |       |             |

And here is how to encode the second example:

|ID|OUTPUT_ID|ORDER|OP|CONST|DATA_SOURCE|
|--- |--- |--- |--- |--- |--- |
|KEY0200|RULE002|0|CONSTANT|1||
|KEY0201|RULE002|1|GET_DATA||/foo/bar/x|
|KEY0202|RULE002|2|DIV|||
|KEY0203|RULE002|3|CONSTANT|1||
|KEY0204|RULE002|4|GET_DATA||/foo/bar/y|
|KEY0205|RULE002|5|DIV|||
|KEY0206|RULE002|6|PLUS|||
|KEY0207|RULE002|7|CONSTANT|2||
|KEY0208|RULE002|8|DIV|||

Evaluating a sequence of operations is straightforward. Start with an empty stack (in Python, this is just a normal list). If the next operation is CONSTANT or GET_DATA, push the value onto the stack. Otherwise, an operation like PLUS will need two operands, so pop two things off the stack and then do that actual operation. As a bonus, we can render a normal mathematical expression as we go: instead of putting a floating point number onto the stack, put a string onto the stack.

Here is the entire evaluator:

```python
def eval_rule(rule):
    s = []

    expr = []

    for (_, x) in rule.sort_values('ORDER').iterrows():
        op = x['OP']

        if op == 'GET_DATA':
            s.append(x['GET_DATA'])
            expr.append('(GET_DATA: ' + str(x['DATA_SOURCE']) + ')')

        elif op == 'CONSTANT':
            s.append(x['CONST'])
            expr.append(str(x['CONST']))

        elif op == 'MUL':
            b = s.pop()
            a = s.pop()
            s.append(a*b)

            b2 = expr.pop()
            a2 = expr.pop()
            expr.append('(' + a2 + '*' + b2 + ')')

        elif op == 'PLUS':
            b = s.pop()
            a = s.pop()
            s.append(a+b)

            b2 = expr.pop()
            a2 = expr.pop()
            expr.append('(' + a2 + '+' + b2 + ')')

        elif op == 'MINUS':
            b = s.pop()
            a = s.pop()
            s.append(a-b)

            b2 = expr.pop()
            a2 = expr.pop()
            expr.append('(' + a2 + '-' + b2 + ')')

        elif op == 'DIV':
            denominator = s.pop()
            numerator   = s.pop()
            s.append(numerator/denominator)

            denominator2 = expr.pop()
            numerator2   = expr.pop()
            expr.append('(' + numerator2 + '/' + denominator2 + ')')
        else:
            raise ValueError('Unknown operator: ' + op)

    if len(s) != 1:
        raise ValueError('Expected one item on the evaluation stack, but found: ' + str(s))

    if len(expr) != 1:
        raise ValueError('Expected one item on the expression stack, but found: ' + str(expr))

    return s[0], expr[0]
```

Just for fun, we will also evaluate the example from the Wikipedia page on Reverse Polish notation:

|ID|OUTPUT_ID|ORDER|OP|CONST|DATA_SOURCE|
|--- |--- |--- |--- |--- |--- |
|KEY0101|RULE003|0|CONSTANT|15||
|KEY0102|RULE003|1|CONSTANT|7||
|KEY0103|RULE003|2|CONSTANT|1||
|KEY0104|RULE003|3|CONSTANT|1||
|KEY0105|RULE003|4|PLUS|||
|KEY0106|RULE003|5|MINUS|||
|KEY0107|RULE003|6|DIV|||
|KEY0108|RULE003|7|CONSTANT|3||
|KEY0109|RULE003|8|MUL|||
|KEY0110|RULE003|9|CONSTANT|2||
|KEY0111|RULE003|10|CONSTANT|1||
|KEY0112|RULE003|11|CONSTANT|1||
|KEY0113|RULE003|12|PLUS|||
|KEY0114|RULE003|13|PLUS|||
|KEY0153|RULE003|14|MINUS|||


Here's the output:

    $ python3 rpn.py
    RULE001
    ((1.0/(GET_DATA: /foo/bar/x))*10.0)
    0.23809523809523808

    RULE002
    (((1.0/(GET_DATA: /foo/bar/x))+(1.0/(GET_DATA: /foo/bar/y)))/2.0)
    0.02857142857142857

    RULE003
    (((15.0/(7.0-(1.0+1.0)))*3.0)-(2.0+(1.0+1.0)))
    5.0

Reverse Polish Notation gives us a compact way to represent a sequence of operators with no ambiguity about the order of evaluation.
