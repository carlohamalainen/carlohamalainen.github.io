---
title: An expression evaluator in CSV
date: 2019-02-09T12:44:45+00:00
author: Carlo Hamalainen
layout: post
permalink: /2019/02/09/an-expression-evaluator-in-csv/
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

<table border="0" cellspacing="0">
  <colgroup width="68"></colgroup> <colgroup width="87"></colgroup> <colgroup width="59"></colgroup> <colgroup width="84"></colgroup> <colgroup width="58"></colgroup> <colgroup width="109"></colgroup><tr>
    <td align="left" height="17">
      ID
    </td>
    
    <td align="left">
      OUTPUT_ID
    </td>
    
    <td align="left">
      ORDER
    </td>
    
    <td align="left">
      OP
    </td>
    
    <td align="left">
      CONST
    </td>
    
    <td align="left">
      DATA_SOURCE
    </td>
  </tr>
  
  <tr>
    <td align="left" height="17">
      KEY0001
    </td>
    
    <td align="left">
      RULE001
    </td>
    
    <td align="right">
    </td>
    
    <td align="left">
      CONSTANT
    </td>
    
    <td align="right">
      1
    </td>
    
    <td align="left">
       
    </td>
  </tr>
  
  <tr>
    <td align="left" height="17">
      KEY0002
    </td>
    
    <td align="left">
      RULE001
    </td>
    
    <td align="right">
      1
    </td>
    
    <td align="left">
      GET_DATA
    </td>
    
    <td align="left">
       
    </td>
    
    <td align="left">
      /foo/bar/x
    </td>
  </tr>
  
  <tr>
    <td align="left" height="17">
      KEY0003
    </td>
    
    <td align="left">
      RULE001
    </td>
    
    <td align="right">
      2
    </td>
    
    <td align="left">
      DIV
    </td>
    
    <td align="left">
       
    </td>
    
    <td align="left">
       
    </td>
  </tr>
  
  <tr>
    <td align="left" height="17">
      KEY0004
    </td>
    
    <td align="left">
      RULE001
    </td>
    
    <td align="right">
      3
    </td>
    
    <td align="left">
      CONSTANT
    </td>
    
    <td align="right">
      10
    </td>
    
    <td align="left">
       
    </td>
  </tr>
  
  <tr>
    <td align="left" height="17">
      KEY0005
    </td>
    
    <td align="left">
      RULE001
    </td>
    
    <td align="right">
      4
    </td>
    
    <td align="left">
      MUL
    </td>
    
    <td align="left">
       
    </td>
    
    <td align="left">
       
    </td>
  </tr>
</table>

And here is how to encode the second example:

<table border="0" cellspacing="0">
  <colgroup width="68"></colgroup> <colgroup width="87"></colgroup> <colgroup width="59"></colgroup> <colgroup width="84"></colgroup> <colgroup width="58"></colgroup> <colgroup width="109"></colgroup><tr>
    <td align="left" height="17">
      ID
    </td>
    
    <td align="left">
      OUTPUT_ID
    </td>
    
    <td align="left">
      ORDER
    </td>
    
    <td align="left">
      OP
    </td>
    
    <td align="left">
      CONST
    </td>
    
    <td align="left">
      DATA_SOURCE
    </td>
  </tr>
  
  <tr>
    <td align="left" height="17">
      KEY0200
    </td>
    
    <td align="left">
      RULE002
    </td>
    
    <td align="right">
    </td>
    
    <td align="left">
      CONSTANT
    </td>
    
    <td align="right">
      1
    </td>
    
    <td align="left">
       
    </td>
  </tr>
  
  <tr>
    <td align="left" height="17">
      KEY0201
    </td>
    
    <td align="left">
      RULE002
    </td>
    
    <td align="right">
      1
    </td>
    
    <td align="left">
      GET_DATA
    </td>
    
    <td align="left">
       
    </td>
    
    <td align="left">
      /foo/bar/x
    </td>
  </tr>
  
  <tr>
    <td align="left" height="17">
      KEY0202
    </td>
    
    <td align="left">
      RULE002
    </td>
    
    <td align="right">
      2
    </td>
    
    <td align="left">
      DIV
    </td>
    
    <td align="left">
       
    </td>
    
    <td align="left">
       
    </td>
  </tr>
  
  <tr>
    <td align="left" height="17">
      KEY0203
    </td>
    
    <td align="left">
      RULE002
    </td>
    
    <td align="right">
      3
    </td>
    
    <td align="left">
      CONSTANT
    </td>
    
    <td align="right">
      1
    </td>
    
    <td align="left">
       
    </td>
  </tr>
  
  <tr>
    <td align="left" height="17">
      KEY0204
    </td>
    
    <td align="left">
      RULE002
    </td>
    
    <td align="right">
      4
    </td>
    
    <td align="left">
      GET_DATA
    </td>
    
    <td align="left">
       
    </td>
    
    <td align="left">
      /foo/bar/y
    </td>
  </tr>
  
  <tr>
    <td align="left" height="17">
      KEY0205
    </td>
    
    <td align="left">
      RULE002
    </td>
    
    <td align="right">
      5
    </td>
    
    <td align="left">
      DIV
    </td>
    
    <td align="left">
       
    </td>
    
    <td align="left">
       
    </td>
  </tr>
  
  <tr>
    <td align="left" height="17">
      KEY0206
    </td>
    
    <td align="left">
      RULE002
    </td>
    
    <td align="right">
      6
    </td>
    
    <td align="left">
      PLUS
    </td>
    
    <td align="left">
       
    </td>
    
    <td align="left">
       
    </td>
  </tr>
  
  <tr>
    <td align="left" height="17">
      KEY0207
    </td>
    
    <td align="left">
      RULE002
    </td>
    
    <td align="right">
      7
    </td>
    
    <td align="left">
      CONSTANT
    </td>
    
    <td align="right">
      2
    </td>
    
    <td align="left">
       
    </td>
  </tr>
  
  <tr>
    <td align="left" height="17">
      KEY0208
    </td>
    
    <td align="left">
      RULE002
    </td>
    
    <td align="right">
      8
    </td>
    
    <td align="left">
      DIV
    </td>
    
    <td align="left">
       
    </td>
    
    <td align="left">
       
    </td>
  </tr>
</table>

Evaluating a sequence of operations is straightforward. Start with an empty stack (in Python, this is just a normal list). If the next operation is CONSTANT or GET_DATA, push the value onto the stack. Otherwise, an operation like PLUS will need two operands, so pop two things off the stack and then do that actual operation. As a bonus, we can render a normal mathematical expression as we go: instead of putting a floating point number onto the stack, put a string onto the stack.

Here is the entire evaluator:

{% highlight python %}
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
{% endhighlight %}

Just for fun, we will also evaluate the example from the Wikipedia page on Reverse Polish notation:

<table border="0" cellspacing="0">
  <colgroup width="68"></colgroup> <colgroup width="87"></colgroup> <colgroup width="59"></colgroup> <colgroup width="84"></colgroup> <colgroup width="58"></colgroup> <colgroup width="109"></colgroup><tr>
    <td align="left" height="17">
      ID
    </td>
    
    <td align="left">
      OUTPUT_ID
    </td>
    
    <td align="left">
      ORDER
    </td>
    
    <td align="left">
      OP
    </td>
    
    <td align="left">
      CONST
    </td>
    
    <td align="left">
      DATA_SOURCE
    </td>
  </tr>
  
  <tr>
    <td align="left" height="17">
      KEY0101
    </td>
    
    <td align="left">
      RULE003
    </td>
    
    <td align="right">
    </td>
    
    <td align="left">
      CONSTANT
    </td>
    
    <td align="right">
      15
    </td>
    
    <td align="left">
       
    </td>
  </tr>
  
  <tr>
    <td align="left" height="17">
      KEY0102
    </td>
    
    <td align="left">
      RULE003
    </td>
    
    <td align="right">
      1
    </td>
    
    <td align="left">
      CONSTANT
    </td>
    
    <td align="right">
      7
    </td>
    
    <td align="left">
       
    </td>
  </tr>
  
  <tr>
    <td align="left" height="17">
      KEY0103
    </td>
    
    <td align="left">
      RULE003
    </td>
    
    <td align="right">
      2
    </td>
    
    <td align="left">
      CONSTANT
    </td>
    
    <td align="right">
      1
    </td>
    
    <td align="left">
       
    </td>
  </tr>
  
  <tr>
    <td align="left" height="17">
      KEY0104
    </td>
    
    <td align="left">
      RULE003
    </td>
    
    <td align="right">
      3
    </td>
    
    <td align="left">
      CONSTANT
    </td>
    
    <td align="right">
      1
    </td>
    
    <td align="left">
       
    </td>
  </tr>
  
  <tr>
    <td align="left" height="17">
      KEY0105
    </td>
    
    <td align="left">
      RULE003
    </td>
    
    <td align="right">
      4
    </td>
    
    <td align="left">
      PLUS
    </td>
    
    <td align="left">
       
    </td>
    
    <td align="left">
       
    </td>
  </tr>
  
  <tr>
    <td align="left" height="17">
      KEY0106
    </td>
    
    <td align="left">
      RULE003
    </td>
    
    <td align="right">
      5
    </td>
    
    <td align="left">
      MINUS
    </td>
    
    <td align="left">
       
    </td>
    
    <td align="left">
       
    </td>
  </tr>
  
  <tr>
    <td align="left" height="17">
      KEY0107
    </td>
    
    <td align="left">
      RULE003
    </td>
    
    <td align="right">
      6
    </td>
    
    <td align="left">
      DIV
    </td>
    
    <td align="left">
       
    </td>
    
    <td align="left">
       
    </td>
  </tr>
  
  <tr>
    <td align="left" height="17">
      KEY0108
    </td>
    
    <td align="left">
      RULE003
    </td>
    
    <td align="right">
      7
    </td>
    
    <td align="left">
      CONSTANT
    </td>
    
    <td align="right">
      3
    </td>
    
    <td align="left">
       
    </td>
  </tr>
  
  <tr>
    <td align="left" height="17">
      KEY0109
    </td>
    
    <td align="left">
      RULE003
    </td>
    
    <td align="right">
      8
    </td>
    
    <td align="left">
      MUL
    </td>
    
    <td align="left">
       
    </td>
    
    <td align="left">
       
    </td>
  </tr>
  
  <tr>
    <td align="left" height="17">
      KEY0110
    </td>
    
    <td align="left">
      RULE003
    </td>
    
    <td align="right">
      9
    </td>
    
    <td align="left">
      CONSTANT
    </td>
    
    <td align="right">
      2
    </td>
    
    <td align="left">
       
    </td>
  </tr>
  
  <tr>
    <td align="left" height="17">
      KEY0111
    </td>
    
    <td align="left">
      RULE003
    </td>
    
    <td align="right">
      10
    </td>
    
    <td align="left">
      CONSTANT
    </td>
    
    <td align="right">
      1
    </td>
    
    <td align="left">
       
    </td>
  </tr>
  
  <tr>
    <td align="left" height="17">
      KEY0112
    </td>
    
    <td align="left">
      RULE003
    </td>
    
    <td align="right">
      11
    </td>
    
    <td align="left">
      CONSTANT
    </td>
    
    <td align="right">
      1
    </td>
    
    <td align="left">
       
    </td>
  </tr>
  
  <tr>
    <td align="left" height="17">
      KEY0113
    </td>
    
    <td align="left">
      RULE003
    </td>
    
    <td align="right">
      12
    </td>
    
    <td align="left">
      PLUS
    </td>
    
    <td align="left">
       
    </td>
    
    <td align="left">
       
    </td>
  </tr>
  
  <tr>
    <td align="left" height="17">
      KEY0114
    </td>
    
    <td align="left">
      RULE003
    </td>
    
    <td align="right">
      13
    </td>
    
    <td align="left">
      PLUS
    </td>
    
    <td align="left">
       
    </td>
    
    <td align="left">
       
    </td>
  </tr>
  
  <tr>
    <td align="left" height="17">
      KEY0153
    </td>
    
    <td align="left">
      RULE003
    </td>
    
    <td align="right">
      14
    </td>
    
    <td align="left">
      MINUS
    </td>
    
    <td align="left">
       
    </td>
    
    <td align="left">
       
    </td>
  </tr>
</table>

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
