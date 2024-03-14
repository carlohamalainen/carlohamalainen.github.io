# IO actions on integers:
#
# data IOAction = Return Int
#               | Put Int (IOAction Int)
#               | Get (Int -> IOAction Int)
#
# Implement using nested lists, e.g.
# 
# [Return, 3]       == Return 3
# 
# Put 42 [Return 0] == [Put, 42, [Return, 0]]

Return = 'Return'
Put    = 'Put'
Get    = 'Get'

put = lambda x: [Put, x, [Return, None]]

get = [Get, lambda x: [Return, x]]


def seq(h, f):
    """
    Sequence the IOActions h and f.
    """

    if h[0] == Return:  return f(h[1])

    if h[0] == Get:     return [Get, lambda z: seq(h[1](z), f)]

    if h[0] == Put:     return [Put, h[1], seq(h[2], f)]

    assert False

get_put = seq(get, put)

def run(io):
    """
    Run an IOAction.
    """

    # print 'run: ' + str(io)

    if io[0] == Return:
        return io[1]

    if io[0] == Put:
        print io[1]
        return run(io[2])

    if io[0] == Get:
        value = int(input('? '))
        return run(io[1](value))

    assert False


run(seq(put(-4), lambda _: seq(get, lambda x: put(x + 1))))

# commands = [[put(3)],
#             ['x', '<-', get],
#             [put('x' + 1)]]










