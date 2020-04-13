# -*- coding: utf-8 -*-

#*****************************************************************************
#       Copyright (C) 2009 Carlo Hamalainen <carlo.hamalainen@gmail.com>, 
#
#  Distributed under the terms of the GNU General Public License (GPL)
#
#    This code is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#    General Public License for more details.
#
#  The full text of the GPL is available at:
#
#                  http://www.gnu.org/licenses/
#*****************************************************************************

"""
A Markov Decision Process (MDP) for the iPod problem 
described at http://norvig.com/ipod.html
"""

import random
import sys

def value_iteration(N, T, target, epsilon = 0.001):
    # The possible actions from any state:
    actions = ['sequential', 'shuffle']

    # Transition probabilities:
    # transitions[s, a, w] = probability of moving from s to w by action a.

    transitions = {}

    # Sequential strategy gets us to the target directly.
    for s in range(N):
        for w in range(N):
            if w == target:
                transitions[s, 'sequential', w] = 1.0
            else:
                transitions[s, 'sequential', w] = 0.0

    # The Shuffle strategy takes us to any state with equal
    # probability.
    for s in range(N):
        for w in range(N):
            transitions[s, 'shuffle', w] = 1.0/N

    # Cost of each action from any state s.
    cost = {}
    for s in range(N): cost[s, 'sequential'] = abs(target - s)
    for s in range(N): cost[s, 'shuffle'] = T

    V1 = [0] * N
    V2 = [1] * N
    policy = ['shuffle'] * N

    while max([abs(V1[i] - V2[i]) for i in range(N)]) > epsilon:
        for s in range(N):
            min_action = actions[0]
            min_action_cost = cost[s, actions[0]] \
                + sum([transitions[s, actions[0], w]*V1[w] for w in range(N)])

            for a in actions:
                this_cost = cost[s, a] + sum([transitions[s, a, w]*V1[w] \
                    for w in range(N)])

                if this_cost < min_action_cost:
                    min_action = a
                    min_action_cost = this_cost

            V2[s] = min_action_cost
            policy[s] = min_action

        V1, V2 = V2, V1   # swapsies

    try:
        p = min([s for s in range(N) if policy[s] == 'sequential']) - 1
        q = min([s for s in range(N) if V2[s] == target - s]) - 1

        # fixme: fails if epsilon is too large, ie. our policy vector isn't
        # optimal.
        # assert p == q

        p = min([s for s in range(N) if V2[s] == target - s]) - 1

        assert policy[p] == 'shuffle'
        assert V2[p] != target - p

        assert policy[p + 1] == 'sequential'
        assert V2[p + 1] == target - (p + 1)

        assert policy[p + 2] == 'sequential'
        assert V2[p + 2] == target - (p + 2)
    except ValueError:
        # we must have come in with a high epsilon value and didn't get
        # a 'correct' value/policy vector.
        p = None

    return V2, policy, p

def simulate(N, T, initial_state, target, policy):
    s = initial_state
    total_cost = 0

    while s != target:
        if policy[s] == 'sequential':
            total_cost += abs(target - s)
            s = target
        else: # policy[s] == 'shuffle'
            total_cost += T
            s = random.randrange(N)

    return total_cost

def average_simulation(N, T, policy):
    # The target state:
    target = N/2

    nr_iters = 1000*N

    return sum([simulate(N, T, random.randrange(N), target, policy) \
        for _ in range(nr_iters)])/float(nr_iters)

def usage():
    print
    print "Usage:"
    print "$ python ipod_mdp.py <N> <T>"
    print
    sys.exit(0)


if __name__ == "__main__":
    if len(sys.argv) == 1: usage()

    if len(sys.argv) == 3:
        N = int(sys.argv[1]) 
        T = float(sys.argv[2]) 

        V, policy, p = value_iteration(N, T, N/2)

        away = N/2 - p

        assert policy[N/2 - (away - 1)] == 'sequential'
        assert policy[N/2 - (away)] == 'shuffle'
        assert policy[N/2 - (away + 1)] == 'shuffle'

        mean_V = float(sum(V))/float(len(V))
        mean_sim = average_simulation(N, T, policy)

        print "mean(V) =", mean_V
        print "mean (simulation):", mean_sim
        print "difference:", mean_V - mean_sim

        print "shuffle when:", away, "or more away"

        sys.exit(0)

    usage()
