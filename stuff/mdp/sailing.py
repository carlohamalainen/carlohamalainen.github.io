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


# For debugging, put these lines somewhere to drop into an ipython shell:
#import IPython
#IPython.Shell.IPShell(user_ns=dict(globals(), **locals())).mainloop()

# To profile, put these in __main__():
#import cProfile
#cProfile.run('thing_to_run()')

import math
import pickle
import random
import scipy
import sys

from scipy import arange, ceil, log, logn
from scipy.stats import rv_discrete

import networkx as nx

def mean(L): return sum(L)/(1.0*len(L))

def stddev(values, meanval=None):
    # copied from http://aima.cs.berkeley.edu/python/utils.html
    # and fixed the denominator.
    """The standard deviation of a set of values.
    Pass in the mean if you already know it."""
    if meanval == None: meanval = mean(values)
    return math.sqrt(sum([(x - meanval)**2 for x in values]) / (len(values)))

def median(values):
    # copied from http://aima.cs.berkeley.edu/python/utils.html
    """Return the middle value, when the values are sorted.
    If there are an odd number of elements, try to average the middle two.
    If they can't be averaged (e.g. they are strings), choose one at random.
    >>> median([10, 100, 11])
    11
    >>> median([1, 2, 3, 4])
    2.5
    """
    n = len(values)
    values = sorted(values)
    if n % 2 == 1:
        return values[n/2]
    else:
        middle2 = values[(n/2)-1:(n/2)+1]
        try:
            return mean(middle2)
        except TypeError:
            return random.choice(middle2)

def my_randint(n):
    """
    Return a random integer from [0, n).
    """

    return random.randint(0, n - 1)

def add_vector(x, y, v):
    """
    Returns (x + v[0], y + v[1]).

    EXAMPLES::

        >>> add_vector(0, 0, (0, 1))
        (0, 1)
        >>> add_vector(0, 0, (-2, 1))
        (-2, 1)
    """
    return (x + v[0], y + v[1])

def check_probability_matrix(P):
    """
    Rows must sum to 1 and no entry can be negative.

    EXAMPLE::

        >>> wind_array = [ [0.4, 0.3, 0.0, 0.0, 0.0, 0.0, 0.0, 0.3], \
                           [0.4, 0.3, 0.3, 0.0, 0.0, 0.0, 0.0, 0.0], \
                           [0.0, 0.4, 0.3, 0.3, 0.0, 0.0, 0.0, 0.0], \
                           [0.0, 0.0, 0.4, 0.3, 0.3, 0.0, 0.0, 0.0], \
                           [0.0, 0.0, 0.0, 0.4, 0.2, 0.4, 0.0, 0.0], \
                           [0.0, 0.0, 0.0, 0.0, 0.3, 0.3, 0.4, 0.0], \
                           [0.0, 0.0, 0.0, 0.0, 0.0, 0.3, 0.3, 0.4], \
                           [0.4, 0.0, 0.0, 0.0, 0.0, 0.0, 0.3, 0.3] ]
        >>> check_probability_matrix(wind_array)
        True
    """

    for x in P:
        if sum(x) != 1: return False

        for y in x:
            if y < 0: return False

    return True

def abs_direction_difference(d1, d2):
    """
    Absolute difference in directions.

    EXAMPLES::

        >>> abs_direction_difference(0, 1)
        1
        >>> abs_direction_difference(3, 2)
        1
        >>> abs_direction_difference(3, 5)
        2
        >>> abs_direction_difference(3, 7)
        4
    """
    #assert d1 in range(8)
    #assert d2 in range(8)

    assert d1 >= 0
    assert d1 < 8

    assert d2 >= 0
    assert d2 < 8

    x = abs(d1 - d2)

    if x < 8 - x:   return x
    else:           return 8 - x

def tack(boat_direction, wind_direction):
    """
    The tack of the boat depends on the relative difference of
    the boat's direction and the wind.

    EXAMPLES::

        >>> tack(0, 0)
        'away'
        >>> tack(0, 7)
        'down'
        >>> tack(0, 2)
        'cross'
        >>> tack(0, 3)
        'up'
        >>> tack(0, 4)
        'into'
    """

    assert boat_direction >= 0
    assert boat_direction < 8

    assert wind_direction >= 0
    assert wind_direction < 8

    d = abs_direction_difference(boat_direction, wind_direction)

    if d == 0: return 'away'
    if d == 1: return 'down'
    if d == 2: return 'cross'
    if d == 3: return 'up'
    if d == 4: return 'into'

    raise ValueError

def direction_vector(d):
    """
    The direction 0 is north so we move by (0, 1) in cartesian
    coordinates.

    EXAMPLES::

        >>> direction_vector(0) # north
        (0, 1)
        >>> direction_vector(6) # west
        (-1, 0)
    """

    # assert d in range(8)
    assert d >= 0
    assert d < 8

    if d == 0: return (0, 1)
    if d == 1: return (1, 1)
    if d == 2: return (1, 0)
    if d == 3: return (1, -1)
    if d == 4: return (0, -1)
    if d == 5: return (-1, -1)
    if d == 6: return (-1, 0)
    if d == 7: return (-1, 1)

class Sailing:
    def __init__(self, wind_array, costs, lake_size, end_x, end_y):
        assert end_x in range(lake_size)
        assert end_y in range(lake_size)
        self.end_x = end_x
        self.end_y = end_y
        self.wind_array = wind_array

        self.wind_distribution = []
        for i in range(len(wind_array)):
            vals = [arange(len(wind_array[i])), wind_array[i]]
            self.wind_distribution.append(rv_discrete(name='custm', \
                                                    values=vals))

        self.costs = costs
        self.lake_size = lake_size

        self.states = []
        for x in range(self.lake_size):
            for y in range(self.lake_size):
                for d in range(8):
                    for w1 in range(8):
                        for w2 in range(8):
                            self.states.append((x, y, d, w1, w2))

        self.wind_array_c = wind_array

    def is_terminal(self, state):
        return (state[0], state[1]) == (self.end_x, self.end_y)

    def stays_in_lake(self, state, action):
        x, y, _, _, _ = state
        x2, y2 = add_vector(x, y, direction_vector(action))

        if x2 in range(self.lake_size) and y2 in range(self.lake_size):
            return True

        return False

    def average_cost_of_transition(self, V, s, new_d):
        x, y, _, _, w2 = s

        x2, y2 = add_vector(x, y, direction_vector(new_d))

        if not self.stays_in_lake(s, new_d): return None

        this_cost = 0

        # We perform the local action
        this_cost += self.cost(s, new_d)

        for w3 in range(8):
            s_new = (x2, y2, new_d, w2, w3)

            this_cost += self.transition_probability(s, s_new)*V[s_new] 

        return this_cost

    def transition_probability(self, s1, s2):
        """
        What is the probability of moving from state s1 to s2?

        s1 = (x1, y1, _, w1, w2)
        s2 = (x2, y2, d2, w2_2, w3)

        The boat was at (x1, y1) and travelled to (x2, y2). Then it must
        be the case that (x2, y2) = (x1, y1) + direction_vector(d2). If this
        does not hold then the probability is 0.

        If the previous wind direction of s2 does not match the new
        wind direction of s1 then the probability is 0 (so we need 
        w2_2 == w2).

        Finally, the probability of moving from s1 to s2 is just the
        probability of the wind changing from w2=w2_2 to w3, which is 
        stored in the global variable wind_array.

        >>> x1, y1 = 1, 1
        >>> x2, y2 = 2, 1
        >>> d1 = 0
        >>> d2 = 2 # the direction that we just travelled
        >>> w1 = 3
        >>> w2 = 2
        >>> w2_2 = w2

        >>> wind_array = [ [0.4, 0.3, 0.0, 0.0, 0.0, 0.0, 0.0, 0.3], \
                           [0.4, 0.3, 0.3, 0.0, 0.0, 0.0, 0.0, 0.0], \
                           [0.0, 0.4, 0.3, 0.3, 0.0, 0.0, 0.0, 0.0], \
                           [0.0, 0.0, 0.4, 0.3, 0.3, 0.0, 0.0, 0.0], \
                           [0.0, 0.0, 0.0, 0.4, 0.2, 0.4, 0.0, 0.0], \
                           [0.0, 0.0, 0.0, 0.0, 0.3, 0.3, 0.4, 0.0], \
                           [0.0, 0.0, 0.0, 0.0, 0.0, 0.3, 0.3, 0.4], \
                           [0.4, 0.0, 0.0, 0.0, 0.0, 0.0, 0.3, 0.3] ]
        >>> lake_size = 5
        >>> end_x = 4
        >>> end_y = 4
        >>> costs = { 'into':0, 'up':1, 'cross':2, \
                             'down':3, 'away':4 }

        >>> S = Sailing(wind_array, costs, lake_size, end_x, end_y)

        >>> S.transition_probability((x1, y1, d1, w1, w2), \
                                    (x2, y2, d2, w2_2, 1))
        0.40000000000000002
        >>> S.transition_probability((x1, y1, d1, w1, w2), \
                                    (x2, y2, d2, w2_2, 0))
        0.0
        """

        x1, y1, _, w1, w2 = s1
        x2, y2, d2, w2_2, w3 = s2

        d_vec1, d_vec2 = direction_vector(d2)

        # The boat was at (x1,y1) and travelled in
        # direction d2 to arrive at (x2, y2).
        if x2 != x1 + d_vec1: return 0
        if y2 != y1 + d_vec2: return 0

        # The new wind direction for s1 must be the
        # previous wind direction of s2.
        if w2 != w2_2: return 0

        # Now we just have the probability of going from
        # wind direction w2 to wind direction w3.

        return self.wind_array_c[w2][w3]

    def new_wind(self, w):
        """
        The wind is currently blowing in direction w and it changes to a
        new direction according to the matrix wind_array, which is encoded
        by general probability distribution in wind_probability_space.

        EXAMPLES::

            >>> wind_array = [ [0.4, 0.3, 0.0, 0.0, 0.0, 0.0, 0.0, 0.3], \
                               [0.4, 0.3, 0.3, 0.0, 0.0, 0.0, 0.0, 0.0], \
                               [0.0, 0.4, 0.3, 0.3, 0.0, 0.0, 0.0, 0.0], \
                               [0.0, 0.0, 0.4, 0.3, 0.3, 0.0, 0.0, 0.0], \
                               [0.0, 0.0, 0.0, 0.4, 0.2, 0.4, 0.0, 0.0], \
                               [0.0, 0.0, 0.0, 0.0, 0.3, 0.3, 0.4, 0.0], \
                               [0.0, 0.0, 0.0, 0.0, 0.0, 0.3, 0.3, 0.4], \
                               [0.4, 0.0, 0.0, 0.0, 0.0, 0.0, 0.3, 0.3] ]
            >>> lake_size = 5
            >>> end_x = 4
            >>> end_y = 4
            >>> costs = { 'into':0, 'up':1, 'cross':2, \
                                 'down':3, 'away':4 }

            >>> S = Sailing(wind_array, costs, lake_size, end_x, end_y)
            >>> S.new_wind(0) in range(8)
            True
            >>> S.new_wind(4) in range(8)
            True
        """

        #assert w in range(8)

        #return self.wind_distribution[w].get_random_element()
        return self.wind_distribution[w].rvs()

    def cost(self, s, d):
        """
        If we are in state s and we decide to travel in direction d, how
        much will this cost? Note that the wind for this new leg is in the 
        last element of s.

        EXAMPLES::

            >>> x1, y1 = 1, 1
            >>> x2, y2 = 2, 1
            >>> d1 = 0
            >>> d2 = 2
            >>> w1 = 3
            >>> w2 = 2
            >>> s = (x1, y1, d1, w1, w2)

            >>> wind_array = [ [0.4, 0.3, 0.0, 0.0, 0.0, 0.0, 0.0, 0.3], \
                               [0.4, 0.3, 0.3, 0.0, 0.0, 0.0, 0.0, 0.0], \
                               [0.0, 0.4, 0.3, 0.3, 0.0, 0.0, 0.0, 0.0], \
                               [0.0, 0.0, 0.4, 0.3, 0.3, 0.0, 0.0, 0.0], \
                               [0.0, 0.0, 0.0, 0.4, 0.2, 0.4, 0.0, 0.0], \
                               [0.0, 0.0, 0.0, 0.0, 0.3, 0.3, 0.4, 0.0], \
                               [0.0, 0.0, 0.0, 0.0, 0.0, 0.3, 0.3, 0.4], \
                               [0.4, 0.0, 0.0, 0.0, 0.0, 0.0, 0.3, 0.3] ]
            >>> lake_size = 5
            >>> end_x = 4
            >>> end_y = 4
            >>> costs = { 'into':0, 'up':1, 'cross':2, \
                                 'down':3, 'away':4 }

            >>> S = Sailing(wind_array, costs, lake_size, end_x, end_y)
            >>> S.cost(s, 0)
            2
            >>> S.cost(s, 1)
            3
            >>> S.cost(s, 6)
            0
        """

        new_wind = s[-1]

        return self.costs[tack(d, new_wind)]

    def best_action(self, s, V):
        """
        If we are in state s, use the value vector V to work out the
        best direction to travel in and its estimated cost.
        """

        x, y, d, w1, w2 = s

        # this is the end state
        if self.is_terminal(s):
            return (-1, 0) # (no action, zero cost)

        # Otherwise we have to loop through all possible actions
        # and find the one with the minimum cost.

        min_d = None
        min_d_cost = None

        for new_d in range(8):
            new_d_cost = self.average_cost_of_transition(V, s, new_d)
            if new_d_cost == None: continue

            if min_d is None:
                min_d = new_d
                min_d_cost = new_d_cost
            elif new_d_cost < min_d_cost:
                min_d = new_d
                min_d_cost = new_d_cost

        return (min_d, min_d_cost)

    def value_iteration(self, epsilon, one_iteration = False):
        V1 = {}
        V2 = {}
        policy = {}
        for s in self.states:
            V1[s] = 0
            V2[s] = 10*epsilon
            policy[s] = -1

        while True:
            max_diff = max([abs(V1[i] - V2[i]) for i in self.states])
            print "Top of value_iteration(), max difference: %.1f" % max_diff
            sys.stdout.flush()

            if max_diff < epsilon:
                break

            for s in self.states:
                policy[s], V2[s] = self.best_action(s, V1)

            V1, V2 = V2, V1

            if one_iteration: break

        V = V2

        V_avg = sum(V.values())/len(V)
        V_stddev = stddev(V.values(), meanval = V_avg)

        return V, policy, V_avg, V_stddev

    def simulate(self, policy):
        w1 = my_randint(8)
        d = my_randint(8)
        w2 = self.new_wind(w1)

        current_state = (my_randint(self.lake_size), \
                        my_randint(self.lake_size), d, w1, w2)

        this_cost = 0

        while not self.is_terminal(current_state):
            new_d = policy[current_state]

            this_cost += self.cost(current_state, new_d)

            x2, y2 = add_vector(current_state[0], current_state[1], \
                direction_vector(new_d))
            w3 = self.new_wind(current_state[-1])
            current_state = x2, y2, new_d, current_state[-1], w3

        return this_cost

    def run_simulations(self, policy, nr_sims = 10000):
        nr_sims = 10000
        sims = [self.simulate(policy) for _ in range(nr_sims)]

        sims_avg = sum(sims)/len(sims)
        sims_stddev = stddev(sims, meanval = sims_avg)

        return sims, sims_avg, sims_stddev

    def choose_action_uniformly_at_random(self, current_state):
        x, y, _, _, _ = current_state

        while True:
            new_d = my_randint(8)
            x2, y2 = add_vector(x, y, direction_vector(new_d))

            if x2 in range(self.lake_size) and y2 in range(self.lake_size):
                return new_d

    def _sample_next_state(self, current_state, action):
        """
        Use the generative model of S to find the next state given that
        we are in current_state and take action action.
        """

        if self.is_terminal(current_state): 
            return None

        cost = self.cost(current_state, action)
        x2, y2 = add_vector(current_state[0], current_state[1], direction_vector(action))
        w3 = self.new_wind(current_state[-1])
        new_state = x2, y2, action, current_state[-1], w3

        return new_state, cost



def value_iteration_example():
    # Wind transition probabilities.
    wind_array = [ \
        [0.4, 0.3, 0.0, 0.0, 0.0, 0.0, 0.0, 0.3], \
        [0.4, 0.3, 0.3, 0.0, 0.0, 0.0, 0.0, 0.0], \
        [0.0, 0.4, 0.3, 0.3, 0.0, 0.0, 0.0, 0.0], \
        [0.0, 0.0, 0.4, 0.3, 0.3, 0.0, 0.0, 0.0], \
        [0.0, 0.0, 0.0, 0.4, 0.2, 0.4, 0.0, 0.0], \
        [0.0, 0.0, 0.0, 0.0, 0.3, 0.3, 0.4, 0.0], \
        [0.0, 0.0, 0.0, 0.0, 0.0, 0.3, 0.3, 0.4], \
        [0.4, 0.0, 0.0, 0.0, 0.0, 0.0, 0.3, 0.3] \
    ]

    lake_size = 5

    end_x = lake_size - 1
    end_y = lake_size - 1

    #costs = { 'into':-10, 'up':-4, 'cross':-3, 'down':-2, 'away':-1 }
    costs = { 'into':1000, 'up':4, 'cross':3, 'down':2, 'away':1 }

    # Run a value iteration algorithm:
    S = Sailing(wind_array, costs, lake_size, end_x, end_y)
    V, policy, V_avg, V_stddev = S.value_iteration(1.5)

    # Run a few thousand simulations:
    nr_sims = 10000
    sims, sims_avg, sims_stddev = S.run_simulations(policy)

    print "Value iteration:"
    print "    Mean cost: %.1f" % V_avg
    print "    Median cost: %.1f" % median(V.values())
    print "    Standard dev: %.1f" % V_stddev
    print
    print "Simulations (run %d times):" % nr_sims
    print "    Mean cost: %.1f" % sims_avg
    print "    Median cost: %.1f" % median(sims)
    print "    Standard dev: %.1f" % sims_stddev

    print
    v_11 = 0.0
    v_11_count = 0

    for s in V.keys():
        if (s[0], s[1]) == (1, 1):
            v_11_count += 1
            v_11 += V[s]

    print "Mean cost to sail across lake from (1, 1) to (%d, %d): %.1f" \
        % (S.end_x, S.end_y, (v_11/v_11_count))

def cross_lake_cost(lake_size):
    # Wind transition probabilities.
    wind_array = [ \
        [0.4, 0.3, 0.0, 0.0, 0.0, 0.0, 0.0, 0.3], \
        [0.4, 0.3, 0.3, 0.0, 0.0, 0.0, 0.0, 0.0], \
        [0.0, 0.4, 0.3, 0.3, 0.0, 0.0, 0.0, 0.0], \
        [0.0, 0.0, 0.4, 0.3, 0.3, 0.0, 0.0, 0.0], \
        [0.0, 0.0, 0.0, 0.4, 0.2, 0.4, 0.0, 0.0], \
        [0.0, 0.0, 0.0, 0.0, 0.3, 0.3, 0.4, 0.0], \
        [0.0, 0.0, 0.0, 0.0, 0.0, 0.3, 0.3, 0.4], \
        [0.4, 0.0, 0.0, 0.0, 0.0, 0.0, 0.3, 0.3] \
    ]

    end_x = lake_size - 1
    end_y = lake_size - 1

    #costs = { 'into':-10, 'up':-4, 'cross':-3, 'down':-2, 'away':-1 }
    costs = { 'into':1000, 'up':4, 'cross':3, 'down':2, 'away':1 }

    # Run a value iteration algorithm:
    S = Sailing(wind_array, costs, lake_size, end_x, end_y)
    V, policy, V_avg, V_stddev = S.value_iteration(0.1)

    v_11 = 0.0
    v_11_count = 0

    for s in V.keys():
        if (s[0], s[1]) == (1, 1):
            v_11_count += 1
            v_11 += V[s]

    print "lake = %d x %d; mean cost to sail across lake from (1, 1) to (%d, %d): %.1f" \
        % (lake_size, lake_size, S.end_x, S.end_y, (v_11/v_11_count))

def save_optimal_solution(lake_size):
    # Wind transition probabilities.
    wind_array = [ \
        [0.4, 0.3, 0.0, 0.0, 0.0, 0.0, 0.0, 0.3], \
        [0.4, 0.3, 0.3, 0.0, 0.0, 0.0, 0.0, 0.0], \
        [0.0, 0.4, 0.3, 0.3, 0.0, 0.0, 0.0, 0.0], \
        [0.0, 0.0, 0.4, 0.3, 0.3, 0.0, 0.0, 0.0], \
        [0.0, 0.0, 0.0, 0.4, 0.2, 0.4, 0.0, 0.0], \
        [0.0, 0.0, 0.0, 0.0, 0.3, 0.3, 0.4, 0.0], \
        [0.0, 0.0, 0.0, 0.0, 0.0, 0.3, 0.3, 0.4], \
        [0.4, 0.0, 0.0, 0.0, 0.0, 0.0, 0.3, 0.3] \
    ]

    end_x = lake_size - 1
    end_y = lake_size - 1

    costs = { 'into':1000, 'up':4, 'cross':3, 'down':2, 'away':1 }

    S = Sailing(wind_array, costs, lake_size, end_x, end_y)
    V, policy, V_avg, V_stddev = S.value_iteration(0.1)

    lake_filename = "lake_" + str(lake_size) + ".pkl"

    output = open(lake_filename, 'wb')
    pickle.dump(V, output)
    pickle.dump(policy, output)
    pickle.dump(V_avg, output)
    pickle.dump(V_stddev, output)
    output.close()


if __name__ == "__main__":
    if len(sys.argv) == 2:
        eval(sys.argv[1])

