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

from sailing import *

from random import random
from scipy import sqrt

class IncDict(dict):
    """
    For code where we update a count in a dictionary, we often need this
    kind of thing to avoid throwing an exception:

    d = {}
    try: d['a'] += 1
    except KeyError: d['a'] = 0 

    The IncDict class makes __getitem__ return 0 for any key that is
    not in the dictionary, and so we can do this instead:

    >>> d = IncDict()
    >>> print d
    {}
    >>> d['a'] += 1
    >>> print d
    {'a': 1}
    >>> d['a'] += 1
    >>> print d
    {'a': 2}
    """

    def __getitem__(self, y):
        if self.has_key(y): return dict.__getitem__(self, y)
        else: return 0

class SailingUCT(Sailing):
    def __init__(self, wind_array, costs, lake_size, end_x, end_y):
        Sailing.__init__(self, wind_array, costs, lake_size, end_x, end_y)
        self.edge_labels = {}

    def select_action_uct(self, state, depth):
        best_action = None
        best_action_val = None

        # ooh, I don't actually choose uniformly at random, I try in
        # order! Implementation detail!!
        for action in range(8):
            if not self.stays_in_lake(state, action): continue
            if self.node_action_counts[(depth, state, action)] == 0:
                return action

        for action in range(8):
            if not self.stays_in_lake(state, action): continue
            nr_action_selected = self.node_action_counts[(depth, state, action)]

            assert nr_action_selected > 0

            average_cost = self.average_cost[(depth, state, action)]
            nr_state_visits = self.node_visit_counts[(depth, state)]

            UCT_FACTOR = 1.0

            this_val = average_cost \
                        + UCT_FACTOR*sqrt(2.0*nr_state_visits/nr_action_selected)

            if best_action is None:
                best_action = action
                best_action_val = this_val
            elif this_val < best_action_val:
                best_action = action
                best_action_val = this_val

        assert best_action is not None

        return best_action

    def search_init(self, initial_state):
        """
        setup search stuff
        """

        self.nr_samples = 0

        self.initial_state = initial_state
        self.nr_rollouts = 0

        self.state_visit_counts = IncDict()
        self.node_visit_counts = IncDict()
        self.node_action_counts = IncDict()
        self.average_cost = IncDict()

        self.edge_labels = {}
        self.tree = nx.DiGraph()
        self.tree.add_node((0, initial_state))

    def run_rollout(self):
        self.aborted_search = False # mmm, messy
        cost = self._search(self.initial_state, 0)
        self.nr_rollouts += 1

        if self.aborted_search: return None
        return cost

    def _search(self, state, depth):
        """
        Internal function for running UCT search.
        """

        if self.is_terminal(state):
            #print "hit target", self.nr_samples 
            return 0

        """
        If we need to cut off simulations at
        some depth we can do so here.

        fixme: from UCT paper, they stopped episodes with
        probability 1/N_s(t), which is presumably the number of times
        that state s has been visited up to time t.

        if is_leaf(tree, state):
            # evaluate state?
        """

        action = self.select_action_uct(state, depth)

        # if we were doing MC rollouts:
        # action = self.choose_action_uniformly_at_random(state)

        new_state, cost = self._sample_next_state(state, action)
        self.nr_samples += 1

        node = (depth, state)
        new_node = (depth + 1, new_state)

        self.tree.add_edge(node, new_node)

        q = cost + (1.0)*self._search(new_state, depth + 1)

        self.state_visit_counts[state] += 1
        self.node_visit_counts[node] += 1

        old_average = self.average_cost[(depth, state, action)]
        n = self.node_visit_counts[(depth, state)]
        new_average = ((n - 1)*old_average + q)/(1.0*n)

        self.average_cost[(depth, state, action)] = new_average
        self.node_action_counts[(depth, state, action)] += 1

        try:
            if q < self.edge_labels[(node, new_node)][1]:
                self.edge_labels[(node, new_node)] = action, q
        except KeyError:
            self.edge_labels[(node, new_node)] = action, q

        return q

####################################################

# same example from sailing.py:

def main(lake_size):
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

    # Grab the pre-computed optimal solution.
    lake_filename = 'lake_' + str(lake_size) + '.pkl'

    pkl_file = open(lake_filename, 'rb')
    V_opt = pickle.load(pkl_file)
    policy_opt = pickle.load(pkl_file)
    V_avg_opt = pickle.load(pkl_file)
    V_stddev_opt = pickle.load(pkl_file)
    pkl_file.close()

    total_nr_samples = 0

    for _ in range(100):
        print "yarrr"
        sys.stdout.flush()

        S = SailingUCT(wind_array, costs, lake_size, end_x, end_y)

        w1 = my_randint(8)
        d = my_randint(8)
        w2 = S.new_wind(w1)

        initial_state = (my_randint(lake_size), my_randint(lake_size), d, w1, w2)

        S.search_init(initial_state)

        optimal_cost = V_opt[initial_state]

        min_cost = None
        while True:
            cost = S.run_rollout()
            if cost is None: continue

            if min_cost is None or cost < min_cost:
                min_cost = cost

            if min_cost < optimal_cost or min_cost - 0.1 <= optimal_cost: break

        print min_cost, S.nr_samples

        total_nr_samples += S.nr_samples

    print
    print "lake_size total_nr_samples for error < 0.1 of optimal"
    print lake_size, total_nr_samples

if __name__ == "__main__":
    if len(sys.argv) == 2:
        eval(sys.argv[1])
