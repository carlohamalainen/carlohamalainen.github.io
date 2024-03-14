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
from scipy import log, sqrt
from incdict import IncDict

class SailingPlanner(Sailing):
    def __init__(self, lake_size):
        Sailing.__init__(self, lake_size)

    def random_action(self, state):
        possible_actions = []

        for action in range(8):
            if self.is_into(state, action): continue
            if not self.stays_in_lake(state, action): continue

            possible_actions.append(action)

        assert len(possible_actions) > 0

        return possible_actions[my_randint(len(possible_actions))]

    def tree_policy(self, state):
        return self.best_Q_value(state)[0]

    def select_action(self, state):
        if random() < 0.01:
            return self.random_action(state)

        action = self.tree_policy(state)
        if action is None: action = self.random_action(state)

        return action

    def search_init(self, initial_state):
        self.nr_samples = 0

        self.initial_state = initial_state

        # keys: state
        # values: how many times we have visited this state during the
        # searches.
        self.state_visit_counts = IncDict()

        # keys: (state, action) tuples
        # values: how many times we have taken 'action' from 'state'
        self.state_action_counts = IncDict()

        # keys: (state, action) tuples
        # values: average cost of taking 'action' from 'state'.
        self.Q = {}

    def search(self, state, depth = 0):
        if self.is_terminal(state): return 0

        action = self.select_action(state)
        new_state, cost = self.sample_next_state(state, action)

        if random() < 1.0/(self.state_visit_counts[(state)] + 1):
            try: q = cost + self.gamma*self.Q[(new_state, action)]
            except KeyError: q = cost + self.gamma*self.V_approx[new_state]
        else:
            q = cost + self.gamma*self.search(new_state, depth + 1)

        assert q != 0

        self.state_visit_counts[(state)] += 1
        self.nr_samples += 1
        self.state_action_counts[(state, action)] += 1

        try:
            old_average = self.Q[(state, action)]
            n = self.state_action_counts[(state, action)]
            #new_average = old_average + (1.0/n)*(q - old_average)
            new_average = old_average + (0.5)*(q - old_average)
        except KeyError:
            new_average = q

        self.Q[(state, action)] = new_average
        return q

    def best_Q_value(self, state):
        best_avg = None
        best_action = None

        for action in range(8):
            try:
                average_cost = self.Q[(state, action)]
            except KeyError:
                continue

            assert average_cost != 0

            if best_avg is None or average_cost < best_avg:
                best_avg = average_cost
                best_action = action

        return best_action, best_avg

####################################################

def sailing_mc_planner(lake_size, V_optimal, V_approx, initial_state, max_nr_samples):
    S = SailingPlanner(lake_size)
    S.V_approx = V_approx

    S.search_init(initial_state)

    S.search(initial_state)

    optimal_cost = V_optimal[initial_state]

    while True:
        _, min_cost = S.best_Q_value(initial_state)

        error = abs(min_cost - optimal_cost)

        if error < 0.1: break

        q = S.search(initial_state)

        if S.nr_samples > max_nr_samples:
            return None

    return S.nr_samples
