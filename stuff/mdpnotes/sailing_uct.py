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
from sailing_mc import SailingPlanner

class SailingUCT(SailingPlanner):
    def random_action_of_untried(self, state):
        possible_actions = []

        for action in range(8):
            if self.is_into(state, action): continue
            if not self.stays_in_lake(state, action): continue
            if self.Q.has_key((state, action)): continue

            possible_actions.append(action)

        if len(possible_actions) == 0: return None

        return possible_actions[my_randint(len(possible_actions))]

    def select_action_uct(self, state):
        # If there is an untried action, give that a go.
        action = self.random_action_of_untried(state)
        if action is not None: return action

        uct_best = None
        uct_best_action = None

        for action in range(8):
            if not self.Q.has_key((state, action)): continue

            average_reward = -1.0*self.Q[(state, action)]

            assert average_reward != 0

            n_s_a = self.state_action_counts[(state, action)]
            n_s = self.state_visit_counts[(state)]

            uct_factor = 15.0

            this_val = average_reward + uct_factor*sqrt(log(n_s)/n_s_a)

            if this_val > uct_best:
                uct_best = this_val
                uct_best_action = action

        return uct_best_action

    def tree_policy(self, state):
        return self.select_action_uct(state)
 
####################################################

def random_state(S):
    w1 = my_randint(8)
    while True: # we weren't sailing into the wind...
        d = my_randint(8)
        if tack(d, w1) != 'into': break
    w2 = S.new_wind(w1)

    while True:
        state = (my_randint(S.lake_size), my_randint(S.lake_size), d, w1, w2)
        if not S.is_terminal(state): break

    return state

def sailing_uct_planner(lake_size, V_optimal, V_approx, initial_state, max_nr_samples):
    S = SailingUCT(lake_size)
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

