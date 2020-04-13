import pickle
from random import random
import sys

from sailing import mean, Sailing
from sailing_mc import sailing_mc_planner
from sailing_uct import sailing_uct_planner

def main(lake_size, max_nr_samples):
    # Grab the pre-computed optimal solution.
    lake_filename = 'lake_' + str(lake_size) + '.pkl'

    pkl_file = open(lake_filename, 'rb')
    V_optimal = pickle.load(pkl_file)
    policy_opt = pickle.load(pkl_file)
    V_avg_opt = pickle.load(pkl_file)
    V_stddev_opt = pickle.load(pkl_file)
    pkl_file.close()

    V_approx = {}
    for state in V_optimal:
        if (state[0], state[1]) == (lake_size - 1, lake_size - 1):
            V_approx[state] = V_optimal[state]
        else:
            V_approx[state] = (1.0 + (2*0.1*random() - 0.1))*V_optimal[state]
            assert V_approx[state] > 0

    nr_simulations = 20

    mc_data = []
    uct_data = []

    # Just used for constructing initial states.
    S_dummy = Sailing(lake_size)

    mc_too_long = 0
    uct_too_long = 0

    for i in range(nr_simulations):
        initial_state = S_dummy.random_state()

        mc_data.append(sailing_mc_planner(lake_size, V_optimal, V_approx, initial_state, max_nr_samples))
        uct_data.append(sailing_uct_planner(lake_size, V_optimal, V_approx, initial_state, max_nr_samples))
        print mc_data[-1], uct_data[-1]
        sys.stdout.flush()

        if mc_data[-1] is None:
            mc_data[-1] = max_nr_samples
            mc_too_long += 1

        if uct_data[-1] is None:
            uct_data[-1] = max_nr_samples
            uct_too_long += 1

    print mc_data
    print uct_data
    print

    if True:
        print "Lake size: %d x %d" % (lake_size, lake_size)
        print "Mean number of samples for error < 0.1:"
        print "    Monte Carlo planning:", mean([x for x in mc_data if x is not None])
        print "    UCT:", mean([x for x in uct_data if x is not None])
        print
        print "Number of simulations that ran too long:"
        print "    Monte Carlo planning:", mc_too_long
        print "    UCT:", uct_too_long

if __name__ == "__main__":
    if len(sys.argv) == 2:
        eval(sys.argv[1])
    else:
        lake_size = 5
        print "Default: trying MC with lake size %d x %d" % (lake_size, lake_size)
        main(lake_size, 50000)



