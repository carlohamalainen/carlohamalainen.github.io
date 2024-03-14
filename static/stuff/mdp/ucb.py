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


import pylab
from scipy import arange, log, pi, sqrt
from scipy.stats import rv_discrete

def avg(L): return sum(L)/(1.0*len(L))

def make_scipy_rv(L):
    vals = [arange(len(L)), L]
    return rv_discrete(name='custm', values=vals)

"""
I want to have some number of machines, and the j-th machine has a spike
in the distribution at the j-th point.
"""

nr_machines = 10

machine_distributions = []
means = []
max_mean = None

for j in range(nr_machines):
    d = [1] * nr_machines
    d[j] = 20
   
    # normalise d
    d_sum = float(sum(d))
    d = [float(x/d_sum) for x in d]

    machine_distributions.append(make_scipy_rv(d))
    means.append(sum([x*d[x]/nr_machines for x in range(len(d))]))

max_mean = max(means)
       
def play_machine(k):
    """
    Play the k-th machine.
    """
    
    return machine_distributions[k].rvs()/(1.0*nr_machines)


def best_mu(): return max_mean
    
def mu(k):
    """
    Expected reward of machine k.
    """
    
    return means[k]    

def run_ucb1(n):
    """
    Perform n plays using the UCB1 strategy.
    """
          
    total_nr_plays = 0
    nr_plays = [0] * nr_machines
    average_reward = [0] * nr_machines
    
    for j in range(nr_machines):
        average_reward[j] = play_machine(j)
        nr_plays[j] += 1
        
        total_nr_plays += 1
    
    total_reward = 0
    
    for _ in range(n):
        max_j = None
        max_xj = None
        
        for j in range(nr_machines):
            xj = float(average_reward[j] + \
                     sqrt(2.0*log(total_nr_plays)/nr_plays[j]))
            
            if max_j is None:
                max_j = j
                max_xj = xj
            elif xj > max_xj:                    
                max_j = j
                max_xj = xj
    
        reward = play_machine(max_j)
        total_reward += reward
        
        average_reward[max_j] = (nr_plays[j]*average_reward[max_j] + reward) \
                                    /(nr_plays[j] + 1)
        nr_plays[j] += 1
        total_nr_plays += 1
        
    # best possible reward, our reward, regret, upper bound on expected regret.
    return (total_nr_plays*best_mu(),
            total_reward,
            total_nr_plays*best_mu() - total_reward,
            8*sum([float(log(total_nr_plays)/(best_mu() - mu(j))) \
            for j in range(nr_machines) if mu(j) < best_mu()]) \
                    + float(1 + pi**2/3) + sum([best_mu() - mu(j) \
                            for j in range(nr_machines)]))

runs = [(n, run_ucb1(n)) for n in [10, 20, 100, 1000, 2000]]

xrange = [x[0] for x in runs]

best_possible_data = [x[1][0] for x in runs]
total_reward_data = [x[1][1] for x in runs]
regret_data = [x[1][2] for x in runs]
regret_bound_data = [x[1][3] for x in runs]

pylab.plot(xrange, best_possible_data, '-o', \
           xrange, total_reward_data, '-^')
pylab.xlabel('number of plays')
pylab.ylabel('reward')
pylab.title('UCB1: best vs actual reward')
pylab.legend( ("best possible reward", "simulated reward"), loc='upper left')

pylab.grid(True)
pylab.savefig("best_and_total_reward.pdf")
pylab.close()

pylab.plot(xrange, regret_data, '-o', \
           xrange, regret_bound_data, '-^')
pylab.xlabel('number of plays')
pylab.ylabel('regret')
pylab.title('UCB1: regret and upper bound')
pylab.legend( ("regret", "bound on regret"), loc='upper left')

pylab.grid(True)
pylab.savefig("regret_and_bound.pdf")

#best_plot = list_plot(best_possible_data, plotjoined = True)
#total_plot = list_plot(total_reward_data, plotjoined = True, linestyle = '--')

#p = plot(best_plot + total_plot)
#p.save("best_and_total_reward.pdf")

#regret_plot = list_plot(regret_data, plotjoined = True)
#regret_bound_plot = list_plot(regret_bound_data, plotjoined = True, linestyle = '--')

#p = plot(regret_plot + regret_bound_plot)
#p.save("regret_and_bound.pdf")

