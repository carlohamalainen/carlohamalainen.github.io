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
from scipy import arange

# Got this data using
# python sailing_tests.py "main(5, 30000)"
# and so on.

lake_size = [3, 5, 10, 20, 30]

mc_data = [8646.05, 4999.45, 12349.2, 146236.65, 253721.45]
uct_data = [354.0, 1338.55, 4390.55, 101556.8, 150383.35]

grid_size = [x**2 for x in lake_size]

pylab.plot(grid_size, mc_data, '-*', grid_size, uct_data, '-o')
pylab.title("Number of samples to achieve error < 0.1 for sailing problem")
pylab.xlabel("grid size")
pylab.ylabel("number of samples")
pylab.legend( ("Monte Carlo planning", "UCT"), loc='upper left')
pylab.grid(True)

pylab.xlim(xmin = 0)
pylab.ylim(ymin = 0)
pylab.grid(True)

pylab.savefig("nr_samples_uct_sailing.pdf")

