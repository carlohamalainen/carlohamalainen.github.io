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
# python sailing_uct.py "main(5)"
# and so on.

partial_run_20 = [107, 55366, 5577, 130710, 173, 597, 2254, 24113, 74626, 131, 196, 55926, 433053, 147, 280688, 362, 296, 1003, 7, 519, 7736]

estimate_20 = sum(partial_run_20)/(1.0*len(partial_run_20))*100

lake_size = [0, 5, 10, 20]

grid_size = [x**2 for x in lake_size]
nr_samples = [0, 21743, 436847, estimate_20]

pylab.loglog(grid_size, nr_samples, marker = '*')
pylab.title("Number of samples to achieve error < 0.1 for sailing problem")
pylab.xlabel("grid size")
pylab.ylabel("number of samples")
pylab.xlim(xmin = 1)
pylab.ylim(ymin = 1)
pylab.grid(True)

pylab.savefig("nr_samples_uct_sailing.pdf")

