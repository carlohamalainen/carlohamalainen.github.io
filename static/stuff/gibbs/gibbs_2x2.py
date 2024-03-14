"""
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

http://carlo-hamalainen.net/stuff/gibbs

Run this file using Sage (http://sagemath.org):

$ sage gibbs_2x2.py

"""

import sage.all
from sage.all import assume, latex, solve, Matrix, var
import random

random.seed(0) # for reproducibility

p1 = var('p1')
p2 = var('p2')
p3 = var('p3')
p4 = var('p4')

assume(p1 >= 0)
assume(p2 >= 0)
assume(p3 >= 0)
assume(p4 >= 0)

assume(p1 <= 1)
assume(p2 <= 1)
assume(p3 <= 1)
assume(p4 <= 1)

assume(p1 + p2 + p3 + p4 == 1)

# Conditional probability matrices:
A_y_x = Matrix([[p1/(p1+p3), p3/(p1+p3)], [p2/(p2+p4), p4/(p2+p4)]])
A_x_y = Matrix([[p1/(p1+p2), p2/(p1+p2)], [p3/(p3+p4), p4/(p3+p4)]])
A = A_y_x*A_x_y

print "A_y_x:"
print latex(A_y_x)
print "A_x_y:"
print latex(A_x_y)
print "A = A_x_x:"
print latex(A)

# Solve for the eigenvalues and then manually
# construct the right eigenvectors of A.

eigenvalues = A.eigenvalues()

v1 = var('v1')
v2 = var('v2')

evector_solutions = []

for eval in eigenvalues:
    characteristic_product = Matrix([v1, v2])*(A - eval*Matrix([[1, 0], [0, 1]]))
    evector_solutions.append(solve([characteristic_product[0, 0], characteristic_product[0, 1]],
                                   [v1, v2], solution_dict = True))

# We are interested in the eigenvector corresponding to 
# eigenvalue 1.
idx = eigenvalues.index(1)
soln = evector_solutions[idx]
assert len(soln) == 1 # sometimes more here?

vec = Matrix([v1, v2])
vec = vec.subs(soln[0])

# There is a free variable in the solution, name begins with 'r'.
free = [v for v in vec[0, 0].variables() if str(v)[0] == 'r']
assert len(free) == 1
free_var = var(free[0])

# vec = [v1, v2], where v1, v2 depend on free_var. Solve for free_var
# such that v1 + v2 == 1.
last_solution = solve([vec[0, 0] + vec[0, 1] - 1], free_var, solution_dict = True)
assert len(last_solution) == 1
vec = vec.subs(last_solution[0])

print "Eigenvector with eigenvalue 1:"
print latex(vec)

# Check that this really is an eigenvector with eigenvalue of 1.
lhs = vec*A - 1*vec
assert lhs[0, 0].is_zero()
assert lhs[0, 1].is_zero()

# Do some numerical tests.

p = [random.random() for _ in range(4)]
p_sum = sum(p)

p = [x/p_sum for x in p]

# Subscript n means 'numerical'.
vec_n = vec.subs({p1:p[0], p2:p[1], p3:p[2], p4:p[3]})
A_n = A.subs({p1:p[0], p2:p[1], p3:p[2], p4:p[3]})

print "Error between numerical and exact: %.4f" % (max(map(abs, (vec_n*A_n - vec_n).row(0))))

f = Matrix([0.5, 0.5])

errors = []

for _ in range(10):
    errors.append(float(max(map(abs, (vec_n - f).row(0)))))
    f = f*A_n    

print errors


import pylab

pylab.plot(range(len(errors) - 1), errors[:-1])
pylab.title("Convergence of $[0.5, 0.5]A^n$ to exact normalised eigenvector")
pylab.xlabel("$n$")
pylab.ylabel("error of $X=1$ term")
pylab.grid(True)

pylab.xlim(xmin = 0)
pylab.ylim(ymin = 0)
pylab.grid(True)

pylab.savefig("gibbs_error_matrix_mult.pdf")
pylab.close()


# Now try an actual Gibbs sampling procedure.
def sample_X_given_Y(y):
    if random.random() < A_x_y_n[y, 0]: return 0
    else: return 1

def sample_Y_given_X(x):
    if random.random() < A_y_x_n[0, x]: return 0
    else: return 1

#p = [random.random() for _ in range(4)]
#p_sum = sum(p)
#p = [x/p_sum for x in p]

# Subscript n means 'numerical'.
vec_n = vec.subs({p1:p[0], p2:p[1], p3:p[2], p4:p[3]})
A_x_y_n = A.subs({p1:p[0], p2:p[1], p3:p[2], p4:p[3]})
A_y_x_n = A.subs({p1:p[0], p2:p[1], p3:p[2], p4:p[3]})

Y = 0
X = sample_X_given_Y(Y)

nr_samples = 5000
nr_X_1 = 0
error_of_distribution_gibbs = []

for _ in range(nr_samples):
    Y = sample_Y_given_X(X)
    X = sample_X_given_Y(Y)

    if X == 1: nr_X_1 += 1

    error_of_distribution_gibbs.append(abs(1.0*nr_X_1/nr_samples - float(vec_n[0, 1])))

pylab.plot(range(len(error_of_distribution_gibbs) - 1), error_of_distribution_gibbs[1:])

pylab.title("Error for $X=1$ in Gibbs sampling")
pylab.xlabel("step")
pylab.ylabel("error")
pylab.grid(True)

#pylab.xlim(xmin = 0)
#pylab.ylim(ymin = 0)
pylab.grid(True)

pylab.savefig("gibbs_2x2_error.pdf")
pylab.close()


print
print "Actual example of vectors and stuff:"
print "p:", p
#print float(vec_n[0, 1] - estimate)
print "A:", latex(A_n)
print "f:", latex(vec_n)


