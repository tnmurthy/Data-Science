# Definition of radius
r = 0.45
# import math package
import math
# calculate C
C = 2*math.pi*r
# calculate A
A = math.pi*r*r
# Build printout
print("Circumference: " + str(C))
print("Area: " + str(A))

# Defination of Radius
r = 192500
# import radians function of math package
from math import radians
# travel distance of moon over 12 degrees, store in dist.
dist = r*radians(12)
print(dist)

height = [1.73, 1.68, 1.71, 1.89, 1.79]
weight = [65.4, 59.2, 63.4, 88.4, 68.7]
import numpy as np
np_height = np.array(height)
np_height
np_weight = np.array(weight)
np_weight
bmi = np_weight/np_height ** 2
bmi
bmi > 23
bmi[bmi > 23]


py_list = [1,2,3]
numpy_array = np.array([1,2,3])
py_list + py_list
numpy_array + numpy_array

# Create list baseball
baseball = [180, 215, 210, 210, 188, 176, 209, 200]
# Import the numpy package as np
import numpy as np
# Create a numpy array from baseball: np_baseball
np_baseball = np.array(baseball)
# Print out type of np_baseball
print(type(np_baseball))

# height is available as a regular list
# Import numpy
import numpy as np
# Create a numpy array from height: np_height
np_height = np_baseball
# Print out np_height
print(np_height)
# Convert np_height to m: np_height_m
np_height_m = np_height * 0.0254
# Print np_height_m
print(np_height_m)

x = ["a", "b", "c"]
x[1]
np_x = np.array(x)
np_x[1]

np_height = np.array([1.73, 1.68, 1.71, 1.89, 1.79])
np_weight = np.array([65.4, 59.2, 63.6, 88.4, 68.7])

type(np_height)
type(np_weight)


# Create baseball, a list of lists
baseball = [[180, 78.4],
            [215, 102.7],
            [210, 98.5],
            [188, 75.2]]
# Import numpy
import numpy as np
# Create a 2D numpy array from baseball: np_baseball
np_baseball = np.array(baseball)
# Print out the type of np_baseball
print(np_baseball)
# Print out the shape of np_baseball
print(np_baseball.shape)

# regular list of lists
x = [["a", "b"], ["c", "d"]]
[x[0][0], x[1][0]]
# numpy
import numpy as np
np_x = np.array(x)
np_x[:,0]

import numpy as np
np_mat = np.array([[1, 2],
                   [3, 4],
                   [5, 6]])
np_mat * 2
np_mat + np.array([10, 10])
np_mat + np_mat

height = np.round(np.random.normal(1.75, 0.20, 5000), 2)
weight = np.round(np.random.normal(60.32, 15, 5000), 2)
np_city = np.column_stack((height, weight))

import numpy as np
x = [1, 4, 8, 10, 12]
print(np.mean(x))
print(np.median(x))