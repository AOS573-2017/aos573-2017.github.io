---
layout: assignment
published: true
title: Binning
due: 
---

# Binning points is a common method when doing analysis of large datasets.  Sorting and binning points by themselves is not difficult, optimizing your sorting methods as your dataset increases in size however can be a challenge.  To go through 1,000 points and bin them, you do not have to worry about being computationally efficient.  To go through 100,000 points and binning them, you must think out the time requirements of each step of your process.

Create two different methods for binning a set of points.  Use the points given in one_thousand_points.txt and one_hundred_thousand_points.txt to compare your methods efficiency.  Optimize one method to be as efficient as possible and compare the time it takes to go through both sets of points using the efficient vs. less efficient method.  Compare the trade off of efficiency with readability.

To time your attempts, use the time module from Python (https://docs.python.org/2/library/time.html).
~~~ python
import time
start_time = time.time()
end_time = time.time()
print start_time - end_time
~~~


Remember, the most Pythonic solution to binning will be simple and easy to read.



