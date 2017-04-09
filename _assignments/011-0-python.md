---
layout: assignment
published: false
title: Interpolation
due: 
---

# Interpolation

Interpolation is incredibly important when gridding and plotting datasets.  Sometimes, you may want to match a dataset at 2.5 x 2.5 degrees to a dataset at 5 x 5 degrees.  There are functions built into different modules of Python to interpolate for you, however you should have an understanding of how the ‘black box’ interpolation functions work before using them.  

For your assignment, build your own interpolation functions.  Start with a simple case of making first a function that takes in two points, a step, and returns a filled in list between the two points.

Test your function using your netCDF dataset.  First, by hand figure out what your function will return.  Then pass in the parameters to your interpolate function and see if you were right.  

A useful function to understand would be the range function in Python.  You can read about it, and other build in functions, here: https://docs.python.org/2/library/functions.html#range.

# Expanding your Interpolation Function

Now that you have a function that interpolates between two numbers, how would you use this function to interpolate a dataset?  Try creating an expanded function that takes in a grid spacing of the incoming dataset, the new grid spacing you want, and the dataset you need to interpolate to a different grid spacing.  Unlike most datasets, you can assume the incoming dataset is a single list.  

Remember in Python, if you use function_1 in function_2, function_1 msut come before function_2 in your script.

