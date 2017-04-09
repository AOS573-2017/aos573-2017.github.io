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

# Expanding on your Interpolation Function
