---
layout: assignment
published: true
title: Sorting homework
due: 04/10/17
---

# Assignment
Sorting procedures are incredibly important when doing scientific programming, however, the way you sort through data depends on the size of the dataset.  Implement three different ways of sorting and binning a 1-dimensional list.  For each case, write out the pseudo-code the prodedure, explaining your methodology. You can choose if you want to sort then bin from low to high, by even increments, by percentiles, etc.  This homework is to test the creativity of your logical thinking.  

To initalize a list to start:
~~~ python
import random
list_to_sort_bin = random.sample(xrange(100), 100)
~~~
will create a list from 0 - 99 of 100 samples.  The first argument xrange(100) is the list of numbers used to populate a sample.  The second argument is the length of the list.  You can choose to increase or decrease the range or sample size however you like to test your sorting methods.


## Help
For this assignment, the numpy module will be extremely helpful.  The functions you may use include:
* linspace
* percentile
* interpolate


To initialize nested lists within a listen (a 2-d array in other languages) in Python, use the one line expression of the form:

~~~ python
nested_list = [ [] for n in range(10) ]
~~~
will create 0-9 (Python is an exclusive language) lists within the list `nested_list.`  


As a hint, in the MatLab sections, Zach had you implement sorting of the arrays.  This can be recreated in Python as one of your three ways.
