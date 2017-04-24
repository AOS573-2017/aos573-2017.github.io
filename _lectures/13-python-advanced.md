---
layout: lecture
published: true
title: Advanced Python
---

# Errorbars in Plots

Adding errorbars in the x, y, or both directions is incredibly easy.  To plot all cases, the same function will be used everytime, ``plt.errorbar(...)``.  Errorbar takes in an x and y list like ``plt.plot`` or ``plt.scatter``, and then either ``yerr = `` or ``xerr = `` or both.  

~~~ python
import numpy as np
import matplotlib.pyplot as plt

xs = np.linspace(0, np.pi*2, 7)
ys = np.cos(-xs)

xerr_scalar = .1
yerr_scalar = .2

plt.errorbar(x, y, yerr = yerr_scalar, xerr = xerr_scalar)
plt.show()
~~~

This sets the yerr and serr bars to a single, defined value.  For values that change with the x,y point, pass a list (or np.array) to xerr and/or yerr.

~~~ python
yerr_list = np.exp(-xs)

plt.errorbar(xs, ys, yerr = yerr_list)
plt.show()
~~~

Now the value of the errorbar changes with each point.  Lets apply this to the xaxis errorbars instead.

~~~ python
xerr_list = np.sin(xs)

plt.errorbar(xs, ys, xerr = xerr_list)
plt.show()
~~~

For errobars that are not symmetrical you must pass a pair of lists to the yerr or xerr.

~~~ python
yerr_lists = [np.sin(xs), .1*np.sin(xs)]

plt.errorbar(xs, ys, yerr = yerr_lists)
~~~


# Shaded

Don't like errobars, use shaded regions to signify the error instead!  To do this, use the ``plt.fill_between(...)`` function.  This takes in a single x and two ys to fill between.
http://matplotlib.org/api/pyplot_api.html#matplotlib.pyplot.fill_between

~~~ python
import matplotlib.pyplot as plt
import numpy as np

xs = np.linspace(0, 10, 100)
ys = np.sin(xs)

plt.fill_between(xs, ys, 0)
plt.show()
~~~

So how would you use this to fill the standard deviation of a dataset?  

~~~ python
xs = np.linspace(0, 50, 100)
ys = [(x*1.2) + 3. for x in xs]

upper_std = [(y * .15) + y for y in ys]
lower_std = [y - (y * .15) for y in ys]


plt.fill_between(xs, ys, upper_std, color = 'y', alpha = .5)
plt.fill_between(xs, ys, lower_std, color = 'r', alpha = .5)
plt.plot(xs, ys, 'k-', linewidth = 3.)
plt.show()
~~~

The alpha argument, which you can use in many other types of plots, changes the transparency.  If alpha wasn't set to .5, then it would be 100% opaque and harder to understand.  

The arguments x, y1, and y2 of fill between are referred to as 'keyword arguments', compared to the color, alpha, and linewidth arguments which are 'non-keyword'/optional.  All 'keyword' arguments must come before optional arguments otherwise you will receive an error ``SyntaxError: non-keyword arg after keyword arg``.  

# Exercise
Find the errors in the code.

# Interpolating a 2d Array in Python

Interpolating a 2D array in Python can be complicated if you want to do it correctly.  The method I am showing uses the scipy ``Interp2D`` function.  This creates a function within your script that allows you to pass in a new set of points that it will then interpolate your dataset to.  

Follow along in class to understand how to use this function.  Code from class with comments will be posted after class.  This may be helpful for your second project.

# Wrapping Interp2D into a Function

Using ``Interp2D`` would be easier if you were to wrap it into a function call that would automatically go through the steps of resizing a dataset.  The function would have to take in the dataset, starting lats, starting lons, end size lats, and end size lons.  It would return the dataset at a different resolution.

~~~ python
from scipy import interpolate
import numpy as np

def interp(data, start_lat, start_lon, end_lat, end_lon):
	function = interpolate.interp2d(start_lon, start_lat, data, kind = 'linear')
	new_data = function(end_lon, end_lat)
	return new_data
~~~

Now, instead of having to call and create different interp2d functions every time you resize an array, just call your own made interp function.  

# Using a Python Script as a Bash Script

Python has a very useful module called subprocess.  Subprocess allows you to create a script in Python and use it as a Bash script.  Unlike Bash scripts though, Python is simple, readable, and easy to change.  
https://docs.python.org/2/library/subprocess.html

To call a basic process, like a previously made Python script, use 	``subprocess.call(string_command_line_args.split())`` with a string of the command line argument you would usually use.

~~~ python
import subprocess

call_name = 'eog ' + figure_path + ' &'

subprocess.call(call_name.split())
~~~

would open a figure in eog using the figure path provided in a new window.

~~~ python
import subprocess

command_arg = 'python find_the_errors.py'

subprocess.call(command_arg.split())
~~~

If ever you have to run even other types of scripts, like IDL or MatLab, within a Bash script, try using instead Python's subprocess.  

# Exercise

Create a fully automated script that takes in a lower and upper x limit and a lower and upper y limit, plots the histogram and mean binned y values with standard deviation bars on a 2 panel plot, and prints out the average standard deviation of the ys.  Save the figure from the script using ``plt.savefig()`` then use ``subprocess.call()`` to open the figure.

