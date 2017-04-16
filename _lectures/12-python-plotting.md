---
layout: lecture
published: false
title: Plotting in Python
---

# MatPlotLib

To start plotting in Python, you must first install the module used called MatPlotLib.

~~~bash
pip install matplotlib
~~~

Now that you have MatPlotLib installed, the way to use the plotting function of it in a Python script is to use a ``import matplotlib.pyplot as plt``.  The ``import ____ as __`` allows you to interact with a module giving it a nickname.  For instance, many people use the same nicknaming of Numpy as np in their scripts, ``import numpy as np``.  For longer names or modules you plan to use a lot, importing as a nickname can save you time typing.

For information on the functions used within this lecture, see the MatPlotLib documentation here http://matplotlib.org/api/pyplot_api.html.

# Histograms

All information on histograms being referenced here can be found in the documentation here http://matplotlib.org/api/pyplot_api.html#matplotlib.pyplot.hist.

To start, lets do a simple histogram plot using the same random module used in homework.

~~~ python
import matplotlib.pyplot as plt
import random

histogram_list = [random.random() for i in range(100)]

plt.hist(histogram_list)
plt.show()
~~~

The ``plt.hist(histogram_list)`` function creates your histogram.  The ``plt.show()`` will pop up another window with your histogram inside.

The ``.hist()`` function takes in a list, like ``histogram_list``, assumes 10 bins (or another number if the rcParams['hist.bins'] has been changed for some reason) which you can alter in the function call, bin ranges, a boolean ``'normed'``, and a number of other parameters.

To change the number of bins in your histogram, just pass in a new int like ``'bins' = new_number_of_bins``.

~~~ python
import matplotlib.pyplot as plt
import random

histogram_list = [random.random() for i in range(100)]
new_number_of_bins = 20
plt.hist(histogram_list, 'bins' = 20)
plt.show()
~~~

To create a PDF, set the ``'normed'`` boolean as True in the function call.

~~~ python
import matplotlib.pyplot as plt
import random

histogram_list = [random.random() for i in range(100)]

plt.hist(histogram_list, 'normed' = True)
plt.show()
~~~

To use your own bin ranges, try passing it a list of linearly spaced numbers from 0 to .1.  (``random.random()`` by default only produces randoms between 0 and 1.)

~~~ python
import matplotlib.pyplot as plt
import random

histogram_list = [random.random() for i in range(100)]

bin_ranges = np.linspace(0, 1., 16)

plt.hist(histogram_list, 'bins' = bin_ranges)
plt.show()
~~~

On your own, practice using either ``'normed'`` or ``'bins'`` and another alternative argument found from the histogram documentation.

# Scatterplots

All information on scatter plots found here is referenced using the ``.scatter()`` documentation from http://matplotlib.org/api/pyplot_api.html#matplotlib.pyplot.scatter.

This example will create a linear line scatter plot.  Scatter plots, and plots in general, are extremely similar to the same function calls used in MatLab.

~~~ python
import numpy as np
import matplotlib.pyplot as plt

#Create our list of xs first from 0 to 2 pi
xs = np.linspace(0, 2*np.pi, 100)

#Then pass these xs to a sin function of numpy
ys = np.sin(xs)

plt.scatter(xs, ys)
plt.show()
~~~

``.scatter()`` takes in a list of points or just two single numbers.

To change the color of the points, as in most (but not all) MatPlotLib plots, use ``c = new color``.  The new color must follow the rules of colors in MatPlotLib found here https://matplotlib.org/api/colors_api.html.  c can also be a list of colors pertaining to another dimension with a colormap used to look up the color.  The transparency of the point can be tuned using the ``'alpha'`` parameter.  The size of the point can be altered using ``'s'``.

To change the color of all the points to one value, pass in a color to the function like ``c = 'k'``, which will turn the points black.
~~~ python
import numpy as np
import matplotlib.pyplot as plt

#Create our list of xs first from 0 to 2 pi
xs = np.linspace(0, 2*np.pi, 100)

#Then pass these xs to a sin function of numpy
ys = np.sin(xs)

plt.scatter(xs, ys, c = 'k')
plt.show()
~~~

To change the color of a point and have it reference a third dimension, start by creating a new list of values you want to reference.

~~~ python
import numpy as np
import matplotlib.pyplot as plt

#Create our list of xs first from 0 to 2 pi
xs = np.linspace(0, 2*np.pi, 100)

#Then pass these xs to a sin function of numpy
ys = np.sin(xs)

#New dimension
zs = np.cos(xs)
~~~

Now that you have a third dimension set, select a colormap from MatPlotLib you want to use from the documentation https://matplotlib.org/examples/color/colormaps_reference.html.  For our exercise, we'll be using 'jet.' You pass in the colormap in the function using ``cmap = 'jet'``. 

To have the colormap 'fitted' to your points, pass in a ``vmin = min(zs), vmax = max(zs)`` to tell the plot what the range of points will be.

~~~ python
plt.scatter(cs, ys, c = zs, cmap = 'jet', vmin = min(zs), vmax = max(zs)0
plt.show()
~~~

Try repeating this with three different types of colormaps.  See what changes.  The 'Qualitative colormaps' use partitoned colors, while the others are a spectrum, when would that become handy?

Take your binned values from homework, read in the lists, and create a scatter plot using one list as the xs, one list as the ys, and one list as the color dimension.

#Making your own colormap

Colormaps can be used in many different types of plots using MatPlotLib.  To make your own colormap, we're going to have you make your own module that has its own instance of a colormap.  To start, open a new script. 

Colormaps are made using light based colormixing of RBG.  You reference the color you want in a dictionary using the color as a key.

For example:

~~~
simple_colormap = {'red': ((0.0, 1.0, 1.0),
					(.5, .5, 0.0)
					(1., 0., .5));
					'blue': ((0.0, 0.0, 0.0),
					(.5, 0.0, .5),
					(1.0, .5, 1.0))
					'green': ((0.0, 0.0, 0.0),
					(.5, 0.0, 1.0),
					(1.0, 1.0, 1.0))
					}
~~~
the first number defines your starting point from 0-1.  The middle number represents how much red will be at the beginning of your starting point (0.0).  The last number represents how much red will be at your end point, the first number of the next tuple.  This is a continuous use of red example.

To save the colormap in MatPlotLib, use the syntax
~~~ python
#!/usr/bin/env python
import matplotlib.pyplot as plt
plt.register_cmap(name = 'BlueRed', data = simple_colormap)
cmap = plt.get_cmap('BlueRed')
~~~

There are two choices of how to get the colormap created from one script to another.  Either, you can make a function in your colormap script that returns your colormap instance OR you can choose to keep it as a variable and reference the variable.

Choice one example:
~~~ python
#!/usr/bin/env python
import matplotlib.pyplot as plt
def get_blue_red_map():
	blue_red_colormap = {???}
	plt.register_cmap(name = 'BlueRed', data = simple_colormap)
	cmap = plt.get_cmap('BlueRed')
	return cmap
~~~

Choice two example:
~~~ python
#!/usr/bin/env python
blue_red_colormap = {???}
~~~

~~~ python
import my_colormap.py
colormap = my_colormap.blue_red_colormap
plt.register_cmap(name = 'BlueRed', data = simple_colormap)
cmap = plt.get_cmap('BlueRed')
~~~

Traditionally, using the first choice is better coding.  Referencing a variable from a module is not the best practices, however both do work. Notice that when referencing a variable from another module, you do not use the ().  () are for function calls only.  If ever debugging your own Python code, missing or having an extra () is a ``Syntax Error``.

Try creating your own colormap that goes from blue to red smoothly.  NOTE: THE EXAMPLE COLORMAP DOES NOT DO THIS; COPYING AND PASTING WILL NOT CREATE A BLUE->RED MAP.






