---
layout: lecture
published: true
title: Plotting in Python
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


