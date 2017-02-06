---
layout: assignment
title: Multi-dimensional Fortran and More I/O
published: false
---

Today we will continue to use our code from last week and retrofit it for this week. Make sure to copy the script into this week's folder and rename it to ensure we do not get confused.

(Assuming you are still in `fortran-week-two` folder)
~~~ bash
$ cp ../fortran-week-one/calculator.f90 ./new_calc.f90
~~~

First, we need to write out our converted data so that it can be used by someone else in our group later on for another project. 

* Retrofit `calculator.f90` to write out relative humidity, the zonal wind, and the meridonal wind to the file `data/obs_conv.txt`. Make sure you include column headers for all of the data.

Now, we want to add some code that calculates averages under a few conditions:

1. Where `u > 0` for all the data.
2. Where `u < 0` for all the data.
3. Where `u > 2` for all the data.
4. Where `u < -2` for all the data.
5. Where `u > 0` for approximately the first quarter of data, or elements 1 to 363, for a rough January to March estimate.
6. Where `u < 0` for approximately the first quarter of data.
7. Where `u > 2` for approximately the first quarter of data.
8. Where `u < -2` for approximately the first quarter of data.
9. Where `u > 0` for approximately the third quarter of data, or elements 726 to 1089, for a rough July to September estimate.
10. Where `u < 0` for approximately the third quarter of data.
11. Where `u > 2` for approximately the third quarter of data.
12. Where `u < -2` for approximately the third quarter of data.

While there is no intrinsic Fortran function to calculate the average, we can use our ingenuity to do it ourselves--and in two ways, to boot! Through two ways, we will see how our own manual calculation of each of the above 12 scenarios' averages compare to using a provided [''black box''](https://en.wikipedia.org/wiki/Black_box) intrinsic function.

## Single Pass Mean

The first way will be a _single pass_ mean. That means that as our code runs, we are going to accumulate two variables that can calculate the mean: a running total and a running count. These are particularly useful when you have to calculate statistics over an enormous amount of data that you cannot store in memory all at once.

> mean of x observations = total of x observations / count of x observations

We will now be calculating and writing out single pass means for these 12 scenarios.

So, however you want to pick naming, define variables to store the total and the count for each of the 12 scenarios above (i.e. 24 different variables: 12 totals and 12 counts).

Let's walk through scenario `1.` from above. Add an if block within your code that tests if `u > 0.0`. If a condition is met, you should add the current rh value to the respective running total variable and add 1 to the respective running count variable.

Then, after your loop, write the result of the division (total / count) with some text describing it to the file `data/obs_avg.txt`; it is your choice whether to break it up into different lines, create columns, or something else.

After compiling and running, if the output file looks okay, continue on with the other scenarios. You can nearly duplicate each bit of code you added with changes to the scenario or array indices. Make sure to also print out the results of all the scenarios in this same `data/obs_avg.txt` file.

## Intrinsic Function Mean

Once the above is completed, the next step will be to construct matrices that hold all of this data as well. Define three new variables to hold the relative humidity, zonal wind, and meridional wind in matrix form of shape `(1453)`. Then, use the `i` iterator within the loop to assign the `i`th element of each matrix with a given value.

Now we can define variables to hold a mask for each of the 12 scenarios. Define twelve logical values that are of the same size as the data you will be averaging over. Next, define the masks based on each of those conditions, e.g. `uoverzeromask = ufullmatrix > 0.0`. Then, the intrinsic function mean is the result of `total(somevariable, MASK=somemask) / count(somevariable, MASK=somemask)`.

Do this for all 12 scenarios and write out the results after the single pass means in `data/obs_avg.txt`. 

## Compare and Conclude

First compare the respective values between the single pass means and the intrinsic function means. Are they similar? Are they different? Jot down any observations in a file `data/conclusions.txt` using nano or another command line text editor.

Finally, compare each pair of scenarios: 1 to 2, 3 to 4, 5 to 6, and so on. Are there differences between times when zonal wind comes from the east versus the west? Is this behavior different during different times of year, e.g. Jan.-Mar. to the full year or Jan.-Mar. to Jul.-Sept.? Write your notes about that as well in `data/conclusions.txt`.

* Submit today's four files--`data/obs_conv.txt`, `data/obs_avg.txt`, `data/conclusions.txt`, and `new_calc.f90`--to the Learn@UW dropbox for Fortran Week Two.
