---
layout: assignment
title: Multi-dimensional Fortran and More I/O
published: true
due: 02/13/17 13:00
---

Today we will continue to use our code from last week and retrofit it for this week. Make sure to copy the script into this week's folder and rename it to ensure we do not get confused.

(Assuming you are still in `fortran-week-two` folder):

~~~ bash
$ cp ../fortran-week-one/calculator.f90 ./new_calc.f90
~~~

First, we need to write out our converted data so that it can be used by someone else in our group later on for another project. 

* Retrofit `calculator.f90` to write out relative humidity, the zonal wind, and the meridional wind to the file `data/obs_conv.txt`. Make sure you include column headers for all of the data.

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

While there is no intrinsic Fortran function to calculate the average, we can use a provided [''black box''](https://en.wikipedia.org/wiki/Black_box) intrinsic function.

## Intrinsic Function Mean

The next step will be to construct matrices that hold all of this data. Define three new variables to hold the relative humidity, zonal wind, and meridional wind in matrix form of shape `(1453)`. Then, use the `i` iterator within the loop to assign the `i`th element of each matrix with a given value.

Now we can define variables to hold a mask for each of the 12 scenarios. Define twelve logical values that are of the same size as the data you will be averaging over. Next, define the masks based on each of those conditions, e.g. `uoverzeromask = ufullmatrix > 0.0`. Then, the intrinsic function mean is the result of `uoverzeroavg = SUM(rhmat, MASK=uoverzeromask) / COUNT(uoverzeromask)`.

Do this for all 12 scenarios and write out the results descriptively in `data/obs_avg.txt`. 

> Hint: You can use the form `LOGICAL, DIMENSION(1453) :: x1, x2, x3, x4` to define multiple variables with the same dimension at the same time.

## Compare and Conclude

Compare each pair of scenarios: 1 to 2, 3 to 4, 5 to 6, and so on. Are there differences between times when zonal wind comes from the east versus the west? Is this behavior different during different times of year, e.g. Jan.-Mar. to the full year or Jan.-Mar. to Jul.-Sept.? Write your notes about that as well in `data/conclusions.txt`.

* Submit today's four files--`data/obs_conv.txt`, `data/obs_avg.txt`, `data/conclusions.txt`, and `new_calc.f90`--to the Learn@UW dropbox for Fortran Week Two.
