---
layout: assignment
title: Introduction to Fortran
published: true
due: 02/03/17 17:00
---

Our ultimate goal in the assignments for this week and next week is to test the hypothesis that when the zonal wind is positive, or westerly, the relative humidity will be lower compared to when the zonal wind is negative, or easterly. One could make the conjecture winds predominantly coming off of Lake Michigan will be more humid than those coming from the continent.

First, we want to standardize our code a bit and prime it to run on a full year of observations.

* Turn the degree to radian conversion into a subroutine.
* Turn the wind decomposition into a subroutine.
* Recompile your program and run it again.

~~~ bash
$ gfortran calculator.f90 -o calculator
$ ./calculator
~~~

* Alter your code now to read in the file `data/obs.txt` instead of `data/obs_crop.txt`.
* Since this file contains a full year of observations, you will need to adjust the number of times you loop. To get the total number of lines, aka observations (minus 1 to count the column names on the first line), you may find the word count command will be handy:

~~~ bash
$ wc -l data/obs.txt
~~~

* Recompile your program and make sure it runs successfully.
* Once you have split up the subroutines and are able to read in all the observations, you can upload your `calculator.f90` script into the Learn@UW Dropbox for Fortran Week One.
