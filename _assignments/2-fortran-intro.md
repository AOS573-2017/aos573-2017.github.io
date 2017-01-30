---
layout: assignment
title: Introduction to Fortran
published: true
---

Our ultimate goal in the assignments for this week and next week is to test the hypothesis that when the zonal wind is positive, or westerly, the relative humidity will be lower compared to when the zonal wind is negative, or easterly. One could make the conjecture winds predominantly coming off of Lake Michigan will be more humid than those coming from the continent.

First, we want to standardize our code a bit and prime it to run on a full year of observations.

* Rename your `data/obs_crop_conv.txt` file to `data/obs_crop_conv_old.txt`:

~~~ bash
$ mv data/obs_crop_conv.txt data/obs_crop_conv_old.txt
~~~

* Turn the degree to radian conversion into a subroutine.
* Turn the wind decomposition into a subroutine.
* Recompile your program and run it again.

~~~ bash
$ gfortran calculator.f90 -o calculator
$ ./calculator
~~~

* Compare the old and new converted files to make sure they are the same (i.e. your refactoring did not change the output). You may find this command to be helpful:

~~~ bash
$ diff data/obs_crop_conv.txt data/obs_crop_conv_old.txt
~~~

  * If they are not the same, reexamine your code to see if you made any typos. Also ask an assistant or classmate if you cannot determine why they may be different.

* Alter your code now to read in the file `data/obs.txt` instead of `data/obs_crop.txt`.
* Since this file contains a full year of observations, you will need to adjust the number of times you loop. To get the total number of lines, aka observations (minus 1 to count the column names on the first line), you may find the word count command will be handy:

~~~ bash
$ wc -l data/obs.txt
~~~

* Change your output file to `data/obs_conv.txt`.
* Recompile your program and make sure it runs successfully.

