---
layout: assignment
title: Introduction to Fortran
published: true
---

Our ultimate goal in the assignment today is to test the hypothesis that when the zonal wind is positive, or westerly, the relative humidity will be lower compared to when the zonal wind is negative, or easterly. One could make the conjecture winds predominantly coming off of Lake Michigan will be more humid than those coming from the continent.

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
* Since this file contains a full year of observations, you will need to adjust the shape of all of your variables as well as the number of times you loop. To get the total number of lines, aka observations (minus 1 to count the column names on the first line), you may find the word count command will be handy:

~~~ bash
$ wc -l data/obs.txt
~~~

* Change your output file to `data/obs_conv.txt`.
* Recompile your program and make sure it runs successfully.

Now, we want to add some code that calculates the average relative humidity under two conditions: where `u > 0` and where `u < 0`. While there is no intrinsic Fortran function to calculate the average, we can leverage two functions instead to calculate our own: [`sum`](https://gcc.gnu.org/onlinedocs/gcc-4.4.7/gfortran/SUM.html) and [`count`](https://gcc.gnu.org/onlinedocs/gcc-4.4.7/gfortran/COUNT.html). Take a look at both of these webpages to see how the Fortran documentation is written up. Both of the functions take a mask argument, which means that you will provide a basic conditional statement (hint the two different conditions were given at the start)--for the sum, the mask is provided as a second input, e.g. `sum(somevariable, some > condition)`, while the count needs to be only provided a mask, e.g. `count(some > condition)`.

Add a new block of code after you close your second file that does the following:

* Open yet another file named `data/obs_avg.txt` to write into.
* Write out a line of text that describes which average you are printing out first (when u is easterly or westerly).
* Write out the resulting average of relative humidity under that condition (note, you can call these functions in the write line, though you could alternatively save the reuslt to a real variable and then write that variable out).
* Write out another line of text that describes the second average you are priting out.
* Write out the resulting average of relative humidity under that other condition.
* Then finally close this new file.

* Recompile your program and run it again.
* Take a look at the `data/obs_avg.txt` file and make sure it output things that you expected.

~~~ bash
$ cat data/obs_avg.txt
~~~

* Submit your `data/obs_conv.txt`, `data/obs_avg.txt`, and `calculator.f90` files to the Learn@UW dropbox for Fortran Week One.
