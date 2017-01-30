---
layout: assignment
title: Introduction to Fortran Continued
published: false
---

Now, we want to add some code that calculates the average relative humidity under two conditions: where `u > 0` and where `u < 0`. While there is no intrinsic Fortran function to calculate the average, and we only have variables of shape `(1)`, we are going to calculate what is known as a _single pass_ mean. That means that as our code runs, we are going to accumulate two variables to calculate means with: a running total and a running count.

First, define four new real variables in your code; a running total and a running count for when u is positive and when u is negative. Let's call them `runtotalpos`, `runcountpos`, `runtotalneg`, and `runcountneg`.

One other concept you will need to know about is `if` statements in Fortran. They are defined like this type of segment:

~~~ f90
IF (variable > something) THEN
  dosomething
ELSE IF (variable < somethingelse) THEN
  dosomethingelse
ELSE
  dosomethingotherwise
END IF
~~~

Note that either the `else if` or the `else` statements are optional when creating an if block.

Add an if block within your code that adds an if block testing if `u > 0.0` or if `u < 0.0`. If that condition is met, you should add the current rh value to the respective running total variable and add 1 to the respective running count variable.

After your loop concludes, add a new block of code that does the following:

* Opens yet another new file named `data/obs_avg.txt` to write into.
* Writes out a line of text that describes which average you are printing out first (when u is easterly or westerly).
* Writes out the resulting average of relative humidity under that condition (note, our single pass mean is calculated as running total divided by running count--you can do this operation in the write statement).
* Write out another line of text that describes the second average you are printing out.
* Write out the resulting average of relative humidity under that other condition.
* Then finally close this new file.

* Recompile your program and run it again.
* Take a look at the `data/obs_avg.txt` file and make sure it output things that you expected.

~~~ bash
$ cat data/obs_avg.txt
~~~

* Submit your `data/obs_conv.txt`, `data/obs_avg.txt`, and `calculator.f90` files to the Learn@UW dropbox for Fortran Week One.
