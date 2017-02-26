---
layout: lecture
published: false
title: Introduction to Matlab
---


# Matlab Overview
Matlab was originally written by a CS professor at the University of New Mexico who didn't want to force his students to learn Fortran in order to use numerical algrabraic and statistical tools. Matlab, unlike Fortran is not a compiled programming language; once you've written a script, all you need to do is run it. Matlab also has a very nice GUI to make writing and running code, as well as visualizing data quite comfortable.

Some major differences in terms of coding syntax from Fortran include not having to define your variable types, Matlab does it for you (and usually does a pretty good job!). Also, Matlab is case sensitive regarding any variables you create. Our first step in getting into coding with matlab is becoming framiliar with its user interface.

# The Matlab Interface
In whatever directory you've been doing your other Fortran lectures, you are going to want to open Matlab.

~~~ bash
matlab &
~~~

The `&` simply allows you to do more things on the shell command line once you've opened Matlab. Once you have opened Matlab, you will see that there are a few boxes, and that in general the program kind of looks like a text editor that also has a command line - it is! Other than the toolbar at the top, you should have three boxes to look at, and they all are titled. One will be your 'command window', one is your 'current folder', and one is your 'workspace'. There may also be a 4th window, depending on which version of Matlab that you are running called 'variables', and it should be empty right now.
