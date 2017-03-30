---
layout: lecture
published: false
title: Introduction to Python
---

## Python

Welcome to the first day of the rest of your (programming) life!  The last unit of the class will be the language Python.
To create a Python script, start by opening a file in the editor and naming it anything you want making sure to include .py at the end of the filename.
You can also, at any time, just open python by typing python into the command line.  This is always useful to test out loops, logics, and other simple script items quickly without re-running an entire script you are currently working on.

Now to make our Helloworld.py script, we'll open it from command line using my favorite editor gedit.  Feel free to use any editor you like, in this case the means will not change the end product.

~~~ bash
 $ gedit Helloworld.py
~~~


Using this Helloworld.py, we will do the first thing all programmers do when starting a new language.  Inside of the script start including #/usr/bin/env python at the top of the script.  Then on a new line, print out Hello, world!


~~~ python
#usr/env/bin python

print Hello, world!
~~~

The #usr/env/bin python signifies in the script that the language being used is python.  You can alter the file parameters from command line in linus using the command 'chmod 777 Helloworld.py' to run the script from the command line without python in the beginning of the name.  In general, it is useful to alter scripts that will not be used as modules as executable.  The print command prints out to terminal. 

## Code Layout

People like python because it is a readable programming language.  Unlike Fortran, where you have commands like OPEN(UNIT=10, 'FILENAME.TXT', mode='read'), or MatLab, where you manipulate arrays to the point of no return, python is incredibly easy to understanding when laid out and executed correctly.  Having readable code allows you to easily debug, pass on, or alter your code.  You can leave less comments about how things work because you intuitively understand just by looking at the code what is happening.  

The first step to laying out your code is to not type anything at all.  Think about what you want your program to do.  Do you need to only plot some data?  Do you need to filter and plot data?  What functions should you make?  What variables will you need?  Having an idea of all the steps you do makes it easier to then sequentially fill in and execute code.  The proper way to layout your code is to have all variables needed in one place, and the steps, in order of necessity, after.

## The Basics


There are three types of numbers you will use when programming in python:
	int: Integers ex: 3
	float: Similar to REALS in Fortran or double in MatLab ex: 3.5
	complex: Any number with an imaginary portion ex: 3.5 + 3j

There are characters and strings in python.
	'p' = character
	'python' = string
	
Types of data structures:

* Arrays in python are called lists and can be created using [].  Lists and items in the list can be altered.

* Tuples are like arrays however they are created using () and cannot be altered once created.

* Dictionaries are a useful type of data structure.  They can assign a key to an object, and when you reference a key to a dictionary it returns the item it references.
	
	
~~~ example.py

this_is_a_int = 1
this_is_a_float = 1.5
this_is_a_complex = 1.5 + 1j

this_is_a_list = [1, 2, 3.5]
this_is_a_tuple = (1, 2, 3.5)

this_is_a_dictionary = {'this_is_a_key_to_a_list': this_is_a_list}
print this_is_a_dictionary['this_is_a_key_to_a_list']
~~~


~~~ bash
 $ python example.py
 $ [1, 2, 3.5]
~~~

You do not need to declare types for variables in python.

In Fortran...
~~~ gfortran
IMPLICIT NONE
REAL :: x, y

X = 3.5
Y = 4.5
~~~

becomes in python
~~~ python
x = 3.5
y = 4.5
~~~

To create a string, just put "" or '' around it.

~~~ python
hello = 'Hello, world!'
~~~

To create a boolean, or logical, variable you only need to declare it as True or False.

~~~ python
python_is_cool = True
~~~

Variables can be reassigned at any time as a different type.  To change the type of the variable, you can reassign it by casting it as another type.

~~~ python
x = '3.5'
x = float(x)
print x
~~~

To find the current type of a variable, use the type() operator.

~~~ python
x = 3.5
print type(x)
~~~



Functions in python should are created using a def function_name(function_variables): format.

~~~ python
def example_function(a, b):
	return (a+b)/2
~~~

All functions must have a return argument and return through all logical paths.  You do not need to state the type of variable a function reads in, or the type of variable it will return.  
Common mistakes in creating and utilizing functions include...

~~~ python
c, d = example_function(1, 2)
TypeError: (Int, float) object is not iterable
~~~

The example function only returns a single float or int, trying to allocate this single float/int to both c and d is not possible.

Furthermore, there are rules of how adding and multiplying floats and ints works in python. There are many easy references online to understand the rules of int and float math.  Adding two ints and dividing by two may return the wrong answer because the value will be rounded to an int.  This mistake does not throw an error but is something to watch out for when coding.

~~~ python
def new_example(a, b, split):
	if split:
		return a
	if b > 0:
		return b
	if b/a > 0:
		return a+b
	if split and a > 0:
		return a * -1
	if a < 0:
		return b -1
~~~

This function is not guaranteed to return.  Having logic within one function in order for to maximize the utility of one function is okay and can be appropriate, however you should always double check the function is always returning.  This function does not throw an error although it may not always return.

~~~ python
def function_2(c, d):
	exponent = function_1(1.0, 2.0)
	return c**exponent + d
def function_1(a, b):
	return (a+b) * (a-b)
~~~

This will throw a syntax error.  You must define the function 1 before function 2 if function 1 will be used in function 2.  This is one reason it is useful and worthwhile to think of the layout of your code ahead of time before starting any new scripts.

~~~ python
ran = 4
def random_function(a):
	return (a + ran ) / (a - ran ) 
~~~

It is best practice in python to only use the variable passed to the function or created in the function.  Functions should be able to be copy and pasted to a module at any time without breaking, using a variable (ran) not defined within the function and not passed to the function can lead to future errors if the function is ever moved to a different script.

The scope of variables is slightly different than in MatLab.  In MatLab, it is inherent that variables loaded in the main script are global variables and accessible to all functions within the script.  In python, this is not the case. You can define global variables within a function as so:

~~~ python
def random_function(a):
	global RAN
	return (a + ran ) / (a - ran )
~~~

This function becomes a bit high maintenance with using a global variable because you must guarantee RAN (global variables should be in all caps) exists and is a number.  Try to structure scripts with no global variables within functions, future changes will be easier to create and debug without global variables to worry about.

It is very useful to think of the structure of all programs before you make them.  As an exmaple, please try the FizzBuzz exercise.  Fizzbuss is an interesting exercise because it tests your ability to think logically and creatively.  There are many ways to do the FizzBuzz exercise, try to think of 3 different ones, and we will go around class and have you talk about the worst way you thought of implementing FizzBuzz and the best way of implementing FizzBuzz.  




