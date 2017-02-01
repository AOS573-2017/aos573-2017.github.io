---
layout: lecture
published: false
title: Multi-dimensional Fortran and More I/O
---

# Prelude

Last week we saw a general introduction to the structure and syntax of Fortran programs, as well as reading in data from an outside file. Unfortunately, all of our variables were of a singleton shape--they could only hold one value! Today we will focus on the construction of arrays and matrices within Fortran; this allows us to access all of a dataset. The other component we will also touch on is the opposite of reading: writing out data. 

## Revisiting Types: Arrays and Character Strings

The nature of a loop, especially when it comes to reading in data, lends itself to constructing arrays and matrices--objects that have a shape greater than one element. Yet another word for these objects is a vector. While they may all be named differently depending on the matrix, they all have something in common: holding more than one element of data within a single variable. To discuss arrays in Fortran, it requires revisiting how types are defined in a program.

When a type definition is made, it is assumed to be of a default size--both in terms of shape of the variable and the memory size given to it. Shape is something easier to tackle first because you can think back to vectors or matrices in math. By default, all variables are given a shape of (1). That means your real number, integer number, or character will have only one element. In Fortran, the shape is defined immediately after the variable name in your type declaration, e.g.:

~~~ f90
REAL :: test(3,2)
CHARACTER :: words(5)
~~~

In this instance, we have italicized a variable `test` that is of type real with the shape `(3,2)` and a variable `words` with shape `(5)`. Notice that I have carefully used the word ''shape''. A character array defined in this way would be 5 single character elements, such as `'a','b','c','d','e'`. Similarly, the real matrix defined here would look like:

|1 |2 |6 |
|3 |4 |9 |

We reserve the word "size" to define how big a single element of an array or matrix can be. Characters are usually the most intuitive type for what this means. The character size defines how many characters can be within a single element. For numbers, the size relates to either how precise the numbers are defined as (in the case of reals) or how large of a number that can be supported (in the case of integers). You may have heard of terms such as "double precision", "single precision", "large integer", "small integer", or other terms like this. Essentially it boils down to how much memory within the computer we allow for a variable to take up. For now, we will not be playing with size or precision for numbers--only characters.

The "size" of a variable is defined immediately after the type name in the type declaration, e.g.:

~~~ f90
CHARACTER(34) :: words
~~~

In _this_ instance, `words` is now a variable of shape `(1)` but that is 34 characters long. An example is `supercalifragilisticexpialidocious`, which is indeed 34 characters long.

You may need to define an array or matrix manually in Fortran for a variety of reasons. To do so, Fortran uses the notation of parentheses and forward slashes, like `(/ 1, 3, 4, 8 /)`. This methodology will only allow you to define one dimension at a time, so if you have a 2x2 matrix, you will have to define either `variable(1,:)` or `variable(:,1)` at one time. We will later cover defining a full multidimensional matrix later.

One way to get around this in defining more than one dimension is to use a data block. While this is a handy way to manually initialize a multidimensional, you have to keep in mind that Fortran is column-major. That means that defining a data array will be initialized by varying the first dimension first. 

To reference a given array element, you surround the index with parentheses. For example, if `variable` has shape `(500,2)`, and we wanted to define or reference the `(130,1)`th value, we would call `variable(130,1) = 2.0` or `somethingelse = variable(130,1)`.

If you do not define an index when referencing an array, Fortran will assume you mean all of the indices. That means saying `variable = 300.0` when `variable` has shape `(500,2)` will set _all_ of the values of `variable` to 300.0. Alternatively, saying `somethingelse = variable` will 1) require `somethingelse` to be the same size as `variable` and 2) set each respective value in `somethingelse` as the respective `variable` value.

> Which of the following choices correctly matches the following declaration:
>
> `CHARACTER(5) :: lyrics(7)`?
>
> 1. `(/ 'A', 'N', 'D', ' ', 'I' /)`
> 2. `(/ 'AND I K', 'NOW WHE', 'N THAT ', 'HOTLINE', ' BLING!' /)`
> 3. `(/ 'AND I', ' KNOW', ' WHEN', ' THAT', ' HOTL', 'INE B', 'LING!' /)`
> 4. `(/ 'A', 'N', 'D', ' ', 'I', ' ', 'K' /)`

Today for class we will be working on a new set of code and data to demonstrate multidimensional arrays. Go ahead and clone this week's repository into your home directory.

~~~ bash
$ git clone https://github.com/AOS573/fortran-week-two.git

$ cd fortran-week-two
~~~

To start out, we will create some mock-up data to see how arrays work.

~~~ bash
$ nano arrays.f90
~~~

Once your new file is open, start with the Fortran templating for a program. We will be defining a few different variables:

~~~ f90
PROGRAM arrays
! by Bucky Badger
! This program will work with some arrays in Fortran
IMPLICIT NONE


END PROGRAM
~~~

Now let's get to some real data. We have two files within our `data/` directory: `model_data.txt` and `model_data.dat`. First, take a look at them. Because there is a lot of data within them, `less` or `head` are more useful here.

~~~ bash
$ less model_data.txt

$ less model_data.dat

~~~

There is not a whole lot of usable information from these files so far--especially the .dat file--and Fortran does not lend itself too easily to wading through data files in an exploratory manner. Were these netCDF or HDF files, there are binary tools that can read attributes and dump the data file variables and attributes. We are left with one option (which should actually have been the first): reading the friendly manual!

Luckily our friend that gave us the data provided us with some information about the file layout:



Earlier in class we saw how to define a single dimension of a vector--but only one dimension at a time. One way to get around this is to use a data block. While this is a handy way to manually initialize a multidimensional, you have to keep in mind that Fortran is column-major. That means that defining a data array will be initialized by varying the first dimension first, the second dimension next, and so on.


## Writing Data

Writing works in a similar way to reading, except now we want our action to be write and we want to use write instead of read. Write statements are very similar to the print statements that we worked on earlier.

Let's make a file just like the original, but now write out the u and v components of the wind. We can do this one of two ways: create a new loop after our current work that will write each line to the new file, or add write statements in our current work that reference a different file identifier. We select the latter for ease right now.


We defined the file identifier for the second file to be different than the first. Assuming they are not both open at the same time, you could use the
 same number; but it never hurts to keep them separate regardless.

Now we should compile our program and see how it performs!


### Command Line Input/Output

One final note about reading and writing before we move on. If you need to either read into your Fortran program from the command line or write out from your Fortran program to the command line, you can use the read and write commands as before with an asterisk for the file identifier. In the case of reading in, perhaps you want the user to specify how many operations to perform or alternatively which type of computation to use or what to name a file. Fortran will allow you to do this--but be cautious about the types that you give to your input. You could cause an error if you are expecting an integer and the user provides a letter. Writing out to the terminal is equivalent to using the print statement.

## Command Line Input


# Lab Assignment

[Follow this link to our assignment](/assignments/3-fortran-arrays-write.html).
