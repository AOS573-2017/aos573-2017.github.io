---
layout: lecture
published: true
title: Multi-dimensional Fortran and More I/O
---

# Prelude

Last week we saw a general introduction to the structure and syntax of Fortran programs, as well as reading in data from an outside file. Unfortunately, all of our variables were of a singleton shape--they could only hold one value! Today we will focus on the construction of arrays and matrices within Fortran; this allows us to access all of a dataset. The other component we will also touch on is the opposite of reading: writing out data.
 
Today for class we will be working on a new set of code and data to demonstrate multidimensional arrays. Go ahead and clone this week's repository into your home directory.

~~~ bash
$ git clone https://github.com/AOS573-2017/fortran-week-two.git

$ cd fortran-week-two
~~~

## Revisiting Types: Arrays and Character Strings

The nature of a loop, especially when it comes to reading in data, lends itself to constructing arrays and matrices--objects that have a shape greater than one element. Yet another word for these objects is a vector. While they may all be named differently depending on the matrix, they all have something in common: holding more than one element of data within a single variable. To discuss arrays in Fortran, it requires revisiting how types are defined in a program.

To start out, we will create some mock-up data to see how arrays work. First open a new file.

~~~ bash
$ nano arrays.f90
~~~

Start another Fortran program template:

~~~ f90
PROGRAM arrays
! by Bucky Badger
! This program will work with some arrays in Fortran
IMPLICIT NONE


END PROGRAM
~~~

When a type definition is made, it is assumed to be of a default size--both in terms of shape of the variable and the memory size given to it. Shape is something easier to tackle first because you can think back to vectors or matrices in math. By default, all variables are given a shape of (1). That means your real number, integer number, or character will have only one element. In Fortran, the shape is defined immediately after the variable name in your type declaration, e.g.:

~~~ f90
PROGRAM arrays
! by Bucky Badger
! This program will work with some arrays in Fortran
IMPLICIT NONE

REAL :: test(3,2)
CHARACTER :: words(5)

END PROGRAM
~~~

In this instance, we have italicized a variable `test` that is of type real with the shape `(3,2)` and a variable `words` with shape `(5)`. Notice that I have carefully used the word ''shape''. A character array defined in this way would be 5 single character elements, such as `'a','b','c','d','e'`. Similarly, the real matrix defined here would look like:

|1 |2 |6 |
|--|--|--|
|3 |4 |9 |

We reserve the word "size" to define how big a single element of an array or matrix can be. Characters are usually the most intuitive type for what this means. The character size defines how many characters can be within a single element. For numbers, the size relates to either how precise the numbers are defined as (in the case of reals) or how large of a number that can be supported (in the case of integers). You may have heard of terms such as "double precision", "single precision", "large integer", "small integer", or others. Essentially it boils down to how much memory within the computer we allow for a variable to take up. For now, we will not be playing with size or precision for numbers--only characters.

The "size" of a variable is defined immediately after the type name in the type declaration, e.g.:

~~~ f90
PROGRAM arrays
! by Bucky Badger
! This program will work with some arrays in Fortran
IMPLICIT NONE

REAL :: test(3,2)
CHARACTER :: words(5)
CHARACTER(34) :: newwords

END PROGRAM
~~~

In _this_ instance, `newwords` is a variable of shape `(1)` but that is 34 characters long. An example is `supercalifragilisticexpialidocious`, which is indeed 34 characters long. If a string does not fill the full size of a character variable, the string will be padded with spaces at the end.

Elements are referenced using a similar way as their shape is designed: the location surrounded in parentheses immediately following the variable name. Additionally, Fortran is an index-one language, or in other words indexes start at 1 instead of 0 (as in some other languages). To reference the 5th element in the variable `words` from before with shape `(5)`, you would type `words(5)`; the 1st column, 2nd row of the variable `test` with shape `(3,2)` would be called like `test(1,2)`. To reference a full dimension of a variable, the colon is used, e.g. `test(1,:)`. Keep in mind that when specifying indices, you _must_ specify a quantity of indices equal to the number of dimensions a variable has. Thus, you could not say `test(2)` because `test` has _two_ dimensions.1

To reference a subset of a dimension (or more than one dimension), the lower index and the upper index are specified straddling a colon; keep in mind that the upper index is included in the elements returned. So if you want the 2nd through the 4th elements of `words`, you would type `words(2:4)`.

Initialization of variables' values in Fortran is recommended--especially if you are using them as a counter. Let's see what happens when we print out a few different slices of our arrays:

~~~ f90
PROGRAM arrays
! by Bucky Badger
! This program will work with some arrays in Fortran
IMPLICIT NONE

REAL :: test(3,2)
CHARACTER :: words(5)
CHARACTER(34) :: newwords

PRINT *,"Test: ", test(1:2,:)
PRINT *, words, " and ", newwords, " are words and new words."

END PROGRAM
~~~

~~~ bash
$ gfortran arrays.f90 -o arrays
$ ./arrays
 Test:   2.11260802E-27  4.59135442E-41  5.87982594E-39   0.0000000
 Í and ï are words and new words.
~~~

The output is going to vary based on your computer and current state. We are seeing numbers that are not really initialized to what we would think they would be: instead they are values close to 0; further, the strings are just a few strange characters. This is not a problem if you are going to be reading in data over the variable. But if you are planning to use that variable as some sort of counter or accumulator, you should manually zero it!

If you do not define an index--even with a colon--when referencing an array, Fortran will assume you mean all of the indices. That means saying `variable = 300.0` when `variable` has shape `(500,2)` will set _all_ of the values of `variable` to 300.0. Alternatively, saying `somethingelse = variable` will 1) require `somethingelse` to be the same size as `variable` and 2) set each respective value in `somethingelse` as the respective `variable` value.

Let's see the behavior when we initialize the variables' values now:

~~~ f90
PROGRAM arrays
! by Bucky Badger
! This program will work with some arrays in Fortran
IMPLICIT NONE

REAL :: test(3,2)
CHARACTER :: words(5)
CHARACTER(34) :: newwords

test = 0.0
words = ''
newwords = ''

PRINT *,"Test: ", test(1:2,:)
PRINT *, words, " and ", newwords, " are words and new words."

END PROGRAM
~~~

~~~ bash
$ gfortran arrays.f90 -o arrays
$ ./arrays
 Test:    0.0000000       0.0000000       0.0000000       0.0000000
       and                                    are words and new words.
~~~

This time, all of the numerical values are set to zero and our characters have been expanded to be all spaces.

You may need to define specific values of an array or matrix manually in Fortran for a variety of reasons. To do so, Fortran uses the notation of parentheses and forward slashes, like `(/ 1, 3, 4, 8 /)`, in something called an array constructor. This methodology will only allow you to define one dimension at a time, so if you have a 2x2 matrix, you will have to define either `variable(1,:)` or `variable(:,1)` at one time. We will later cover defining a full multidimensional matrix later.

> Which of the following choices correctly matches the following declaration:
>
> `CHARACTER(5) :: lyrics(7)`?
>
> 1. `(/ 'A', 'N', 'D', ' ', 'I' /)`
> 2. `(/ 'AND I K', 'NOW WHE', 'N THAT ', 'HOTLINE', ' BLING!' /)`
> 3. `(/ 'AND I', ' KNOW', ' WHEN', ' THAT', ' HOTL', 'INE B', 'LING!' /)`
> 4. `(/ 'A', 'N', 'D', ' ', 'I', ' ', 'K' /)`

### Allocatable Variables

So far we have been _hard-coding_ the shapes of variables, which means we manually specify the lengths of each dimension prior to run time. It may be the case, however, that we will not know the length of each dimension needed until run time. This behavior could occur when we are reading in files with different sized variables. Or we could be filling a variable only with variables that meet a certain criteria. These cases require using a _dynamic allocation_ of variable shape. Dynamic allocation punts the reservation of memory for a variable until we specifically choose to allocate it and it also provides us the ability to free (or deallocate) that memory once we are done with the variable.

In Fortran, dynamic allocation is defined with the `ALLOCATABLE` keyword (similar to specifying `PARAMETER`) and using colons to specify the number of dimensions of a variable. The shape is defined using the `ALLOCATE` statement and the variable memory can be discarded using the `DEALLOCATE` statement. If you are working with a large amount of data that is read in and then manipulated or transferred into a few other variables, it may be necessary to use dynamic allocation if for nothing else than to ensure your program is able to run with your machine's memory.

Here we will define an allocatable variable of three dimensions as well as three variables that we will later use to allocate the lengths of each dimension:

~~~ f90
PROGRAM arrays
! by Bucky Badger
! This program will work with some arrays in Fortran
IMPLICIT NONE

REAL :: test(3,2)
CHARACTER :: words(5)
CHARACTER(34) :: newwords

REAL, ALLOCATABLE :: modeldata(:,:,:)
INTEGER :: nlon, nlat, ntime

nlon = 360
nlat = 180
ntime = 24 * 365

PRINT *, ALLOCATED(modeldata)
PRINT *, SHAPE(modeldata)

ALLOCATE(modeldata(nlon, nlat, ntime))

PRINT *, ALLOCATED(modeldata)
PRINT *, SHAPE(modeldata)

DEALLOCATE(modeldata)

test = 0.0
words = ''
newwords = ''

PRINT *,"Test: ", test(1:2,:)
PRINT *, words, " and ", newwords, " are words and new words."

END PROGRAM
~~~

~~~ bash
$ gfortran arrays.f90 -o arrays
$ ./arrays
 F
           0           0           0
 T
         360         180        8760
 Test:    0.0000000       0.0000000       0.0000000       0.0000000
       and                                    are words and new words.
~~~



## Writing Data

Writing works in a similar way to reading, except now we want our file action to be write and we want to use write instead of read. Write statements are very similar to the print statements that we worked on earlier.

We will write out some of the sample data to a file in our directory.

~~~ f90
PROGRAM arrays
! by Bucky Badger
! This program will work with some arrays in Fortran
IMPLICIT NONE

REAL :: test(3,2)
CHARACTER :: words(5)
CHARACTER(34) :: newwords

REAL, ALLOCATABLE :: modeldata(:,:,:)
INTEGER :: nlon, nlat, ntime

nlon = 360
nlat = 180
ntime = 24 * 365

PRINT *, ALLOCATED(modeldata)
PRINT *, SHAPE(modeldata)

ALLOCATE(modeldata(nlon, nlat, ntime))

PRINT *, ALLOCATED(modeldata)
PRINT *, SHAPE(modeldata)

DEALLOCATE(modeldata)

test(:,1) = (/ 100.0, 95.0, 97.0 /)
test(:,2) = (/ 74.0, 2.0, 103.0 /)
words = (/ 'bucky' /)
newwords = ''

PRINT *,"Test: ", test(1:2,:)
PRINT *, words, " and ", newwords, " are words and new words."

OPEN(UNIT=10, FILE='test_out.txt', ACTION='write')
WRITE(10, *) "Unformatted test"
WRITE(10, *) test
WRITE(10, *) "Formatted test"
WRITE(10, '(6(F8.4))') test
WRITE(10, *) "Abbreviated test"
WRITE(10, '(6(F3.1))') test
CLOSE(UNIT=10)

END PROGRAM
~~~

~~~ bash
$ gfortran arrays.f90 -o arrays
arrays.f90:12:

words = (/ 'bucky' /)
1
Error: Different shape for array assignment at (1) on dimension 1 (5 and 1)
~~~

Uh-oh! We used the array constructor to define words--with shape `(5)`--using a single element. Instead let's see what happens now when we define all five elements with the vector notation, but keep one of them of the wrong length.

~~~ f90
PROGRAM arrays
! by Bucky Badger
! This program will work with some arrays in Fortran
IMPLICIT NONE

REAL :: test(3,2)
CHARACTER :: words(5)
CHARACTER(34) :: newwords

REAL, ALLOCATABLE :: modeldata(:,:,:)
INTEGER :: nlon, nlat, ntime

nlon = 360
nlat = 180
ntime = 24 * 365

PRINT *, ALLOCATED(modeldata)
PRINT *, SHAPE(modeldata)

ALLOCATE(modeldata(nlon, nlat, ntime))

PRINT *, ALLOCATED(modeldata)
PRINT *, SHAPE(modeldata)

DEALLOCATE(modeldata)

test(:,1) = (/ 100.0, 95.0, 97.0 /)
test(:,2) = (/ 74.0, 2.0, 103.0 /)
words = (/ 'bucky', 'u', 'c', 'k', 'y' /)
newwords = ''

PRINT *,"Test: ", test(1:2,:)
PRINT *, words, " and ", newwords, " are words and new words."

OPEN(UNIT=10, FILE='test_out.txt', ACTION='write')
WRITE(10, *) "Unformatted test"
WRITE(10, *) test
WRITE(10, *) "Formatted test"
WRITE(10, '(6(F8.4))') test
WRITE(10, *) "Abbreviated test"
WRITE(10, '(6(F3.1))') test
CLOSE(UNIT=10)

END PROGRAM
~~~

~~~ bash
$ gfortran arrays.f90 -o arrays
arrays.f90:12.19:

words = (/ 'bucky', 'u', 'c', 'k', 'y' /)
                   1
Error: Different CHARACTER lengths (5/1) in array constructor at (1)
~~~

This time the compiler tells us we are using a character with length of 5 in a space that should have a length of 1. Now correct the first element to only a 'b' and try again.

~~~ f90
PROGRAM arrays
! by Bucky Badger
! This program will work with some arrays in Fortran
IMPLICIT NONE

REAL :: test(3,2)
CHARACTER :: words(5)
CHARACTER(34) :: newwords

REAL, ALLOCATABLE :: modeldata(:,:,:)
INTEGER :: nlon, nlat, ntime

nlon = 360
nlat = 180
ntime = 24 * 365

PRINT *, ALLOCATED(modeldata)
PRINT *, SHAPE(modeldata)

ALLOCATE(modeldata(nlon, nlat, ntime))

PRINT *, ALLOCATED(modeldata)
PRINT *, SHAPE(modeldata)

DEALLOCATE(modeldata)

test(:,1) = (/ 100.0, 95.0, 97.0 /)
test(:,2) = (/ 74.0, 2.0, 103.0 /)
words = (/ 'b', 'u', 'c', 'k', 'y' /)
newwords = ''

PRINT *,"Test: ", test(1:2,:)
PRINT *, words, " and ", newwords, " are words and new words."

OPEN(UNIT=10, FILE='test_out.txt', ACTION='write')
WRITE(10, *) "Unformatted test"
WRITE(10, *) test
WRITE(10, *) "Formatted test"
WRITE(10, '(6(F8.4))') test
WRITE(10, *) "Abbreviated test"
WRITE(10, '(6(F3.1))') test
CLOSE(UNIT=10)

END PROGRAM
~~~

~~~ bash
$ gfortran arrays.f90 -o arrays
$ ./arrays
 F
           0           0           0
 T
         360         180        8760
 Test:    100.00000       95.000000       74.000000       2.0000000
 bucky and                                    are words and new words.
$ cat test_out.txt
 Unformatted test
   100.00000       95.000000       97.000000       74.000000       2.0000000       103.00000
 Formatted test
100.0000 95.0000 97.0000 74.0000  2.0000103.0000
 Abbreviated test
************2.0***
~~~

Our `words` variable was correctly defined, so the program was able to compile properly. On run time, we only printed the first two columns of `test` but the full character strings were printed out. Examining the file we see that each line corresponds to a respective write statement--again, each write statement issues a new line. Based on our experience from last class, the formatting gave what we likely suspected: with only a width of 3 in the final line, and one of those belonging to the decimal places, most of our test values would not fit and Fortran therefore prints instead that given length of asterisks.

If we just run our program again, we will see what happens to the output file again.

~~~ bash
$ gfortran arrays.f90 -o arrays
$ ./arrays
 F
           0           0           0
 T
         360         180        8760
 Test:    100.00000       95.000000       74.000000       2.0000000
 bucky and                                    are words and new words.
$ cat test_out.txt
 Unformatted test
   100.00000       95.000000       97.000000       74.000000       2.0000000       103.00000
 Formatted test
100.0000 95.0000 97.0000 74.0000  2.0000103.0000
 Abbreviated test
************2.0***
~~~

Is that expected? That Fortran replaced the file with a new one containing the data? Perhaps. But what if you are wanting to use a continuous log file among different programs? Or something else where you want to keep track of the output each time? You can specify the position keyword to append to the file instead:

~~~ f90
PROGRAM arrays
! by Bucky Badger
! This program will work with some arrays in Fortran
IMPLICIT NONE

REAL :: test(3,2)
CHARACTER :: words(5)
CHARACTER(34) :: newwords

REAL, ALLOCATABLE :: modeldata(:,:,:)
INTEGER :: nlon, nlat, ntime

nlon = 360
nlat = 180
ntime = 24 * 365

PRINT *, ALLOCATED(modeldata)
PRINT *, SHAPE(modeldata)

ALLOCATE(modeldata(nlon, nlat, ntime))

PRINT *, ALLOCATED(modeldata)
PRINT *, SHAPE(modeldata)

DEALLOCATE(modeldata)

test(:,1) = (/ 100.0, 95.0, 97.0 /)
test(:,2) = (/ 74.0, 2.0, 103.0 /)
words = (/ 'b', 'u', 'c', 'k', 'y' /)
newwords = ''

PRINT *,"Test: ", test(1:2,:)
PRINT *, words, " and ", newwords, " are words and new words."

OPEN(UNIT=10, FILE='test_out.txt', ACTION='write', POSITION='append')
WRITE(10, *) "Unformatted test"
WRITE(10, *) test
WRITE(10, *) "Formatted test"
WRITE(10, '(6(F8.4))') test
WRITE(10, *) "Abbreviated test"
WRITE(10, '(6(F3.1))') test
CLOSE(UNIT=10)

END PROGRAM
~~~

~~~ bash
$ gfortran arrays.f90 -o arrays
$ ./arrays
 F
           0           0           0
 T
         360         180        8760
 Test:    100.00000       95.000000       74.000000       2.0000000
 bucky and                                    are words and new words.
$ cat test_out.txt
 Unformatted test
   100.00000       95.000000       97.000000       74.000000       2.0000000       103.00000
 Formatted test
100.0000 95.0000 97.0000 74.0000  2.0000103.0000
 Abbreviated test
************2.0***
 Unformatted test
   100.00000       95.000000       97.000000       74.000000       2.0000000       103.00000
 Formatted test
100.0000 95.0000 97.0000 74.0000  2.0000103.0000
 Abbreviated test
************2.0***
~~~

## Masking and Logicals

Recalling from our very first class, we saw that a variable can be assigned a logical value, i.e. true or false, as the result of some logical expression. As an example, if `x = 3` and `y = x == 4`, then `y` will be `false`; if `y = x < 4`, then `y` will be `true`. We can do the same thing in Fortran to construct masks--and we can even do it for full matrices. We want to declare a logical variable of the same shape as `test` that will store the results of where our test values are a B or greater (80.0 or higher).

~~~ f90
PROGRAM arrays
! by Bucky Badger
! This program will work with some arrays in Fortran
IMPLICIT NONE

REAL :: test(3,2)
CHARACTER :: words(5)
CHARACTER(34) :: newwords
LOGICAL :: passing(3,2)

REAL, ALLOCATABLE :: modeldata(:,:,:)
INTEGER :: nlon, nlat, ntime

nlon = 360
nlat = 180
ntime = 24 * 365

PRINT *, ALLOCATED(modeldata)
PRINT *, SHAPE(modeldata)

ALLOCATE(modeldata(nlon, nlat, ntime))

PRINT *, ALLOCATED(modeldata)
PRINT *, SHAPE(modeldata)

DEALLOCATE(modeldata)

test(:,1) = (/ 100.0, 95.0, 97.0 /)
test(:,2) = (/ 74.0, 2.0, 103.0 /)
words = (/ 'b', 'u', 'c', 'k', 'y' /)
newwords = ''

passing = test > 80.0

PRINT *,"Test: ", test(1:2,:)
PRINT *, words, " and ", newwords, " are words and new words."
PRINT *, "Were scores passing? :"
PRINT *, passing(1:2,:)

OPEN(UNIT=10, FILE='test_out.txt', ACTION='write', POSITION='append')
WRITE(10, *) "Unformatted test"
WRITE(10, *) test
WRITE(10, *) "Formatted test"
WRITE(10, '(6(F8.4))') test
WRITE(10, *) "Abbreviated test"
WRITE(10, '(6(F3.1))') test
CLOSE(UNIT=10)

END PROGRAM
~~~

~~~ bash
$ gfortran arrays.f90 -o arrays
$ ./arrays
 F
           0           0           0
 T
         360         180        8760
 Test:    100.00000       95.000000       74.000000       2.0000000
 bucky and                                    are words and new words.
 Were scores passing? :
 T T F F
~~~

This mask can come in handy when calling certain [intrinsic functions](https://gcc.gnu.org/onlinedocs/gcc-4.4.7/gfortran/Intrinsic-Procedures.html#Intrinsic-Procedures) as we will see in the lab assignment.

## If Statements

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

Note that either the `else if` or the `else` statements are optional when creating an if block. If you have no else statement and none of the conditions are met, then the code will just continue on.


# Lab Assignment

[Follow this link to our assignment](/assignments/03-fortran-arrays-io.html).
