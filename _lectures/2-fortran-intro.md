---
layout: lecture
published: false
title: Introduction to Fortran
---

# Fortran Overview

Fortran is a compiled generic programming language, with a strong penchant towards numerical--and especially matrix--computations. Over time, Fortran standards have evolved since its creation in the mid-20th century to add more modern features like *object-oriented programming* (which we will not be touching on until Python). One of the recent standards, Fortran 95, is what we will be focusing on for this course because this was the version that provided an easy interface to *parallelization*--meaning splitting up a large set of computations across multiple computer processors. But do note that most--if not all--of the features present in older versions (e.g. FORTRAN 77) are *backwards compatible*, meaning they can still be used in Fortran 95.

One legacy of Fortran is, as you just saw in the previous paragraph with the stylized '77 name, the use of ALL UPPER CASE LETTERS. As the language has evolved, the use of caps has relaxed from a requisite to an option. But keeping the backwards compatibility means that Fortran is case insensitive: you can define a variable named `data` and access it with `data`, `dATA`, `DATA`, or any other combination therein. For readability purposes, we will be using upper case for any language statements and lower case when referencing variables. One other intricacy of Fortran to keep in mind is that lines cannot be longer than 132 characters; if you need to spill over one statement onto another line, use the ampersand (`&`) to terminate the first line and continue on the following.

Let's start our first piece of Fortran code! Open up your terminal and edit a new file called `calculator.f95`. This program is going to calculate a variety of meteorological quantities based on a few observations we have.

~~~ bash
$ nano calculator.f95
~~~

While the file extension is mostly arbitrary, and only a `.f` would suffice, we include the year so that we can remember what standard we were crafting our program with and so that other programmers will also know this.

## Code Structure

The highest level of Fortran code is the main program file, which is the first set of instructions that are read by the computer. Fortran also has lower level parts of the code called subprograms and modules. Subprograms can be split up into two parts: functions and subroutines. Functions are intended more towards some computation, where you enter some value or values and return one variable (like calculating the sine of a number or the greatest common denominator). Subroutines, on the other hand, operate direction on the specified input variables. Modules are like a cluster of subprograms that usually share a common theme; it may be easier to think of them as plug-and-play libraries providing functionality related to some set of operations. Later on we will be working with a module called LAPACK which will provide us a wide array of linear algebra operations.

How your main program file, subprograms, and modules fit into the code is mostly a personal choice. You can place all of your code into one single file or you can split up different subprograms into respective files. All that matters is, when it comes time to compile your program, you supply all of the required files. We will touch on best practices about how to structure your Fortran code later in this unit, but for now most of our Fortran code will be placed into one file.

So we start our main program with a statement that declares it is a program and gives it a name, and end our main program with a similar declaration:

~~~ f90
PROGRAM calculator

END PROGRAM calcaulator
~~~

Functions are called with the form `output = FUNCTIONNAME(input1, maybeinput2, maybeinput3)`, while subroutines--since they operate on the specified inputs--are called as `CALL SUBROUTINENAME(input1, maybeinput2, maybeput3). While all the variables specified are input variables, consider the case where an input has not previously been defined (whether it be a single zero or an array of zeros). The subroutine could calculate some value based on some of the input variables and return the output as one of the other variables.

## Commenting

Comments in Fortran are indicated by an exclamation mark `!`. Comments do not have to start at the beginning of a line, and in fact can be placed on a line after a statement, but anything after a `!` is considered to be a comment. These are all examples of comments within code:

~~~ f90
! This section computes the square of the variable.
!   y = x ** 2, where y is the square of x.
y = x ** 2   ! See, this is where it is computed.
~~~

The only code that will actually be computed is the third line, `y = x * x`, because the operation on the second line occurred _after_ the exclamation mark.

We want to add a brief comment that describes our program. While this can go anywhere, while we're here we can add it to the top.

~~~ f90
PROGRAM calculator
! by Bucky Badger
! This program calculates meteorological variables.

END PROGRAM calculator
~~~


## Basic Variable Types

As we briefly discussed during last class, Fortran does not have the same typing flexibility as interactive languages like Python or Matlab. By default, Fortran defines a small set of variables (including i, j, and k) as integers to be assumed for counting or loop variables and everything else as real numbers. It is possible to overried this behavior using the `IMPLICIT` statement where you can specify which variables you wish for Fortran to retain this behavior for. But, given the general undesired behavior that this all creates, a legacy practice within Fortran is to define `IMPLICIT NONE` in each program--signalling to the compiler that you do not want this type assumption to be made for any variable.

So we add this line to our program on the next line.

~~~ f90
PROGRAM calculator
! by Bucky Badger
! This program calculates meteorological variables.
IMPLICIT NONE

END PROGRAM calculator
~~~

Variables used in Fortran include INTEGER, REAL, CHARACTER (also known as a string), LOGICAL, and IMAGINARY. Variable declarations occur at the beginning of the program and follow the general form `TYPENAME :: variablename, optionalsecondvariable, optionalthirdvariable`. For our first computation, we are going to decompose wind speed and direction observations into the u and v components of the wind. The variables we will need to define are the original wind speed and direction, and resulting u and v components.

~~~ f90
PROGRAM calculator
! by Bucky Badger
! This program calculates meteorological variables.
IMPLICIT NONE

! These are our original variables
REAL :: spd, dir

! These are our computed variables
REAL :: u, v

END PROGRAM calculator
~~~

Another variable we will need in a moment is pi (&#960;). In Fortran, you can define a variable to specifically be a parameter, meaning its value is given initially and cannot be changed. While this may seem ancillary, it will be important later on. Parameter values are defined at the same time the type is defined.

~~~ f90
PROGRAM calculator
! by Bucky Badger
! This program calculates meteorological variables.
IMPLICIT NONE

! These are our original variables
REAL :: spd, dir

! These are our computed variables
REAL :: u, v

! This is our constant
REAL, PARAMETER :: pi=3.14159

END PROGRAM calculator
~~~


## The Compiler

When we talk about Fortran being a *compiled* language, we are referring to the fact that a script or set of scripts must be run through a program called a compiler that assembles, or compiles, this source code into a form readable by the processor. Generally all programs must at some point go through a transformation stage like this, but when that step must be explicity and separately performed to generate the machine code, we term it a compiled language. Once you run source code through the compiler, a separate executable file will be generated that you run to perform the actions specified in your source code. (There are even special compilers, termed *transcompilers*, that can compile source code from one language into another.)

There are a variety of Fortran compilers available--both commercial and free. Examples of compilers include `ifort` (Intel), `pgfort` (The Portland Group), `G95`, and `gfortran` (GNU); the former two are commercial compilers and the latter two are open source. Depending on the specific compiler and options used, the compiler may, in addition to transforming the code from source, also intelligently optimize the code so that it runs faster or uses less memory. The level to which they can optimize is also dependent on the system used. Commercial compiler vendors who also sell computer processors may, for example, be able to offer the best optimization for their processor.

For this class, we will be using the `gfortran` compiler. Now that we have an example program created, let's run it through our compiler!

~~~ bash
$ gfortran calculator.f90
~~~

Once you run this command, you should now see a file called `a.out` in that directory, which is the default output file name for `gfortran`. This file is the compiled executable that you can now run:

~~~ bash
$ ./a.out
~~~

We did not get any errors, so our program works! Right now, all we have done in our program is define the variables, so we do not expect anything to happen.

If you want to change the name of the output file, you can specify the `-o` option followed by the desired file name when calling the compiler:

~~~ bash
$ gfortran calculator.f90 -o calculator
$ ./calculator
~~~~

(Alternatively, you could rename the `a.out` file to the name you desire, but this is the more systematic method--especially when it comes to adding compilation as one step in a chain of commands.)

## Intrinsic Functions

Calculations, like the trigonometric functions, have been known for centuries. One of the earliest uses of computers was trajectory calculations during World War II that relied heavily upon many of these basic functions. Some functions, like the trigonometric ones, that have widespread applicability and use are already built into the programming language (giving them the name _intrinsic functions_), so there is no need for us to reinvent the wheel. Instead, we can rely upon these built in functions which are supplied by the compiler. [Here](https://gcc.gnu.org/onlinedocs/gcc-4.4.7/gfortran/Intrinsic-Procedures.html#Intrinsic-Procedures) is a list of intrinsic functions that are included in our compiler version.

First, we should include an example observation in our code. The Dane County Airport (KMSN) METAR observation on 01 January 2017 at 16:05Z was 11 knots at 200 degrees. Remember that, while the direction is that which the wind comes _from_, we need the direction _towards_ to properly decompose the vector, so we will subtract 180 degrees from the direction.

~~~ f90
PROGRAM calculator
! by Bucky Badger
! This program calculates meteorological variables.
IMPLICIT NONE

! These are our original variables
REAL :: spd, dir

! These are our computed variables
REAL :: u, v

! This is our constant
REAL, PARAMETER :: pi=3.14159

spd = 11.0               ! this is kts
spd = spd * 1.151        ! this is mph

dir = 200.0 - 180.0      ! direction wind is going

END PROGRAM calculator
~~~

Now, as we saw before, functions are called with the syntax `output = FUNCTIONNAME(input, possiblyinput2, andsoon)`. The functions `COS` and `SIN` are available intrinsically. Based on the documentation linked above, we know that `COS` and `SIN` accept an angle in radians only, so we first need to convert our direction into radians. We can use the conversion `radians = degrees * (pi / 180.0)`. Unfortunately, pi is not an intrinsic variable--it is impossible for us to represent an irrational number precisely in a computer, and the language creators do not know what precision is desired. That is why we had to manually define our pi parameter earlier.

~~~ f90
PROGRAM calculator
! by Bucky Badger
! This program calculates meteorological variables.
IMPLICIT NONE

! These are our original variables
REAL :: spd, dir

! These are our computed variables
REAL :: u, v

! This is our constant
REAL, PARAMETER :: pi=3.14159

spd = 11.0               ! this is kts
spd = spd * 1.151        ! this is mph

dir = 200.0 - 180.0      ! direction wind is going in degrees
dir = dir * (pi / 180.0) ! direction wind is going in radians

END PROGRAM calculator
~~~

We now have all the pieces necessary to calculate our wind decomposition. Here is a diagram (neither to scale or at the correct angle) showing our situation:

![Decomposition of wind vector into zonal and meridional components.](/static/images/fortran/wind_decomp.png){: height="400px" width="400px"}

We see the zonal (meridional) component is opposite (adjacent) our defined toward wind vector.

~~~ f90
PROGRAM calculator
! by Bucky Badger
! This program calculates meteorological variables.
IMPLICIT NONE

! These are our original variables
REAL :: spd, dir

! These are our computed variables
REAL :: u, v

! This is our constant
REAL, PARAMETER :: pi=3.14159

spd = 11.0               ! this is kts
spd = spd * 1.151        ! this is mph

dir = 200.0 - 180.0      ! direction wind is going in degrees
dir = dir * (pi / 180.0) ! direction wind is going in radians

u = spd * COS(dir)
v = spd * SIN(dir)
~~~

## Print Statements

To print something out to the terminal in Fortran, we use `PRINT`. Print first requires a formatting character prior to specifying what to print. This formatting can be used to make a pretty table, properly formatted numbers, or a variety of other uses. To print the variables in the default format, specify a `*`, followed by the variables.

~~~ f90
PROGRAM calculator
! by Bucky Badger
! This program calculates meteorological variables.
IMPLICIT NONE

! These are our original variables
REAL :: spd, dir

! These are our computed variables
REAL :: u, v

! This is our constant
REAL, PARAMETER :: pi=3.14159

spd = 11.0               ! this is kts
spd = spd * 1.151        ! this is mph

dir = 200.0 - 180.0      ! direction wind is going in degrees
dir = dir * (pi / 180.0) ! direction wind is going in radians

u = spd * COS(dir)
v = spd * SIN(dir)

PRINT *, u, v

END PROGRAM calculator
~~~

Besides previously defined variables, you can specify strings or numbers within the print statement to print as well. In fact, you can do this without including any variables. This is often used for debugging or informational purposes. Let's add a few different print statements to make our program more descriptive as it runs.

~~~ f90
PROGRAM calculator
! by Bucky Badger
! This program calculates meteorological variables.
IMPLICIT NONE

! These are our original variables
REAL :: spd, dir

! These are our computed variables
REAL :: u, v

! This is our constant
REAL, PARAMETER :: pi=3.14159

PRINT *, "Program is now starting."

spd = 11.0               ! this is kts
spd = spd * 1.151        ! this is mph

dir = 200.0 - 180.0      ! direction wind is going in degrees
dir = dir * (pi / 180.0) ! direction wind is going in radians

PRINT *, "Outputs are now being calculated."

u = spd * COS(dir)
v = spd * SIN(dir)

PRINT *, "The zonal speed is ", u, " mph."
PRINT *, "But, the meridional speed is ", v, " mph."
PRINT *, "Program has finished."

END PROGRAM calculator
~~~

Alright, we have added a lot to our program since we first tried compiling it. Let's compile it now and see what happens!

~~~ bash
$ gfortran calculator.f95 -o calculator
$ ./calculator
 Program is now starting.
 Outputs are now being calculated.
 The zonal speed is    11.897449      mph.
 But, the meridional speed is    4.3303142      mph.
 Program has finished.
~~~

Wow! It runs and, more importantly, we are getting some output now. Notice how there are lots of spaces surrounding the numbers, though? Since we used the `*` in our print statements, Fortran is printing the default spacing for all the characters. We can modify our print statement to format the numbers to look a little neater.


|Type               |Symbol |First Parameter  |Second Parameter |
|-------------------|-------|-----------------|-----------------|
|Integer            |I*w.m* |*Width*          |*Leading zeros*  |
|Logical            |L*w*   |*Leading spaces* |                 |
|Character          |A*w*   |*Width*          |                 |
|Real               |Fw.d   |Width            |Decimal places   |
|Real (exponential) |Ew.d   |Width            |Decimal places   |
|Tab character      |Tn     |Tab-to position  |                 |
|Space character    |X      |Spaces to add    |                 |
|Array or many      |r...   |Times repeated   |                 |
(In this table, note that _optional_ parts of the formatting string are italicized.)

Formatting strings are constructed using the above table dependending on the variable type or types. And it is literally a formatting string, meaning we must define it with quotes and within the quotes a set of enclosing parentheses. If you specify a format string, each variable to be printed _must_ have a formatting string associated with it; this is either accomplished by adding individual format strings for each variable separated by commas or alternatively by using one formatting string and placing the number of times to repeat that format immediately before it. Let's now modify our old print statements as well as adding one more to demonstrate the repetition.

~~~ f90
PROGRAM calculator
! by Bucky Badger
! This program calculates meteorological variables.
IMPLICIT NONE

! These are our original variables
REAL :: spd, dir

! These are our computed variables
REAL :: u, v

! This is our constant
REAL, PARAMETER :: pi=3.14159

PRINT *, "Program is now starting."

spd = 11.0               ! this is kts
spd = spd * 1.151        ! this is mph

dir = 200.0 - 180.0      ! direction wind is going in degrees
dir = dir * (pi / 180.0) ! direction wind is going in radians

PRINT *, "Outputs are now being calculated."

u = spd * COS(dir)
v = spd * SIN(dir)

PRINT *, "The zonal speed is ", u, " mph."
PRINT "(A,F4.2,A4)", "But, the meridional speed is ", v, " mph."
PRINT "(2(F5.3),5X,2(F6.3))", u, v, u, v
PRINT "(T20,A)", "Program has finished."

END PROGRAM calculator
~~~

We've made modifications to the last two lines as well as added a line between those two. Let's recompile this puppy and see what each of these do.

~~~ bash
$ gfortran calculator.f95 -o calculator
$ ./calculator
 Program is now starting.
 Outputs are now being calculated.
 The zonal speed is    11.897449      mph.
But, the meridional speed is 4.33 mph
*****4.330     11.897 4.330
                   Program has finished.
~~~

A few things have changed now. The first line we manually specified the formatting specifier that starts with "But,..." is now touching the left side of the terminal when printing. This is another legacy component from the age of dot matrix printers that remains in the language. Moving on to the actual text printed, the first specifier we added was a lone `A`, which means print the whole character variable. The second specifier asked for a float that takes up four total characters with two of those as decimal places. We indeed count four characters (the decimal point counts as one!) with no spaces padded beyond those we manually added to the characters. Finally, we specified `A4`, meaning we wanted a width of four characters. If you look at the actual string we want to print, it has _five_ characters (see the space right after the left quote?), so the fifth character--the period--was not printed.

The second line is the newly inserted line where we wanted to print u and v twice. We first requested a float of width five with at least three decimal places--and because that is surrounded by parentheses with a preceding 2, this specification is used twice. But look at our first variable: it is all stars! When Fortran cannot display the number as requested (usually because you have been too skimpy on the width), it will instead print asterisks in that place. Since we asked for a width of five, we received five asterisks. Our second number, however, was able to fit since it is less than 10 and can fit within five characters. Our next formatting string part requested five spaces, which were added. Finally, we rehashed the repeated string formatter but this time requested two floats with width of _six_ instead of five. Luckily enough, both of our variables met that criteria. 

The final line is spaced over quite far! That's because our first specifier for this line requested we tab over to the 20th character. Once there, our character formatter then requested we print the whole character variable. Alternatively, we could have written `TR20`, which would have tabbed from the right hand side of the terminal characters and began printing.

Let's clean up our print statements a bit before we move on. Specifically, we are going to delete the three lines that print various iterations of u and v and add one new line.

~~~ f90
PROGRAM calculator
! by Bucky Badger
! This program calculates meteorological variables.
IMPLICIT NONE

! These are our original variables
REAL :: spd, dir

! These are our computed variables
REAL :: u, v

! This is our constant
REAL, PARAMETER :: pi=3.14159

PRINT *, "Program is now starting."

spd = 11.0               ! this is kts
spd = spd * 1.151        ! this is mph

dir = 200.0 - 180.0      ! direction wind is going in degrees
dir = dir * (pi / 180.0) ! direction wind is going in radians

PRINT *, "Outputs are now being calculated."

u = spd * COS(dir)
v = spd * SIN(dir)

PRINT *, spd, dir, u, v

PRINT "(T20,A)", "Program has finished."

END PROGRAM calculator
~~~

## Reading and Writing

Fortran has the ability natively to read in and write out text files and Fortran-specific binary files.



# Lab Assignment

[Follow this link to our assignment](/assignments/2-fortran-intro.html).
