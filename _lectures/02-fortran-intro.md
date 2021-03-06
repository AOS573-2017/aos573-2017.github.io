---
layout: lecture
published: true
title: Introduction to Fortran
---

# Prelude

Prior to starting today's lab, we need the same datasets so we can examine data together. Go ahead and get today's lesson by `git clone` and then navigate into it:

~~~ bash
$ git clone https://github.com/aos573/fortran-week-one
Cloning into 'fortran-week-one'...
remote: Counting objects: 8, done.
remote: Compressing objects: 100% (7/7), done.
remote: Total 8 (delta 0), reused 8 (delta 0), pack-reused 0
Unpacking objects: 100% (8/8), done.
$ cd fortran-week-one
~~~

# Fortran Overview

Fortran is a compiled generic programming language, with a strong penchant towards numerical--and especially matrix--computations. Over time, Fortran standards have evolved since its creation in the mid-20th century to add more modern features like *object-oriented programming* (which we will not be touching on until Python). One of the recent standards, Fortran 95, is what we will be focusing on for this course because this was the version that provided an easy interface to *parallelization*--meaning splitting up a large set of computations across multiple computer processors. But do note that most--if not all--of the features present in older versions (e.g. FORTRAN 77) are *backwards compatible*, meaning they can still be used in Fortran 95.

One legacy of Fortran is, as you just saw in the previous paragraph with the stylized '77 name, the use of ALL UPPER CASE LETTERS. As the language has evolved, the use of caps has relaxed from a requisite to an option. But keeping the backwards compatibility means that Fortran is case insensitive: you can define a variable named `data` and access it with `data`, `dATA`, `DATA`, or any other combination therein. For readability purposes, we will be using upper case for any language statements and lower case when referencing variables. One other intricacy of Fortran to keep in mind is that lines cannot be longer than 132 characters; if you need to spill over one statement onto another line, use the ampersand (`&`) to terminate the first line and continue on the following.

Let's start our first piece of Fortran code! Open up your terminal and edit a new file called `calculator.f90`. This program is going to calculate a variety of meteorological quantities based on a few observations we have.

~~~ bash
$ nano calculator.f90
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

Functions are called with the form `output = FUNCTIONNAME(input1, maybeinput2, maybeinput3)`, while subroutines--since they operate on the specified inputs--are called as `CALL SUBROUTINENAME(inputoutput, possibleinputoutput2, possibleinputoutput)`. While all the variables specified are input variables, consider the case where an input has not previously been defined (whether it be a single zero or an array of zeros). The subroutine could calculate some value based on some of the input variables and return the output as one of the other variables.

## Commenting

Comments in Fortran are indicated by an exclamation mark `!`. Comments do not have to start at the beginning of a line, and in fact can be placed on a line after a statement, but anything after a `!` is considered to be a comment--except whenever the exclamation mark is within a character definition, like `'ohmy!'`. These are all examples of comments within code:

~~~ f90
! This section computes the square of the variable.
!   y = x ** 2, where y is the square of x.
y = x ** 2   ! See, this is where it is computed.
~~~

The only code that will actually be computed is the third line, `y = x ** 2`, because the operation on the second line occurred _after_ the exclamation mark.

We want to add a brief comment that describes our program. While this can go anywhere, while we're here we can add it to the top.

~~~ f90
PROGRAM calculator
! by Bucky Badger
! This program calculates meteorological variables.

END PROGRAM calculator
~~~


## Basic Variable Types

As we briefly discussed during last class, Fortran does not have the same typing flexibility as interactive languages like Python or Matlab. By default, Fortran defines a small set of variables (including i, j, and k) as integers to be assumed for counting or loop variables and everything else as real numbers. It is possible to overried this behavior using the `IMPLICIT` statement where you can specify which variables you wish for Fortran to retain this behavior for. But, given the general undesired behavior that this all creates, a legacy practice within Fortran is to define `IMPLICIT NONE` in each program--signaling to the compiler that you do not want this type assumption to be made for any variable.

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
REAL :: uwind, vwind

END PROGRAM calculator
~~~

Another variable we will need in a moment is pi. In Fortran, you can define a variable to specifically be a parameter, meaning its value is given initially and cannot be changed. While this may seem ancillary, it will be important later on. Parameter values are defined at the same time the type is defined.

~~~ f90
PROGRAM calculator
! by Bucky Badger
! This program calculates meteorological variables.
IMPLICIT NONE

! These are our original variables
REAL :: spd, dir

! These are our computed variables
REAL :: uwind, vwind

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
REAL :: uwind, vwind

! This is our constant
REAL, PARAMETER :: pi=3.14159

spd = 11.0               ! this is kts
spd = spd * 1.151        ! this is mph

dir = 200.0
dir = dir - 180.0        ! direction wind is going

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
REAL :: uwind, vwind

! This is our constant
REAL, PARAMETER :: pi=3.14159

spd = 11.0               ! this is kts
spd = spd * 1.151        ! this is mph

dir = 200.0
dir = dir - 180.0        ! direction wind is going in degrees
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
REAL :: uwind, vwind

! This is our constant
REAL, PARAMETER :: pi=3.14159

spd = 11.0               ! this is kts
spd = spd * 1.151        ! this is mph

dir = 200.0
dir = dir - 180.0        ! direction wind is going in degrees
dir = dir * (pi / 180.0) ! direction wind is going in radians

uwind = spd * SIN(dir)
vwind = spd * COS(dir)
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
REAL :: uwind, vwind

! This is our constant
REAL, PARAMETER :: pi=3.14159

spd = 11.0               ! this is kts
spd = spd * 1.151        ! this is mph

dir = 200.0
dir = dir - 180.0        ! direction wind is going in degrees
dir = dir * (pi / 180.0) ! direction wind is going in radians

uwind = spd * SIN(dir)
vwind = spd * COS(dir)

PRINT *, uwind, vwind

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
REAL :: uwind, vwind

! This is our constant
REAL, PARAMETER :: pi=3.14159

PRINT *, "Program is now starting."

spd = 11.0               ! this is kts
spd = spd * 1.151        ! this is mph

dir = 200.0
dir = dir - 180.0        ! direction wind is going in degrees
dir = dir * (pi / 180.0) ! direction wind is going in radians

uwind = spd * SIN(dir)
vwind = spd * COS(dir)

PRINT *, uwind, vwind
PRINT *, "The program has finished."
PRINT *, "Go Bucky!"

END PROGRAM calculator
~~~

Alright, we have added a lot to our program since we first tried compiling it. Let's compile it now and see what happens!

~~~ bash
$ gfortran calculator.f90 -o calculator
$ ./calculator
 Program is now starting.
   4.3303142       11.897449  
 The program has finished.
 Go Bucky!
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

Formatting strings are constructed using the above table depending on the variable type or types. And it is literally a formatting string, meaning we must define it with quotes and within the quotes a set of enclosing parentheses. If you specify a format string, each variable to be printed _must_ have a formatting string associated with it; this is either accomplished by adding individual format strings for each variable separated by commas or alternatively by using one formatting string and placing the number of times to repeat that format immediately before it. Let's now modify our old print statements as well as adding one more to demonstrate the repetition.

~~~ f90
PROGRAM calculator
! by Bucky Badger
! This program calculates meteorological variables.
IMPLICIT NONE

! These are our original variables
REAL :: spd, dir

! These are our computed variables
REAL :: uwind, vwind

! This is our constant
REAL, PARAMETER :: pi=3.14159

PRINT *, "Program is now starting."

spd = 11.0               ! this is kts
spd = spd * 1.151        ! this is mph

dir = 200.0
dir = dir - 180.0        ! direction wind is going in degrees
dir = dir * (pi / 180.0) ! direction wind is going in radians

uwind = spd * SIN(dir)
vwind = spd * COS(dir)

PRINT '(F5.2,3X,F5.2)', uwind, vwind
PRINT '(F5.3,3X,F5.3)', uwind, vwind
PRINT '(T20,A)', "The program has finished."
PRINT *, "Go Bucky!"

END PROGRAM calculator
~~~

We've made modifications to a few existing lines as well as adding another with slightly different formatting (having more decimal places). Let's recompile this puppy and see what each of these do.

~~~ bash
$ gfortran calculator.f90 -o calculator
$ ./calculator
 Program is now starting.
 4.33   11.90
4.330   *****
                   The program has finished.
 Go Bucky!
~~~

A few things have changed now. With the two lines now that have the wind formatting specified, they have indeed shrunk in size. For the first line, we now see a width of 5 with 2 on the right size of the decimal place: that means up to 2 characters on the right side of the decimal place, 1 character for the decimal place, and up to 2 more for the numbers on the left side of the decimal place. In the case of 4.33, because it is less than 10, the leftmost character space is padded with a blank space. The numbers are also separated by three space characters as we specified with `3X`.

For the second line of printing, there is some strange behavior! We specified 3 characters for the decimal aspect, 1 for the decimal, and only 1 reserved for the number left of the decimal. That means the number 11.90 cannot fit into the space anymore! In this case, Fortran instead prints out asterisks of that width size to indicate the lack of fitting.

The finished program line is spaced over quite far! That's because our first specifier for this line requested we tab over to the 20th character. Once there, our character formatter then requested we print the whole character variable. Alternatively, we could have written `TR20`, which would have tabbed from the _right hand side_ of the terminal characters and continued printing.

Let's clean up our print statements a bit before we move on since we are currently duplicating content.

~~~ f90
PROGRAM calculator
! by Bucky Badger
! This program calculates meteorological variables.
IMPLICIT NONE

! These are our original variables
REAL :: spd, dir

! These are our computed variables
REAL :: uwind, vwind

! This is our constant
REAL, PARAMETER :: pi=3.14159

PRINT *, "Program is now starting."

spd = 11.0               ! this is kts
spd = spd * 1.151        ! this is mph

dir = 200.0
dir = dir - 180.0        ! direction wind is going in degrees
dir = dir * (pi / 180.0) ! direction wind is going in radians

uwind = spd * SIN(dir)
vwind = spd * COS(dir)

PRINT *, uwind, vwind
PRINT '(T20,A)', "Program has finished."
PRINT *, "Go Bucky!"

END PROGRAM calculator
~~~

## `Do` Loops

`Do` loops provide a quick way to perform or iterate an operation many times. In Fortran, we must define an iterator variable which is used to not only indicate to Fortran how many times to loop but also for us to keep track of what loop iteration we are on. Loops are defined of the form:

~~~ f90
DO var = nstart, nend, ninc
   something done here
END DO
~~~

where `var` is your iterator integer variable, `nstart` is some integer starting number, `nend` is some integer ending number that is included, and `ninc` is an optional integer increment number (which is assumed to be 1 if you do not specify otherwise). So if you specify `1,5`, the loop will iterate from 1 to 5 by 1 each time: `1 2 3 4 5`. If you specify `1,5,1`, it will still iterate `1 2 3 4 5`. But if you specify `1,5,2`, it will iterate `1 3 5`.

Loops can also be *nested* within each other:

~~~ f90
DO var1 = nstart1 , nend1
  something possibly done here for every outer loop
  DO var2 = nstart2 , nend2
    something done here for every outer and inner loop
  END DO
END DO
~~~

Nested loops are often used when you need to loop over a two dimensional variable, but we will get to those in next class.

Note in these examples, the statements within the loop are indented by two additional spaces. Here is a good time to bring up the practice of making code readable. You will often run into a problem with your code that requires you to find the problem, a process called *debugging*, and that requires literally reading your code. While spacing is not mandatory in Fortran, doing so makes the debugging process easier so you can keep track of what code is part of which loop--this is especially a problem when you have many lines of code within a loop.

The amount of spaces (or the use of a tab instead) varies by language, profession, social circle, and a variety of other conventions. Some workplaces or projects have a set convention that must be followed. For your own work in Fortran, it comes down to personal preference. Two and four spaces are fairly common spacing distances used. Tabs, while used some times, can cause some issues because of the way different operating systems define what a tab actually is. Spacing in examples will follow two per level for the notes to save space.

Now, the end goal is to be able to loop over observations from a file and perform our conversion on each one. For now, we will add a do loop in our program that performs our conversion for eight times. Note we first need to define a new integer variable to serve as our operator.

~~~ f90
PROGRAM calculator
! by Bucky Badger
! This program calculates meteorological variables.
IMPLICIT NONE

! These are our original variables
REAL :: spd, dir

! These are our computed variables
REAL :: uwind, vwind

! This is our constant
REAL, PARAMETER :: pi=3.14159

! This is our loop variable
INTEGER :: i

PRINT *, "Program is now starting."

DO i = 1, 8
  spd = 11.0               ! this is kts
  spd = spd * 1.151        ! this is mph

  dir = 200.0
  dir = dir - 180.0        ! direction wind is going in degrees
  dir = dir * (pi / 180.0) ! direction wind is going in radians

  uwind = spd * SIN(dir)
  vwind = spd * COS(dir)

  PRINT *, i, uwind, vwind
END DO

PRINT '(T20,A)', "Program has finished."
PRINT *, "Go Bucky!"

END PROGRAM calculator
~~~

Let's compile and run our program now and see what happens.

~~~ bash
$ gfortran calculator.f90 -o calculator
$ ./calculator
 Program is now starting.
           1   4.3303142       11.897449    
           2   4.3303142       11.897449    
           3   4.3303142       11.897449    
           4   4.3303142       11.897449    
           5   4.3303142       11.897449    
           6   4.3303142       11.897449    
           7   4.3303142       11.897449    
           8   4.3303142       11.897449  
                   Program has finished.
 Go Bucky!
~~~

We can see now our calculation was performed eight times--the number of times we specified in our loop--and that is confirmed by the `i` we added to our print statement.

## Reading

Fortran has the ability natively to read in and write out both text files and Fortran-specific binary files. We are going to focus on text files for now given their human-readable formatting. Luckily, we already downloaded some sample text data files when we cloned our weekly repository. Check out our sample file using `cat` or `less`:

~~~ bash
$ cat data/obs_crop.txt
MM/DD/YY        HH:MM   RH-PER  DIR-DEG WIND-KTS
01/01/16        00:52   62.28   250     12
01/01/16        06:52   70.46   240     12
01/01/16        12:52   73.77   270     8
01/01/16        18:52   68.33   260     10
01/02/16        00:52   71.69   240     12
01/02/16        06:52   65.84   300     9
01/02/16        12:52   80.55   240     6
01/02/16        18:52   63.24   230     10
~~~

We have a text file with lined up columns spaced by spaces. The first line of the file contains the column headers; the four columns are date, time, wind direction, and wind speed. If you count the characters and spaces, the first column starts at position 1, the second column starts at position 17, the third column starts at position 25, the fourth column starts at position 33, and the fifth column starts at position 41.

We must open the file, assign it an identifier, and provide some information about it (which we will see in just a minute). Once a file has been opened, each read command you provide will read in a single line of the file; that means you will need a loop set up that reads for every line. The read command requires that you specify the file identifier to read from, and for formatted files (i.e. non-binary, human-readable text files) the formatting of the line to be read in. Finally, after the loop, we should close the file. Let's walk through all of this for our file from above.

~~~ f90
PROGRAM calculator
! by Bucky Badger
! This program calculates meteorological variables.
IMPLICIT NONE

! These are our original variables
REAL :: spd, dir, rh

! These are our computed variables
REAL :: uwind, vwind

! This is our constant
REAL, PARAMETER :: pi=3.14159

! This is our loop variable
INTEGER :: i

PRINT *, "Program is now starting."

OPEN(UNIT=10, FILE='data/obs_crop.txt', ACTION='read', STATUS='old')
READ(10, *)  ! Skip the first line--column headers

DO i = 1, 8
  READ(10, '(24X,F8.2,F8.0,F8.0)') rh, dir, spd
  spd = spd * 1.151        ! this is mph

  dir = dir - 180.0        ! direction wind is going in degrees
  dir = dir * (pi / 180.0) ! direction wind is going in radians

  uwind = spd * SIN(dir)
  vwind = spd * COS(dir)

  PRINT *, i, uwind, vwind, rh
END DO

CLOSE(UNIT=10)

PRINT '(T20,A)', "Program has finished."
PRINT *, "Go Bucky!"

END PROGRAM calculator
~~~ 

The file identifier/unit number we chose is mostly arbitrary, but it _must_ be used consistently to reference our file when reading or closing. The action verb defines whether the file is read to be read from or written into. If you do not select one, both can occur (though that could cause some chaos). Finally, the status verb tells Fortran whether to expect the file to exist or not. It may be the case, at some point, Fortran should expect a file to exist. In this case, you would define the status as old. It may be the case that the file should _not_ exist, in which case you want the status to be new (this helps with accidental overwrites). Or, you do not have to define any status and it will work as always.

We also added an extra read statement at the beginning; in the paragraph above, it was noted that a read statement will read and advance past a whole line. In the case of our data, our first line had headers that we did not want to read. Similar to the asterisk notation for printing, you can supply an asterisk in the read formatting, which means "assume the best". You can place as few or as many variables in the read call as you like; Fortran will fill as many of them as possible. This can also mean that if you have a two-dimensional matrix, where one of the dimensions is equal to the quantity of variables on a given line, you can supply that full dimension of the variable and it will be filled. We will have a better example of this statement next class.

## Functions and Subroutines

The last topic we are going to cover in this lecture is the syntax of functions and subroutines. As mentioned at the outset, these are very similar subprograms--the main difference being how they handle output. Functions always return only one result, and thus can be used within another statement or on their own. Subroutines can return any number of variables by modifying some or all of the input variables, but they must be called on their own line. (As an example, you could call a function to provide the input for another function, such as `COS(SQRT(3.0))` since both `COS` and `SQRT` are functions.

A code "chore" that is often performed is the process of _refactoring_. That means taking your code and changing it and cleaning it up to still do the same actions but in a neater way. Often, refactoring also comes into play with _modularizing_ code or turning parts of it into smaller and reusable subprograms. In the case of the code we have been working on so far, there are a few different actions that lend themselves to being subprograms: converting from degrees to radians, converting from knots to miles per hour, breaking down a wind magnitude and direction into the zonal and meridional components.

We are going to practice writing one right now--converting a wind value in knots to a wind value in miles per hour. We can actually write our subroutine in the same file, too! That does require us to place it after our main program in the code. The structure of a subroutine is similar to a program, with the surrounding subroutine declarations, variable and type definitions, and otherwise consistent syntax.

~~~ f90
PROGRAM calculator
! by Bucky Badger
! This program calculates meteorological variables.
IMPLICIT NONE

! These are our original variables
REAL :: spd, dir, rh

! These are our computed variables
REAL :: uwind, vwind

! This is our constant
REAL, PARAMETER :: pi=3.14159

! This is our loop variable
INTEGER :: i

PRINT *, "Program is now starting."

OPEN(UNIT=10, FILE='data/obs_crop.txt', ACTION='read', STATUS='old')
READ(10, *)  ! Skip the first line--column headers

DO i = 1, 8
  READ(10, '(24X,F8.2,F8.0,F8.0)') rh, dir, spd
!  spd = spd * 1.151        ! this is mph
  CALL ktstomph(spd)       ! this is mph

  dir = dir - 180.0        ! direction wind is going in degrees
  dir = dir * (pi / 180.0) ! direction wind is going in radians

  uwind = spd * SIN(dir)
  vwind = spd * COS(dir)

  PRINT *, i, uwind, vwind, rh
END DO

CLOSE(UNIT=10)

PRINT '(T20,A)', "Program has finished."
PRINT *, "Go Bucky!"

END PROGRAM calculator

SUBROUTINE ktstomph(speed)
! by Bucky Badger
! This subroutine converts a speed in knots to miles per hour.
IMPLICIT NONE

! This is our input and our output variable.
REAL :: spd

spd = spd * 1.151

END SUBROUTINE ktstomph
~~~

Note a few things in this subroutine. First, we are directly modifying the input variable. So when we supply a variable and call the subroutine, that original variable is being modified based on whatever we do in the subroutine. Second, note that the variable definition in the subroutine is in the context of the subroutine--not the original variable. If we were to supply a single value from an array, the subroutine definition should only be for a single value. Also notice that we replaced our old conversion in the code with a call to the subroutine that includes our speed variable.

You could alternatively define the subroutine with a separate variable for each of the input and the output. The subroutine would then need to be called with two variables in the parentheses: one to give the input and one to hold the output. In that case, we could either 1) have a separate variable with the speed conversion output or 2) supply only one speed variable for both variables as follows:

~~~ f90
PROGRAM calculator
! by Bucky Badger
! This program calculates meteorological variables.
IMPLICIT NONE

! These are our original variables
REAL :: spd, dir, rh

! These are our computed variables
REAL :: uwind, vwind

! This is our constant
REAL, PARAMETER :: pi=3.14159

! This is our loop variable
INTEGER :: i

PRINT *, "Program is now starting."

OPEN(UNIT=10, FILE='data/obs_crop.txt', ACTION='read', STATUS='old')
READ(10, *)  ! Skip the first line--column headers

DO i = 1, 8
  READ(10, '(24X,F8.2,F8.0,F8.0)') rh, dir, spd
!  spd = spd * 1.151        ! this is mph
  CALL ktstomph(spd, spd)       ! this is mph

  dir = dir - 180.0        ! direction wind is going in degrees
  dir = dir * (pi / 180.0) ! direction wind is going in radians

  uwind = spd * SIN(dir)
  vwind = spd * COS(dir)

  PRINT *, i, uwind, vwind, rh
END DO

CLOSE(UNIT=10)

PRINT '(T20,A)', "Program has finished."
PRINT *, "Go Bucky!"

END PROGRAM calculator

SUBROUTINE ktstomph(inspeed, outspeed)
! by Bucky Badger
! This subroutine converts a speed in knots to miles per hour.
IMPLICIT NONE

! This is our input and our output variable.
REAL :: inspeed, outspeed

outspeed = inspeed * 1.151

END SUBROUTINE ktstomph
~~~

# Lab Assignment

[Follow this link to our assignment](/assignments/02-fortran-intro.html).
