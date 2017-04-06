---
layout: lecture
published: true
title: Fortran CLI, Modules, and Libraries
---

# Prelude

Today we will see how you can interact with the user while a Fortran program is running to alter the runtime behavior. In the past, we have seen in the past how to implement subprograms into Fortran, so we also will be expanding on that concept through _modules_.

But first, we need to clone the repository for this week.

~~~ bash
$ git clone https://github.com/aos573/fortran-week-three

$ cd fortran-week-three
~~~

## Command Line Interface

Sometimes, it is necessary to ask for information from the user while the program is running (often called at runtime). It could be a specific input file to read in, a certain parameter to use in a simulation, or some option that should be taken into account. In any case, Fortran provides you with the ability to utilize an interface with the user. And it can be implemented in two different ways.

Go ahead and open up the file `speaker.f90` from our repository:

~~~ bash
$ nano speaker.f90
~~~
~~~ f90
PROGRAM speaker
! PROGRAM speaker
! by Buckingham U. Badger
!
! This program prints something depending on the dialogue number provided.
!
! Created 02/10/17 BUB
IMPLICIT NONE

INTEGER :: dialoguenumber


!  |                                               |  !
! \|/ We need to ask for the dialogue number here \|/ !





! /|\ We need to ask for the dialogue number here /|\ !
!  |                                               |  !


! Here we print out the requested dialogue number.
IF (dialoguenumber .EQ. 1) THEN
    PRINT *, "Winter is coming."
ELSE IF (dialoguenumber .EQ. 2) THEN
    PRINT *, "These are not the droids you are looking for."
ELSE IF (dialoguenumber == 3) THEN
    PRINT *, "That _is_ a tasty burger."
ELSE IF (dialoguenumber == 4) THEN
    PRINT *, "I'm the king of the world."
ELSE IF ((dialoguenumber .EQ. 5) .OR. (dialoguenumber .EQ. 6)) THEN
    PRINT *, "I'm the greatest botanist on this planet."
ELSE IF ((dialoguenumber .EQ. 7) .OR. (dialoguenumber .EQ. 8) .OR. (dialoguenumber .EQ. 9)) THEN
    PRINT *, "So I guess I'm out of the book club."
ELSE IF ((dialoguenumber .EQ. 1) .AND. (dialoguenumber .EQ. 9)) THEN
    ! This will never happen because a single variable cannot equal two numbers
    PRINT *, "A wormhole's not a naturally occurring phenomenon."
ELSE
    ! In the event the user specify a number that is not a part of our dialogue
    PRINT *, "Sorry, but that dialogue--", dialoguenumber, "--does not exist."
END IF

END PROGRAM
~~~

We see a simple program that prints out a movie or television quote depending on the value of the variable `dialoguenumber`. (In addition, this is another example of the `IF...ELSE IF...ELSE` syntax.) Notice the if statements here also show the two different ways you can define comparison operators in Fortran: either with the symbol (`==`, `/=`, `>`, `<`, `>=`, `<=`) or with a dotted letter syntax (`.EQ.`, `.NE.`, `.GT.`, `.LT.`, `.GE.`, `.LE.`). The only thing left for us to do is to somehow define the dialogue number.

### Reading From Command Line

The first way to provide an interface to the user is by prompting the user for the value. In Fortran, something can be read in this manner using the `READ` syntax, but slightly tweaked--instead of a file unit number, an asterisk is provided within the parentheses. We _should_ give the user some indication of what we are asking for, so we can add a simple print statement on the line above the reading to tell the user what we are looking for:

~~~ f90
PROGRAM speaker
! PROGRAM speaker
! by Buckingham U. Badger
!
! This program prints something depending on the dialogue number provided.
!
! Created 02/10/17 BUB
IMPLICIT NONE

INTEGER :: dialoguenumber


!  |                                               |  !
! \|/ We need to ask for the dialogue number here \|/ !


PRINT *, 'Please enter a dialogue number: '
READ(*,*) dialoguenumber


! /|\ We need to ask for the dialogue number here /|\ !
!  |                                               |  !


! Here we print out the requested dialogue number.
IF (dialoguenumber .EQ. 1) THEN
    PRINT *, "Winter is coming."
ELSE IF (dialoguenumber .EQ. 2) THEN
    PRINT *, "These are not the droids you are looking for."
ELSE IF (dialoguenumber == 3) THEN
    PRINT *, "That _is_ a tasty burger."
ELSE IF (dialoguenumber == 4) THEN
    PRINT *, "I'm the king of the world."
ELSE IF ((dialoguenumber .EQ. 5) .OR. (dialoguenumber .EQ. 6)) THEN
    PRINT *, "I'm the greatest botanist on this planet."
ELSE IF ((dialoguenumber .EQ. 7) .OR. (dialoguenumber .EQ. 8) .OR. (dialoguenumber .EQ. 9)) THEN
    PRINT *, "So I guess I'm out of the book club."
ELSE IF ((dialoguenumber .EQ. 1) .AND. (dialoguenumber .EQ. 9)) THEN
    ! This will never happen because a single variable cannot equal two numbers
    PRINT *, "A wormhole's not a naturally occurring phenomenon."
ELSE
    ! In the event the user specify a number that is not a part of our dialogue
    PRINT *, "Sorry, but that dialogue--", dialoguenumber, "--does not exist."
END IF

END PROGRAM
~~~

Now go ahead and compile this and run it.

~~~ bash
$ gfortran speaker.f90 -o speaker
$ ./speaker
 Please enter a dialogue number:

~~~

After the program prints our statement, it advances a line and Fortran is waiting intently for a response. Go ahead and provide a number between one and nine, then press Enter. (If you look at the code, you notice we have defined `dialoguenumber` as an integer.)

~~~ bash
$ gfortran speaker.f90 -o speaker
$ ./speaker
 Please enter a dialogue number:
2
 These are not the droids you are looking for.
~~~

You can run the program successively without compiling and provide a different result--a program only needs to be compiled if the code within the scripts changes. Try a few different numbers (including one outside of the range 1-9).

~~~ bash
$ ./speaker
 Please enter a dialogue number:
3
 That _is_ a tasty burger.
$ ./speaker
 Please enter a dialogue number:
8
 So I guess I'm out of the book club.
$ ./speaker
 Please enter a dialogue number:
5
 I'm the greatest botanist on this planet.
$ ./speaker
 Please enter a dialogue number:
10
 Sorry, but that dialogue--       10 --does not exist.
~~~

### Command Line Arguments

The other way to have user input at run time is to allow the user to provide command line arguments, which are given when the program is actually called from the command line. In the Fortran 90 standard, this feature was not mandatory--and thus the availability of this feature depends on your compiler if you are using older code standards.

Fortunately, because we have been using the GNU compiler, we can take advantage of the [`GETARG` extension](https://gcc.gnu.org/onlinedocs/gfortran/GETARG.html). Unfortunately, this subroutine only accepts character type variables to store the input. So we will need to make a few modifications to our code to outfit our `dialoguenumber` variable as a character as well as turning our if expressions to compare characters.

~~~ f90
PROGRAM speaker
! PROGRAM speaker
! by Buckingham U. Badger
!
! This program prints something depending on the dialogue number provided.
!
! Created 02/10/17 BUB
IMPLICIT NONE

CHARACTER(1) :: dialoguenumber


!  |                                               |  !
! \|/ We need to ask for the dialogue number here \|/ !


CALL GETARG(1, dialoguenumber)


! /|\ We need to ask for the dialogue number here /|\ !
!  |                                               |  !


! Here we print out the requested dialogue number.
IF (dialoguenumber .EQ. "1") THEN
    PRINT *, "Winter is coming."
ELSE IF (dialoguenumber .EQ. "2") THEN
    PRINT *, "These are not the droids you are looking for."
ELSE IF (dialoguenumber == "3") THEN
    PRINT *, "That _is_ a tasty burger."
ELSE IF (dialoguenumber == "4") THEN
    PRINT *, "I'm the king of the world."
ELSE IF ((dialoguenumber .EQ. "5") .OR. (dialoguenumber .EQ. "6")) THEN
    PRINT *, "I'm the greatest botanist on this planet."
ELSE IF ((dialoguenumber .EQ. "7") .OR. (dialoguenumber .EQ. "8") .OR. (dialoguenumber .EQ. "9")) THEN
    PRINT *, "So I guess I'm out of the book club."
ELSE IF ((dialoguenumber .EQ. "1") .AND. (dialoguenumber .EQ. "9")) THEN
    ! This will never happen because a single variable cannot equal two numbers
    PRINT *, "A wormhole's not a naturally occurring phenomenon."
ELSE
    ! In the event the user specify a number that is not a part of our dialogue
    PRINT *, "Sorry, but that dialogue--", dialoguenumber, "--does not exist."
END IF

END PROGRAM
~~~

After we compile, let's try calling it like normal again to see what happens.

~~~ bash
$ gfortran speaker.f90 -o speaker
$ ./speaker
 Sorry, but that dialogue-- --does not exist.
~~~

When there are more variables requested than have been specified on the command line, Fortran will fill those character variables with spaces. In this case, because `dialoguenumber` has a size of 1, a single space is given. To make it easier on the user to know that something is awry, we can use a function to check that the number of arguments provided is the correct number--and if it is not correct, we can tell the user what to do.

~~~ f90
PROGRAM speaker
! PROGRAM speaker
! by Buckingham U. Badger
!
! This program prints something depending on the dialogue number provided.
!
! Created 02/10/17 BUB
IMPLICIT NONE

CHARACTER(1) :: dialoguenumber
INTEGER :: arglen


!  |                                               |  !
! \|/ We need to ask for the dialogue number here \|/ !

! Check number of arguments supplied; quit if incorrect.
arglen = IARGC()
IF (arglen /= 1) THEN
    PRINT *, 'This program expects exactly one command line argument to be specified, but you specified ', arglen, '.'
    PRINT *, 'The program call syntax is: ./programname dialoguenumber.'
    STOP
END IF

CALL GETARG(1, dialoguenumber)


! /|\ We need to ask for the dialogue number here /|\ !
!  |                                               |  !


! Here we print out the requested dialogue number.
IF (dialoguenumber .EQ. "1") THEN
    PRINT *, "Winter is coming."
ELSE IF (dialoguenumber .EQ. "2") THEN
    PRINT *, "These are not the droids you are looking for."
ELSE IF (dialoguenumber == "3") THEN
    PRINT *, "That _is_ a tasty burger."
ELSE IF (dialoguenumber == "4") THEN
    PRINT *, "I'm the king of the world."
ELSE IF ((dialoguenumber .EQ. "5") .OR. (dialoguenumber .EQ. "6")) THEN
    PRINT *, "I'm the greatest botanist on this planet."
ELSE IF ((dialoguenumber .EQ. "7") .OR. (dialoguenumber .EQ. "8") .OR. (dialoguenumber .EQ. "9")) THEN
    PRINT *, "So I guess I'm out of the book club."
ELSE IF ((dialoguenumber .EQ. "1") .AND. (dialoguenumber .EQ. "9")) THEN
    ! This will never happen because a single variable cannot equal two numbers
    PRINT *, "A wormhole's not a naturally occurring phenomenon."
ELSE
    ! In the event the user specify a number that is not a part of our dialogue
    PRINT *, "Sorry, but that dialogue--", dialoguenumber, "--does not exist."
END IF

END PROGRAM
~~~

## Modules

A _module_ in Fortran is essentially a collection of subprograms that share a common theme or purpose--such as performing certain calculations--that have their own set of variables and routines. In addition, they can be useful for defining parameters as variables that should be shared throughout code. Their function can be considered redundant or not completely purposeful, but they help to keep code tidy and separated logically. Let's create a new file that will be a special module called `arithmetic` in `arithmetic.f90`.

~~~ bash
$ nano arithmetic.f90
~~~

The syntax for defining a module is similar to the other program and subprogram definitions:

~~~ f90
MODULE arithmetic
CONTAINS

END MODULE arithmetic
~~~

Notice the one main difference is the `CONTAINS` statement, under which we define any subroutines or functions we want to include in the module. The only thing you can define above this statement is any "global" variable you would like to access from any of the subroutines.

We want to keep things simple right now to focus our understanding on the relationship between modules and programs, so let's add a single subroutine that performs an addition operation.

~~~ f90
MODULE arithmetic
CONTAINS

SUBROUTINE addition(firstnumber, secondnumber, theaddition)
! by Bucky Badger
! Takes two numbers and returns the addition of them.
IMPLICIT NONE

REAL :: firstnumber, secondnumber, theaddition

theaddition = firstnumber + secondnumber

END SUBROUTINE addition

END MODULE arithmetic
~~~

Now we need to define a main program that can use this module and similarly employ this subroutine. Luckily the syntax for using a module is pretty straightforward: `USE modulename`. Go ahead and open up a new file for our program. In here, use the module and prepare three variables for the subroutine.

~~~ bash
$ nano main.f90
~~~

~~~ f90
PROGRAM main
! by Bucky Badger
! This program will show off the use of modules.
USE arithmetic
IMPLICIT NONE

REAL :: add1, add2, add3

END PROGRAM main
~~~

Once the `USE` statement is issued, and the module is correctly compiled with our program (which we will see how to do in a little bit), we have access to all the functions and subroutines from the module just as we would if we had included them directly in our main program file. Let's define values for our two `add` variables and call our addition subroutine.

~~~ f90
PROGRAM main
! by Bucky Badger
! This program will show off the use of modules.
USE arithmetic
IMPLICIT NONE

REAL :: add1, add2, add3

add1 = 3.0
add2 = 4.0

CALL addition(add1, add2, add3)
PRINT *, add3

END PROGRAM main
~~~

With everything included to call our subroutine, we should now compile this program and module and test them out. The process for compiling a program that uses a module has an extra step than how we compile just a program. First, we must compile the module source code but not yet link it to anything:

~~~ bash
$ gfortran -c arithmetic.f90
~~~

This generates an `arithmetic.o` object file. Now, we include that object file first before the main program file when compiling:

~~~ bash
$ gfortran arithmetic.o main.f90 -o main
~~~

The result of this command is a `main` executable file that we will run our program with. In addition, you will notice an `arithmetic.mod` file, which contains the compiled and linked code for our arithmetic module file. Now run the program and see what happens!

~~~ bash
$ ./main
 7.000000
~~~

It works! We have now succesfully created a module containing a subroutine, called that module in a main program, and used a subroutine from the module!

## Compilation Scripts

As the number of compiler calls we make continues to increase, it can be tiring to type all of the commands out. One useful way to mitigate this is to create a script that runs the compilation steps for you--a bash script, specifically. A bash script simply automates commands you would normally type, so to create a script you simply write out the commands you want to run into a file. Here we name the file with a `.sh` suffix to denote it is a shell script.

~~~ bash
$ nano compile_class.sh
~~~

~~~ bash
gfortran -c arithmetic.f90
gfortran arithmetic.o main.f90 -o main
~~~

Now, to run the script, we call the program `bash` and supply the file name as a command line argument:

~~~ bash
$ bash compile_class.sh
~~~

Now, when you want to compile `arithmetic.f90` and `main.f90`, you only have to type that command and run this script. Think of all the keystrokes you can save!

## While Statements

One final type of statement that will be useful for, among other things, our first project is a while block. While statements are similar to do loops in that they will continue to loop through a block of code so long as some expression that we specify remains true. You can imagine a while loop in an example like saying "while the sky is blue, ride your bike." Once the sky is no longer blue (i.e. when it is night), you will stop riding your bike. Let's see how these work in Fortran!

Using our `main.f90` code from before, let's first change the variables in our `addition` call--namely changing the third variable to be the second variable again: `CALL addition(add1, add2, add2)`. What this will do is add `add1` and `add2` then store the result in `add2`. So if we called this subroutine just once, `add2` would then equal 7. If we did it again, it would be 4 + 7 = 10; then again 4 + 10 = 14; and so on. THis is a prime candidate for a while loop demonstration! So, a while loop is added with the condition that `add2` is less than 40:

~~~ f90
PROGRAM main
! by Bucky Badger
! This program will show off the use of modules.
USE arithmetic
IMPLICIT NONE

REAL :: add1, add2, add3

add1 = 3.0
add2 = 4.0

DO WHILE (add2 < 40.0)
    CALL addition(add1, add2, add2)
    PRINT *, add2
END DO

END PROGRAM main
~~~

Now we recompile the program with our compilation shell script and run it again.

~~~ bash
$ bash compile_class.sh
$ ./main
 7.000000
 10.00000
 13.00000
 16.00000
 19.00000
 22.00000
 25.00000
 28.00000
 31.00000
 34.00000
 37.00000
 40.00000
~~~

We see that the while block of code ran many times as indicated by the multiple numbers that have printed. Furthermore, they are increasing as you would expect given that we changed the subroutine call to modify our second variable with the result of the addition. Finally, it is of interest to note that even though our expression checked for `add2` _LESS THAN_ 40.0, we still printed 40. That is because on the final loop, `add2` was initially 37; after the subroutine was called, `add2` was saved as 40 and that number was printed. Then, 40 no longer met the while condition and the while loop exited. Had we said `(add2 <= 400)`, the final number to print would have been 43.

# Lab Assignment

[Follow this link to our assignment](/assignments/04-fortran-cli-modules-lib.html).
