---
layout: lecture
published: false
title: Fortran Modules, CLI, and Timestepping
---

# Prelude

We have seen in the past how to implement subprograms into Fortran, so today we will be expanding on that concept through _modules_. We will also see how you can interact with the user while a Fortran program is running to alter what happens at runtime. Finally, the concept of timestepping and numerical methods will be touched on in preparation for next week's Fortran lesson.

First, we need to clone the repository for this week.

~~~ bash
$ git clone https://github.com/aos573/fortran-week-three

$ cd fortran-week-three
~~~

## Modules

A _module_ in Fortran is essentially a collection of subprograms that share a common theme or purpose--such as reading a file type or performing certain calculations--that have their own variables and routines. 

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

We see a simple program that prints out a movie or television quote depending on the value of the variable `dialoguenumber`. (In addition, this is another example of the `IF...ELSE IF...ELSE` syntax.) The only thing left for us to do is to somehow define the dialogue number.

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

~~ f90
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
~~

