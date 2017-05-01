---
layout: lecture
published: true
title: Releasing You into the Wild
---

# Prelude

Make sure to clone today's repository from Github:

~~~
$ git clone https://github.com/aos573/aos573-final-week.git
Cloning into 'aos573-final-week'...
remote: Counting objects: 10, done.
remote: Compressing objects: 100% (5/5), done.
remote: Total 10 (delta 0), reused 10 (delta 0), pack-reused 0
Unpacking objects: 100% (10/10), done.
$ cd aos573-final-week
~~~

# Recap of Class Concepts

You may recall from the start of the class when we discussed the purpose of this class, why we were going through three languages, and why the class would be split into an interactive lecture and guided homework session. Our learning goals were focused on learning about the methods and techniques of programming while applying them to three different languages. To help elucidate this point, consider the case of [Paul McCartney](https://en.wikipedia.org/wiki/Paul_McCartney), a world-renowned musician, composer, and multi-instrumentalist:

### Acoustic Guitar and Whistle:

<center><iframe width="700" height="500" src="https://www.youtube.com/embed/xFBKXQILeqI?start=103&end=114" frameborder="0" allowfullscreen></iframe></center>

### Keyboard and Vocals:

<center><iframe width="700" height="500" src="https://www.youtube.com/embed/7QlwzmbiKx8?start=145&end=189" frameborder="0" allowfullscreen></iframe></center>

### Drum Kit

<center><iframe width="700" height="500" src="https://www.youtube.com/embed/_4jeMWI6pP8?end=18" frameborder="0" allowfullscreen></iframe></center>

### Mandolin and Vocals:

<center><iframe width="700" height="500" src="https://www.youtube.com/embed/U16IU7HwhDY?start=259" frameborder="0" allowfullscreen></iframe></center>

### Upright Base and Vocals:

<center><iframe width="700" height="500" src="https://www.youtube.com/embed/q-f4JZfNx1Y?start=85&end=106" frameborder="0" allowfullscreen></iframe></center>

### Harmonica...

<center><iframe width="700" height="500" src="https://www.youtube.com/embed/_jY-KCC4jHc?start=12" frameborder="0" allowfullscreen></iframe></center>

Paul's mastery of music comes not from knowing how to play a single song on a single instrument, but rather understanding the intricacies of musical structure, theory, and thematics then composing a song and applying it with an instrument that fits the situation well. You probably won't hear him playing electric guitar on a sad acoustic song, nor will he be whistling on an upbeat rock song. Paul writes a song for and chooses the right instrumennt to communicate a message, elicit an emotion, or achieve some goal.

In a similar manner, we can treat the skills that we have acquired in this class as our mastery of solving complex problems with the assistance of computers. Skills like planning out our code conceptually, modularizing into reusable functions, reading and searching for documentation online, and as a whole wrangling and presenting data. Our last project ties together these concepts in a nice and tidy scientific analysis. So, for today, we are going to learn a few additional skills to help in the future.

# Test-Driven Development Cycle

Previously we played around with the idea of pseudo-code--planning and drawing out the structure and flow of an analysis program. Once the pseudo-code is generated and the plan decided, you generally move to plugging that in to your editor. This aspect of the process is rarely error-proof. When you run or compile your code for the first time, you will likely come across errors. These errors appear in things like typos, misspellings, bad keywords, or incorrect function calls. It could be the case, however, that you have a bigger picture error not caught by the compiler or at run time: perhaps you made a wrong assumption, the operator you used is incorrect, or something else is wrong but is not visible on the surface. One way to add a additional check for these sorts of errors is to follow the practice of writing _tests_.

A test in programming is somewhat similar to a test in university. Your code checks the output of some function or computation; if it is as expected, the test passes; if it is unexpected, the test fails. Tests by no means absolutely verify that code is without any bugs, but they provide a mechanical check to see how things are going. As an example, let's say you had a function that multiplied two values together. Create a new file and add that function:

~~~ bash
$ nano testing_tests.py
~~~
~~~ python
def multiplier(x, y):
    return x * y
~~~

To test this function, we come up with one or two examples, determine the result in our head or by hand, and then see if our function actually does what we want:

~~~ python
def multiplier(x, y):
    return x * y

test1 = multiplier(3, 4)        # Test positive integers
if test1 != 12:
    print 'Test 1 failed; expected 12 but got {0}'.format(test1)

test2 = multiplier(-1, 2)       # Test mix of positive and negative integers
if test2 == -2:
    pass
else:
    raise Exception('Test 2 failed! We were expecting -2 but got %f.' % (test2,))

test3 = multiplier(6.22, -8.1)  # Test positive and negative floats
assert test3 == -50.382
~~~

Here I have provided not only three tests (see the code comment for what each one tested) but also three different ways of checking those tests. The first uses an if statement and prints if something was wrong. The second uses an if-else statement: if it's as expected, the statement passes; if it is not as expected, we throw our own [`Exception`](https://docs.python.org/2/library/exceptions.html). The third uses the built in [assert](https://docs.python.org/2/reference/simple_stmts.html#assert) statement, which will throw an error if what you assert is not true. Run the code and see what happens:

~~~ bash
$ python testing_tests.py
$ 
~~~

Nothing happens! That's good news if you're the program author because it means that the tests you computed by hand match the function, but it is not so good news if you are trying to learn what happens when these tests fail. So, for learning purposes, tweak the multiplier program to add instead of multiply and run the code again.

~~~ python
def multiplier(x, y):
    return x + y

test1 = multiplier(3, 4)        # Test positive integers
if test1 != 12:
    print 'Test 1 failed; expected 12 but got {0}'.format(test1)

test2 = multiplier(-1, 2)       # Test mix of positive and negative integers
if test2 == -2:
    pass
else:
    raise Exception('Test 2 failed! We were expecting -2 but got %f.' % (test2,))

test3 = multiplier(6.22, -8.1)  # Test positive and negative floats
assert test3 == -50.382
~~~

~~~ bash
$ python testing_tests.py
Test 1 failed; expected 12 but got 7.
  File "testing_test.py", line 12, in <module>
    raise Exception('Test 2 failed! We were expecting 2 but got %f.' % (test2,))
Exception: Test 2 failed! We were expecting -2 but got 1.000000.
~~~

We see that our first test failed and so it printed that statement, and our second test also failed so an exception was thrown. As we have seen in the past, when an exception is thrown, execution of the program stops. So to see the behavior of the third test, comment out the second test and run the program again.

~~~ python
def multiplier(x, y):
    return x + y

test1 = multiplier(3, 4)        # Test positive integers
if test1 != 12:
    print 'Test 1 failed; expected 12 but got {0}'.format(test1)

#test2 = multiplier(-1, 2)       # Test mix of positive and negative integers
#if test2 == -2:
#    pass
#else:
#    raise Exception('Test 2 failed! We were expecting -2 but got %f.' % (test2,))

test3 = multiplier(6.22, -8.1)  # Test positive and negative floats
assert test3 == -50.382
~~~

~~~ bash
$ python testing_tests.py
Test 1 failed; expected 12 but got 7
Traceback (most recent call last):
  File "testing_test.py", line 15, in <module>
    assert test3 == -50.382
AssertionError
~~~

Our first error statement printed again and, because we commented out the exception, we now see what happens when an assertion fails. Essentially, assertion can be a tidy way to collapse the whole if-else statement in the second test, but note the assertion is not available in all languages. You will, however, find some sort of `stop`, `halt`, or `exception` statement in nearly any language.

Let's practice focusing development around tests in the context of a simplified exercise.

## Exercise: Taking the Difference of Gridded Values

> Consider the case of two two-dimensional matrices with unknown dimensions. You are asked to take the difference of these two datasets and report the end result. Sketch out in pseudo-code how you would accomplish this task. Once that is done, open `test-exercise.py` and write two tests calling a function `diff_grid` with 1) two 2x2 matrices and 2) two 3x5 matrices. Finally, compose the function `diff_grid`!


# The Shell Revisited

Back in the very first class we got a basic introduction to some command line programs, such as `cd`, `ls`, and `pwd`. Later, in the Fortran section, we created a bash script to compile and run our program. Now we are going to expand our bash repertoire a bit further.

## Pipes and Redirects

Our exposure to the command line shell in Fortran automated processes for us, but the actions we accomplished were separate individual pieces. First we compiled modules. Then we compiled the main program. Then we ran the main program. Each of these commands commands took in or possibly provided some file or executable. In a shell, it is actually possible to stitch together commands through a process known as piping. Additionally, we can redirect the output text of a command from the command line to a file using the redirect command.

Let's try these two features out with our repository for the day as well as some of the commands from the first class. Navigate into the `data/` directory and see all the files in there:

~~~ bash
$ cd data/
$ ls
1910-03-01.dat  1910-03-05.dat  1910-03-09.dat  1910-03-13.dat  1910-03-18.dat  1910-03-22.dat  1910-03-26.dat  1910-03-30.dat
1910-03-02.dat  1910-03-06.dat  1910-03-10.dat  1910-03-15.dat  1910-03-19.dat  1910-03-23.dat  1910-03-27.dat  1910-03-31.dat
1910-03-03.dat  1910-03-07.dat  1910-03-11.dat  1910-03-16.dat  1910-03-20.dat  1910-03-24.dat  1910-03-28.dat  info.txt
1910-03-04.dat  1910-03-08.dat  1910-03-12.dat  1910-03-17.dat  1910-03-21.dat  1910-03-25.dat  1910-03-29.dat
$ cat info.txt
Precipitation reports for Badgerland.
$ cat 1910-03-01.dat
snow
~~~

We see there are two months of precipitation reports for ''badgerland''--wherever that is...

Nevertheless, our boss has asked us to provide him with a listing of and the quantity of data files that exist in this directory. One way to accomplish this would be to manually record the file name and count the number of files, but it turns out we will soon be receiving more directories with thousands of files and that manual method is not scalable to those quantities. This is where our redirection and piping can come in handy!

The first task to tackle is creating a file that lists all the files, each on a single line. We have seen before that `ls` displays a directory listing. When we supply the flag `-1` (that's dash one) to ls, i.e. `ls -1`, the command prints each file in a separate line.

~~~ bash
$ ls -1
1910-03-01.dat
1910-03-02.dat
1910-03-03.dat
1910-03-04.dat
1910-03-05.dat
1910-03-06.dat
1910-03-07.dat
1910-03-08.dat
1910-03-09.dat
1910-03-10.dat
1910-03-11.dat
1910-03-12.dat
1910-03-13.dat
1910-03-15.dat
1910-03-16.dat
1910-03-17.dat
1910-03-18.dat
1910-03-19.dat
1910-03-20.dat
1910-03-21.dat
1910-03-22.dat
1910-03-23.dat
1910-03-24.dat
1910-03-25.dat
1910-03-26.dat
1910-03-27.dat
1910-03-28.dat
1910-03-29.dat
1910-03-30.dat
1910-03-31.dat
info.txt
~~~

Our boss specifically asked about the _data_ files though and not our metadata file. So we restrict our command to only files that end in `.dat`:

~~~ bash
$ ls -1 *.dat
1910-03-01.dat
1910-03-02.dat
1910-03-03.dat
1910-03-04.dat
1910-03-05.dat
1910-03-06.dat
1910-03-07.dat
1910-03-08.dat
1910-03-09.dat
1910-03-10.dat
1910-03-11.dat
1910-03-12.dat
1910-03-13.dat
1910-03-15.dat
1910-03-16.dat
1910-03-17.dat
1910-03-18.dat
1910-03-19.dat
1910-03-20.dat
1910-03-21.dat
1910-03-22.dat
1910-03-23.dat
1910-03-24.dat
1910-03-25.dat
1910-03-26.dat
1910-03-27.dat
1910-03-28.dat
1910-03-29.dat
1910-03-30.dat
1910-03-31.dat
~~~

Halfway there! Now, this looks like a prime opportunity to redirect to a file. Redirect uses the greater than sign `>` and then is followed by the file name to redirect the output to. Do note that if the file already exists, it will be overwritten.

~~~ bash
$ ls -1 *.dat > filenames.txt
$
~~~

Take a look at the `filenames.txt` file and see what's in there.

~~~ bash
$ cat filenames.txt
1910-03-01.dat
1910-03-02.dat
1910-03-03.dat
1910-03-04.dat
1910-03-05.dat
1910-03-06.dat
1910-03-07.dat
1910-03-08.dat
1910-03-09.dat
1910-03-10.dat
1910-03-11.dat
1910-03-12.dat
1910-03-13.dat
1910-03-15.dat
1910-03-16.dat
1910-03-17.dat
1910-03-18.dat
1910-03-19.dat
1910-03-20.dat
1910-03-21.dat
1910-03-22.dat
1910-03-23.dat
1910-03-24.dat
1910-03-25.dat
1910-03-26.dat
1910-03-27.dat
1910-03-28.dat
1910-03-29.dat
1910-03-30.dat
1910-03-31.dat
~~~

As you can see, what _would_ have printed in the terminal (called standard out) was instead printed into the file. This is the beauty of redirection. Alternatively, if you wanted to use redirection to _append_ to a file as opposed to overwriting, you can use two greater than signs `>>` followed by the file name.

We are halfway done. The next step is to get the number of files that are present. Recalling that `ls -1` prints each file name on a separate file, we can leverage the word count program `wc`. As the name suggests, word count is able to count the number of words that are fed into it, but using a flag `-l` (dash the letter l), it can count the number of lines supplied to it.

First let's look at the behavior of `wc -l` by supplying it a file.

~~~ bash
$ wc -l filenames.txt
      30 filenames.txt
~~~

We get back that there are 30 lines in the file. Word count can also read from the command line input (called standard in) when there is no file name given. Try that out by issuing the command, typing three Wisconsin cities (separating each one by the Enter key), then pressing Ctrl+d to issue the ''End of File'' command:

~~~ bash
$ wc -l
Milwaukee
Fond du Lac
Monroe
       3
~~~

Just as expected, we supplied three lines of text and word count returned that there were indeed three lines of text. 

Now to revisit piping. As mentioned before, piping stitches together different commands in Unix. This happens by connecting the standard out from one program to the standard in of another. Piping is implemented by the `|` operator, with the command to the left of it the first command run and also the command whose standard out is connected to the standard in of the command to the right of the pipe operator. Check it out:

~~~ bash
$ ls -1 *.dat | wc -l
     30
~~~

Prett neat! You can actually implement many different pipes together, though that will be left to you to play with outside of class.

The final step is to redirect the output of this piping command to a file that says how many files exist so our boss will be happy:

~~~ bash
$ ls -1 *.dat | wc -l > filenumber.txt
$ cat filenumber.txt
     30
~~~

## `grep`ing

Now our boss wants to know which files have snow in them. Yet another tool that is useful in the command line is one called `grep`, which involves global search and print. grep is one of the more powerful Swiss army knives as far as shells go, so keep in mind that we are only grazing the surface of grep's power. Grep provides the functionality to search for a string in files or standard in and return files that contain that string or inversely files that do not contain that string. The form of the command is `grep "string to search" files`. Try that for snow with our data files:

~~~ bash
$ grep "snow" *.dat
1910-03-01.dat:snow
1910-03-08.dat:snow
1910-03-12.dat:snow
1910-03-13.dat:snow
1910-03-15.dat:snow
1910-03-16.dat:snow
1910-03-19.dat:snow
1910-03-20.dat:snow
1910-03-21.dat:snow
1910-03-22.dat:snow
1910-03-24.dat:snow
1910-03-25.dat:snow
~~~

The files that contain our string are printed as are line context around the match. Because the files are only a single word, this behavior takes the form of printing the string again. We can suppress the strings to retain only a list of files by using the `-l` (dash letter l) flag in calling our command. Do take note that flags must be supplied _before_ the files or standard in.

~~~ bash
$ grep -l "snow" *.dat
1910-03-01.dat
1910-03-08.dat
1910-03-12.dat
1910-03-13.dat
1910-03-15.dat
1910-03-16.dat
1910-03-19.dat
1910-03-20.dat
1910-03-21.dat
1910-03-22.dat
1910-03-24.dat
1910-03-25.dat
~~~

This can be redirected to a file to summarize snowing days.

~~~ bash
$ grep -l "snow" *.dat > snowdays.txt
$ cat snowdays.txt
1910-03-01.dat
1910-03-08.dat
1910-03-12.dat
1910-03-13.dat
1910-03-15.dat
1910-03-16.dat
1910-03-19.dat
1910-03-20.dat
1910-03-21.dat
1910-03-22.dat
1910-03-24.dat
1910-03-25.dat
~~~

Grep can also print the inverse case--files that do _not_ match the supplied string. This is accomplished by supplying the `-v` (dash letter v) flag:

~~~ bash
$ grep -l -v "snow" *.dat
1910-03-02.dat
1910-03-03.dat
1910-03-04.dat
1910-03-05.dat
1910-03-06.dat
1910-03-07.dat
1910-03-09.dat
1910-03-10.dat
1910-03-11.dat
1910-03-17.dat
1910-03-18.dat
1910-03-23.dat
1910-03-26.dat
1910-03-27.dat
1910-03-28.dat
1910-03-29.dat
1910-03-30.dat
1910-03-31.dat
~~~

## Loops

We have seen loops in Fortran, Matlab, and Python, so it is worthwhile to see how they are used in bash as well. For loops take the form of (noting the separate lines):

~~~ bash
for x in something
do
some action here
done
~~~

In this case, `x` is the loop object, `something` is an array of things (strings, filenames, etc.), and `some action here` can be anything. The array to loop over could be a list of files, either manually typed out or alternatively specified by wildcard notation. Then, to reference the loop object within the loop, we use bash notation of `${variablename}`.

~~~ bash
$ for x in 1910-03-01.dat 1910-03-02.dat 1910-03-03.dat
> do
> cat ${x}
> done
snow
rain
clear
~~~

### Exercise: Printing File and its Name
> We can weave in as many commands as we want within the loop. Try writing a for loop that `echo`es the file name to the standard out, `cat`s the file contents to the standard out, and then finally appends the file contents to a file named `observations.txt`.

## Bash Wrap-up

The shell is a powerful tool, especially for analysis when it comes to dealing with human-readable files. There is a deep legacy in the shell system so any problem you need to tackle has likely been accomplished before. As with Unix, there are different variants of shell available; we have only dealt with bash. Some shells provide more flexibility while others offer more utility. Just be aware that not all shells may not have every function you need.

# Parting Thoughts

Remember, we are musicians of programming. We use the instrument best suited for the situation, tying together the theoretical and practical concepts we have learned. Rules that are useful for programming:

* Don't be afraid to search online
* Don't be afraid to ask for help
* Don't be afraid to reuse the wheel
