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
remote: Counting objects: , done.
remote: Compressing objects: 100% (), done.
remote: Total (delta 0), reused (delta 0), pack-reused 0
Unpacking objects: 100% (), done.
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

<center><iframe width="700" height="500" src="https://www.youtube.com/embed/_jY-KCC4jHc?start=2" frameborder="0" allowfullscreen></iframe></center>

Paul's mastery of music comes not from knowing how to play a single song on a single instrument, but rather understanding the intricacies of musical structure, theory, and thematics then composing a song and applying it with an instrument that fits the situation well. You probably won't hear him playing electric guitar on a sad acoustic song, nor will he be whistling on an upbeat rock song. Paul writes a song for and chooses the right instrumennt to communicate a message, elicit an emotion, or achieve some goal.

In a similar manner, we can treat the skills that we have acquired in this class as our mastery of solving complex problems with the assistance of computers. Skills like planning out our code conceptually, modularizing into reusable functions, reading and searching for documentation online, and as a whole wrangling and presenting data. Our last project ties together these concepts in a nice and tidy scientific analysis. So, for today, we are going to learn a few additional skills to help in the future.

# Test-Driven Development Cycle

Previously we played around with the idea of pseudo-code--planning and drawing out the structure and flow of an analysis program. Once the pseudo-code is generated and the plan decided, you generally move to plugging that in to your editor. This aspect of the process is rarely error-proof. When you run or compile your code for the first time, you will likely come across errors. These errors appear in things like typos, misspellings, bad keywords, or incorrect function calls. It could be the case, however, that you have a bigger picture error not caught by the compiler or at run time: perhaps you made a wrong assumption, the operator you used is incorrect, or something else is wrong but is not visible on the surface. One way to add a additional check for these sorts of errors is to follow the practice of writing _tests_.

A test in programming is somewhat similar to a test in university. Your code checks the output of some function or computation; if it is as expected, the test passes; if it is unexpected, the test fails. Tests by no means absolutely verify that code is without any bugs, but they provide a mechanical check to see how things are going. As an example, let's say you had a function that multipled two values together. Create a new file and add that function:

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

> Consider the case of two two-dimensional matrices with unknown dimensions. You are asked to take the difference of these two datasets and report the end result. Sketch out in pseudo-code how you would accomplish this task. Once that is done, open `test-exercise.py` and write two tests calling a function `sum_grid` with 1) two 2x2 matrices and 2) two 3x5 matrices. Finally, compose the function `sum_grid`!

# Shell Scripts Revisited

Back in the very first class we got a basic introduction to some command line programs, such as `cd`, `ls`, and `pwd`. Later, in the Fortran section, we created a bash script to compile and run our program. Now we are going to expand our bash repertoire a bit further.

## Pipes and Redirects

## `grep`ing and Finding

## Loops

# Parting Thoughts

Remember, we are musicians of programming. We use the instrument best suited for the situation, tying together the theoretical and practical concepts we have learned. Golden rules that are useful for programming:

* Don't be afraid to search online
* Don't be afraid to ask for help
* Don't be afraid to reuse the wheel
