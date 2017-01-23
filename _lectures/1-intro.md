---
layout: lecture
published: false
title: Introduction to Programming, Logic, Shell, and Git
---

# Course Introduction

This course is designed to give you an introduction to multiple programming languages and technical computing methods used in our field of Atmospheric and Oceanic Sciences. For information about specific course content, please see the course syllabus.

Most class sessions will be taught in an interactive lecture format for the first half, with in-class coding demonstrations as we walk through the lesson. The second half of class will usually entail assigned lab exercises to further explore and reinforce the concepts from the interactive lecture. There will also be two projects assigned over the course of the semester that will tie together many of the concepts we learn in class while solving a typical research problem.

# Introduction to Programming

What is the importance of programming? Why should we learn it? Are good software development practices important for a scientist? 

One of the first benefits of computer programming that comes to mind is automation. Lewis Fry Richardson, an English scientist and early pioneer of numerical weather prediction (NWP), wrote a text book on the process of NWP in 1922. Through the course of the book, he laid out the methodology for integrating or computing by hand his weather forecast. Based on the time it took him to solve the equations manually and assuming someone could increase their solving speed with enough practice, he estimated that it would take approximately 64,000 human "computers" to issue a weather forecast based on a 200 km grid resolution (so the whole state of Kentucky would receive a single data point). You can read more about Richardson's hypothetical "forecast factory" [here](https://archive.org/stream/weatherpredictio00richrich#page/219/mode/1up/).

Another benefit of computer programming is speed. When we automate, for example, one of Richardson's computations, it takes a trivial amount of time for the computer to perform. So we gain the ability to perform many calculations in a very short amount of time, allowing us to solve calculations that might otherwise be unsolvable. Modern personal computers can reach speeds of 10<sup>10</sup> operations per second.

Following good software development practices is important even for scientists because bugs or mistakes are inevitable in computer programming. When we draw conclusions based upon some result that was at one point computed or manipulated with code, we should ensure that we have minimized the possibility for a false result due to some computation error.

## Languages

A programming language is one that communicates instructions to a computer or other device. In Atmospheric and Oceanic Sciences, we use programming languages for a wide spectrum of tasks: from data collection and logging, to data exploration and analysis, to modeling and forecasting. In these situations and others, programming allows us to perform some computation or task electronically.

## Variable Types

When using a programming language, variables and expressions are given a *type*, which describes what that information is and what can be done with it. Specific types used can vary depending on the language, but important types for scientific analysis include:

* String: a letter or line of alphanumeric characters.
* Integer: a whole (integer) number, generally used for counting.
* Real: a rational number that has either a fixed or varied set of decimal places.
* Boolean: a logical value--either true or false.
* Array/list/vector/dictionary: some number of elements of a given type.

Languages handle types differently. Some, like Python and Matlab, make an assumption of the type based on how you define a variable (*dynamic typing*). Others, including Fortran, follow *static typing*--meaning you must explicitly define what a variable type is.

> Given the abovementioned types, what type would define the following as?
>
> 1. 5
> 2. 'The quick brown fox jumped over the lazy dog.'
> 3. 3.14159267
> 4. -97.0
> 5. False

In this example, we can see that there can be a bit of ambiguity present when a number type is assumed. The good news is that, generally, for our purposes it is okay if a language has to fall back on defining a number that could be either an integer or real as a real. Times when you need to be particularly cognizant of typing are operators like integer division that could cause unexpected results.

## Programming Structure

There are a few different ways that you can approach how you structure your code. Older languages like Fortran follow *structured programming*, where code is evaluated 

# Shell

The `$` at the start

# Brief Git Introduction

For most of the lab assignments in this class, we will be using a tool called Git to download that week's package of code. This allows us to easily distribute pre-written examples to you and ensure everyone is starting on the same page.

More broadly, Git is a program that assists in version control. Software version control is the process of tracking changes to a piece or group of code. Version control takes an initial snapshot of a file and then layers the changes made on top of the original like a sandwich. Doing so enables you to go back to prior times in the code development and see changes to the code at that time. While there are many different programs that implement version control, we will use Git for our purposes because of its prevalance on modern computing systems.

Git is implemented in a code *repository*, which essentially is just a designated directory with code. Keep in mind that code here is a pretty generic term. We could be talking about computer programs and functions or text files like those used in LaTeX. A good rule of thumb is to not track changes to files that are not human-readable because it is otherwise difficult to show the text differences in the file at different times. So images, HDF files, and other binary data files should not be tracked with Git.

Finally, note that Git is *not* a replacement for computer file backups. Git history is stored in the repository alongside the files you are tracking, so if your hard drive fails you will lose both.

## Git Installation

Git is already installed on our lab computers, but that may not be the case on your personal computer. The version of your Git software is not too important, but aim for 1.8 or higher. Here are very brief installation primers for a few operating systems:

### Windows
[Git for Windows](https://git-for-windows.github.io/) includes the basic Git tools, a special Git bash shell, and a graphical interface. Which of these you use will depend on your Windows code development workflow, but for our purposes ensure you have Git Bash.

### Mac
If you have OS X 10.9 (Mavericks) or higher, you can download the latest version of Git from [here](https://git-scm.com/download/mac).

If you have an OS X below 10.9), you will need to download Git 2.3.5 [here](https://sourceforge.net/projects/git-osx-installer/files/git-2.3.5-intel-universal-snow-leopard.dmg/download).

### Linux
Git is available from most Linux distribution's package manager. Use the appropriate command, like `sudo yum install git` or `sudo apt-get install git`.

## Git Configuration

The first time you start working with Git, you need to define a few parameters that provide metadata for Git to use. Open up a terminal and enter the following commands, substituting the sample information with your own personal information:

~~~ bash
$ git config --global user.name "Buckingham Badger"
$ git config --global user.email "bucky@wisc.edu"
~~~

You can run the command `git config --list` to confirm your credentials have been added.

## Git Workflow for Class

The code repositories for our assignments are hosted on GitHub, which is an online Git host. You can view all the repositories for our course at [http://github.com/aos573](https://github.com/aos573). As mentioned at the outset, each week will have a separate repository that may contain sample code examples, skeleton code (where only a few parts of the code are filled in), sample data, or empty files to use. These will be the starting point from which our lab activities will proceed.

For this first week, the code we will be working with is in the repository [http://github.com/aos573/intro-week](https://github.com/aos573/intro-week). There are two ways to download this code repository to our local computer. The first involves using a single git command that will download the remote repository you specify into a folder on your computer. By default, it will create a directory on your computer with the same name as the specified repository (in this case, `intro-week`) and place all of the repository's files within that directory.

~~~ bash
$ git clone http://github.com/aos573/intro-week
Cloning into 'intro-week'...
remote: Counting objects: 3, done.
remote: Compressing objects: 100% (2/2), done.
remote: Total 3 (delta 0), reused 3 (delta 0), pack-reused 0
Unpacking objects: 100% (3/3), done.
Checking connectivity... done.
~~~

The other way to download the code is to manually visit the GitHub repository link, click the green `Clone or download` link near the top right of the screen, and download a .zip file. Once unzipped on your computer, the repository will now be wherever you unzipped that directory.

# Lab Assignment

[Follow this link to our first assignment](/assignments/1-intro.html).
