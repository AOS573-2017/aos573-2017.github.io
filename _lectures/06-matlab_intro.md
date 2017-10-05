---
layout: lecture
published: true
title: Introduction to Matlab
---

~~~ bash
$ git clone https://github.com/AOS573-2017/matlab-week6.git

$ cd matlab-week6
~~~

# Matlab Overview
Matlab was originally written by a CS professor at the University of New Mexico who didn't want to force his students to learn Fortran in order to use numerical algrabraic and statistical tools. Matlab, unlike Fortran is not a compiled programming language; once you've written a script, all you need to do is run it. Matlab also has a very nice GUI to make writing and running code, as well as visualizing data quite comfortable.

Some major differences in terms of coding syntax from Fortran include not having to define your variable types, Matlab does it for you (and usually does a pretty good job!). Also, Matlab is case sensitive regarding any variables you create. Our first step in getting into coding with matlab is becoming framiliar with its user interface.

# The Matlab Interface
In whatever directory you've been doing your other Fortran lectures, you are going to want to open Matlab.

~~~ bash
matlab &
~~~

The `&` simply allows you to do more things on the shell command line once you've opened Matlab. Once you have opened Matlab, you will see that there are a few boxes, and that in general the program kind of looks like a text editor that also has a command line - it is! It has many of the commands of the bash shell as well as its own built in functions as well. Other than the toolbar at the top, you should have three boxes to look at, and they all are titled. One will be your 'command window', one is your 'current folder', and one is your 'workspace'. There may also be a 4th window, depending on which version of Matlab that you are running called 'variables', and it should be empty right now.

Let's make some data to play around with in our interface:

~~~ matlab
our_data = randn(10,10); % semicolon supresses output!

disp(our_data(2,1)) % displays the datapoint

% or...

our_data(2,1) % will give you something similar
~~~

In matlab, to select a specific data point, we can simply specify the coordinates of our variable, and it goes data(row,column). In general, unless your data is absolutely massive, it's often a good idea to look at the data you have in order to get a feel for the range of values you're dealing with and what is reasonable.

# Variable Types
In Matlab, you don't usually need to specify what type each variable has. As in Fortran, we have reals, integers, charcters/strings, and booleans. In Matlab, integers aren't used all that often, and matlab only rarely specifies the variable to be an integer. The most common type specified by matlab are reals and strings. In matlab, you CAN multiply integers and reals in many cases, the resulting output will be the type that uses the least memory. In general, we mostly use `double` or `single` precision variables, double uses twice the memory of singles.


# Writing Scripts in Matlab
In Matlab, the vast majority of the time you will be writing scripts rather than doing stuff from the command line. This is a good idea because a script saves the work you are doing for future use and documentation, as well as simply being more efficient. Matlab scripts have a suffix of `.m`. Let's make a new script. To do this, we can use a text editor like we did for Fortran, but we can also use Matlab's built in text editor. Let's use this one for now, click the new script button in the top left. Before we write anything, we should comment our file's header, and save it.

~~~ matlab
% By Bucky Badger
% This program will teach us some of the basics of matlab
clear all
close all
~~~

It's generally a good idea to clear your variables and close your figures each time before running your code, the `clear all` and `close all` above do just that. If you don't sometimes your previous run of this script can affect your result.

# Comments
Note that in matlab the `%` is used for commenting, just as in Fortran, it is a very good practice to comment major portions of your code. There will be times when you haven't used a certain script for a year or more, and when you come back to it, if it isn't commented, you will hate yourself.

# How to Loop
Looping is slightly different in Matlab compared to Fortran, but the concepts are very much the same:

~~~ matlab
% By Bucky Badger
% This program will teach us some of the basics of matlab
clear all
close all

int = 2; % the interval for which we will loop

var1 = zeros(20,1); % makes an array filled with zeros

for i = 1:int:20 % equivalent to a do loop in fortran

  var1(i) = i+i^2; % replaces some of the zeros with values
  
end
~~~

Lets run our code here. Clicking the run button will both save and run your code, often you will be asked if you want to change your directory because the directory the command window is in is not the same as the directory the script is in. Go back to the command window and let's look at our variables. `var1` has many values that are zeros, and many that have some large numbers. Note that the last value is 0, even though our for loop went from 1 to 20. This is because our interval of 2 started at 1, and added 2 each time. To start at 2 and end at 20, we would have had to make `2:int:20`.

If loops are very similar to Fortran as well, lets have another variable that uses our previous result:

~~~ matlab
% By Bucky Badger
% This program will teach us some of the basics of matlab
clear all
close all

int = 2; % the interval for which we will loop

var1 = zeros(20,1); % makes an array filled with zeros

for i = 1:int:20 % equivalent to a do loop in fortran

  var1(i) = i+i^2; % replaces some of the zeros with values
  
end

var2 = ones(20,1); % makes an array filled with ones

% we want to loop through all of our previous var1 values to check for whether or not they are zero. The easiest way to do this is with a for loop

for n = 1:length(var1) % looping through the length of var1, note that this doesn't work nearly as well for 3D variables.
  
  if var1(n) ~= 0 && var1(n) <= 50 % if var1 doesn't equal zero and is less than or equal to 50
    
    var2(n) = var1(n)*  3;
    
  elseif var1(n) >50
  
    var2(n) = var1(n) * 1.5;
    
  else
  
    var2(n) = 30;
    
  end
end

~~~

As you can see, looping in Matlab is very similar to looping in Fortran, except that the syntax is slightly different.

# Intrinsic Functions
Just as in Fortran, Matlab has intrinsic functions. We used a few in the previous section. In general, Matlab has a huge number of these functions, much more than in Fortran, to the extent that there is an intrinsic function for a large portion of anything you want to do. The best way to find intrinsic functions in Matlab is to search on google for whatever you are looking to do, and end with the word 'Matlab'.

If you know the intrinsic function you want, but aren't quite sure how to use it, or keep getting errors when using it, type `help [function]` in the command line, and you will get the basic documentation for it. You can also search that function on the Mathworks website to get more extensive documentation and examples.

One last note. Matlab allows you to replace an intrinsic function with some other value i.e. `sin = 4` would replace the sin function with a variable value. It is a bad idea to do this, mostly because you may end up wanting to use that function, and will get confused by your result.

Let's use another function, starting from the end of our previous script:

~~~ matlab
var3 = sin( var1 .* var2);
~~~

This calculate the sin of the multiplication between var1 and var2. The `.*` tells us that we are NOT performing matrix multiplication, but are rather multiplying index by index. If the arrays aren't the same size, this will throw an error.

# Basic Plotting
Matlab has a very large number of built in plotting tools that are extremely useful when trying to visualize any sort of data. You can plot from your script, as well as by selecting variables in the data viewer in the main window. Here, we will show off some simple plotting tools that you can use. We will greatly expand more in each following class.

~~~ matlab
figure(1) % will open up a figure window, and allows us multiple windows

scatter(var1,var2) % plots var1 on the x axis and var2 on the y axis

figure(2)

scatter(var1,var3) % plots var1 on the x axis and var3 on the y axis

figure(3)

scatter(var2,var3) % plots var2 on the x axis and var3 on the y axis
title('var2 vs var3')

~~~

Line plots can be made with the `plot` command, but in general, you want either a monotonic x axis, from your variable, or a monotonic y axis from your variable. We can also plot multiple lines at once using the `hold on` command.

~~~ matlab
x = 1:length(var1);

figure(4)
plot(x,var1) % plots the index of var1 vs its value

hold on

plot(x,var2) % same as above except with var2

plot(x,var3) 

~~~
Nice! we see three different lines based on their index values. It would be nice if we could see which line is which though

~~~ matlab
legend('var1','var2','var3')
~~~

# Lab Assignment
[Follow this link to our assignment](/assignments/06-matlab1.html).
