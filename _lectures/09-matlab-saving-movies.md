---
layout: lecture
published: true
title: Saving and Movies
---

~~~ bash
$ git clone https://github.com/AOS573-2017/matlab_week9.git

$ cd matlab_week9

$ matlab2015a &
~~~

This week, we will be focusing on how to save figures inside your script, how to make movies, and finally, if we have time, making 3-dimensional figures. The application of saving figures inside your script is fairly obvious, if you need to make a large number of figures, it is much more efficient to simply save them in your script rather than save and edit them by hand. Creating movies is obviously a fairly useful thing, not super applicable in terms of systematic analysis, but very useful for visualization and making presentations. 
# Saving figures

There are numerous ways to save figures within a matlab script, if you google, the most common way would be the use of the `print` command, however, I'm going to recommend a different way that I think is more elegant and useful.

~~~ matlab
clear all
close all

x = 0:0.1:4; % some random range

y = sin(x);


figure(1)
plot(x,y,'linewidth',2)
title('Sin Wave')

% now the commands for saving
fpat = '~\matlab_week9\'; % path
fnam=(['sine_fig1.eps']); % file name, I prefer the .eps figure type, it's a postscript like pdf, but better, brackets will be shown to be useful in one second, and don't hurt anything here

set(gcf,'paperunits','inches')
xwidth = 8; % width of figure
ywidth = 6; % height of figure

set(gcf,'paperposition',[0 0 xwidth ywidth])

saveas(gcf,[fpat,filesep,fnam],'epsc') % epsc for color, eps will only give a black and white figure
~~~

As we can see, there are only three important commands needed to save a figure, defining the path, definine the figure name, and finally giving the saveas command. The rest of the commands just allowed us to control how large and what shape we want the figure to be. The main use for the saveas command was to handle lots of figures, let's see how to do that.

~~~ matlab
clear all
close all

x = 0:0.1:4; % some random range
fpat = '~\matlab_week9\'; % path

for i = 1:5
  fnum = num2str(i)

  y = i * sin(x);
  
  figure(1)
  plot(x,y,'linewidth',2)
  title('Sin Wave')
  
  fnam = (['sine_fig' fnum '.eps']); % need to plug in string variables in brackets

  
  saveas(gcf,[fpat,filesep,fnam],'epsc')
  
end


~~~

Movies are also fun to make, and are fairly similar to making a loop of figures, except that we tell matlab not to save individual figure files, but rather capture their images make movies. One important thing in matlab is that movies are made via screen-capture, unlike regular images, which can be made in terminal, without the GUI if one would like.

~~~ matlab
fig1 = figure(1); % a slightly different way to define a figure

winsize = get(fig1,'Position'); % says where we are going to capture
winsize(1:2) = [0 0]; % says what we are going to capture (all the figure)

fps=2; % number of images per second

outfile='~/matlab_week9/new_movie'; % filename for .avi output?

mov = avifile(outfile,'fps',fps,'quality',10); % says that we are making a .avi file

set(fig1,'NextPlot','replacechildren'); % says that we will be continuously replacing figure 1.

pip = 0;
for i = 0:60
    pip = pip +pi/16;
    x = 0:(pi/32):pip;
    y = sin(x)
    plot(x,y)

    F=getframe(fig1,winsize);
    M(t)=getframe(fig1,winsize);
    mov=addframe(mov,F);
end

mov = close(mov);
~~~

# Lab Assignment
[Follow this link to our assignment](/assignments/09-matlab4.html).
