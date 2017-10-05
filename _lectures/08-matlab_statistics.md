---
layout: lecture
published: true
title: More statistics in Matlab
---

~~~ bash
$ git clone https://github.com/AOS573-2017/matlab_week8.git

$ cd matlab_week8
~~~

This week will be a fairly short lecture, followed by a group assignment, focusing on the application of statistics in Matlab. To highlight some of those applications, we will first load in our practice data and see what it looks like.

~~~ matlab
% this is a script to practice statistical methods, by Zach Hansen

clear all
close all

load aos573_prac.mat
~~~

We can see that there are three variables, T_K, qv, and z. We see that the T_K and qv have dimensions 4320x64, while z is solely 64x1. Thus we can guess that T_K and qv have some horizontal axis which is 4320 datapoints long, and have a vertical axis that corrolates to z. To get a feel for what this data looks like, we should plot a mean vertical profile, as well as a series of T_K and qv at a couple levels, let's use the lowest level, and the level closest to 5000m. Looking at our z data, we can see that 5000m correlates to the 25th index. Let's use the subplot command to make these 6 images into 2 figures.

~~~ matlab
T_prof = mean(T_K,1); % calculates the mean of the first dimension
qv_prof = mean(qv,1); % same but for qv

figure(1)
subplot(1,2,1) % (number of rows, number of columns, figure index)
plot(T_prof,z)
title('mean temperature profile')
xlabel('temperature (K)')
ylabel('height (m)')

subplot(1,2,2)
plot(qv_prof,z)
title('mean water vapor profile')
xlabel('water vapor (g/kg)')
ylabel('height (m)')

%% now we plot two specific levels
x_rng = 1:length(T_K(:,1));
figure(2)
subplot(4,1,1)

plot(x_rng,T_K(:,1))
title('temperature at 25m')
ylabel('temperature (K)')

subplot(4,1,2)

plot(x_rng,qv(:,1))
title('water vapor at 25m')
ylabel('water vapor (g/kg)')

subplot(4,1,3)

plot(x_rng,T_K(:,25))
title('temperature at 5000m')
ylabel('temperature (K)')

subplot(4,1,4)

plot(x_rng,qv(:,25))
title('water vapor at 5000m')
ylabel('water vapor (g/kg)')
~~~

So we can see that the figures in our second subplot are rather invariant, it doesn't add a ton of information in this case, but probably gives evidence for x to be a time axis in equilibrium rather than a spatial axis. Considering that I made this data, I know that it is a time axis.

Let's perform some simple statistics now. Perhaps we want to find out how well our 25m temperature predicts our 25m water vapor. Lets scatter the two against eachother.

~~~ matlab
%% scatter plotting

figure(3)
scatter(T_K(:,1),qv(:,1))
title('25m temperature vs 25m water vapor')
xlabel('25m temperature (K)')
ylabel('25m water vapor (g/kg)')

~~~

So the data is obviously very noisy, but perhaps if we squint hard enough, we can see that higher temperatures tend to be associated with higher water vapor values. Let's bin our data so that our relationship is more smoothe. To bin, we define a range of values associated with our predictor variable (in this case T_K), and average our predictand associated with those values.

~~~ matlab
%% binning
T_bt = T_K(:,1); % saves characters and time
qv_bt = qv(:,1);

range_T = min(T_bt):0.5:max(T_bt); % defines our bins

list = 0;

% begin our binning loop
for i = 2:length(range_T)
    list = list+1;
    
    qv_T(list) = mean(qv(T_bt >= range_T(i-1) & T_bt \< range_T(i))); % the backslash is only there because github doesn't like less than sign
    
end
    
qv_T = [NaN, qv_T];

figure(4)
plot(range_T,qv_T)
title('binned water vapor of a given temperature')
xlabel('temperature (K)')
ylabel('water vapor (g/kg)')
    
~~~


It's clear that the relationship from this data is a little funky, perhaps if we only looked at stuff where T > 299, things would be clearer. Lets plot this, and see if we can grab the regression from it.

~~~ matlab
qv_299 = qv_bt(T_bt > 299);
T_299 = T_bt(T_bt>299);

figure(5)
scatter(T_299,qv_299)
~~~

We can get regression lines by using the basic fitting command under tools in the figure window. Let's use a linear fit and quadratic fit, grab the equations, and then see how well those equations actually work by calculating the RMSE. I can't show the use of the window here, but assume that I have grabbed the equations, and they is given below.

~~~ matlab
% regression and errors

% p1-p3 are parameters of the regression, l or q represents quadratic fit or linear fit.
l_p1 = 0.41517;
l_p2 = -110.24;
x = T_299;
l_y = l_p1 .* x + l_p2;

q_p1 = 0.039633;
q_p2 = -23.385;
q_p3 = 3462.7;

q_y = q_p1*x.^2 + q_p2*x +q_p3;

% now to calculate RMSE
RMSE_l = sqrt(mean((qv_299-l_y).^2));
RMSE_q = sqrt(mean((qv_299-q_y).^2));
~~~

Thus we see that in this case, we get a better linear fit than quadratic fit, though the differences are fairly small.

# Lab Assignment
[Follow this link to our assignment](/assignments/08-matlab8.html).
