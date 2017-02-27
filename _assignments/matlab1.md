---
layout: assignment
published: true
title: Matlab Plotting and intrinsic functions
due: 03/06/17 13:00
---

Create a new script that will load in the `.mat` file from this week's repository. In this assignment, I'm using matlab's proprietary file type, because they are simply the easiest to load. We will be using other types in the future.

This file contains temperature (K), water vapor (g/kg), pressure (hPa), height(m), and time(hours). There are 64 heights and 48 times.

#Assignment
All of the following should be in one script, which you will turn in to the course dropbox. We will check if your script runs, and does what it is supposed to do. You are highly encouraged to make use of google to find how to do certain things.

1. Plot surface temperature and water vapor versus time of day. Label your x and y axes, and give a title..
2. Plot another level of your choice versus time of day for the previous two variables. Label you axes, and be sure that the title tells us what level we are at.
3. Calculate mean temperature and water vapor profiles, and plot them versus height and pressure. don't forget to label.
4. Calculate the mean relative humidity at each height using your previous values. RH is the ratio between vapor pressure and saturation vapor pressure. Mixing ratio can be related to vapor pressure as e(vap press) = P*(MR)/(MR+(Rd/Rv)). Where MR is mixing ratio in kg/kg, Rd is 287.0, Rv is 462, and P is pressure in pascals. Saturation vapor pressure can be related directly to temperature es = 0.622 * exp((17.67*T(celcius))/(243.5+T(celcius))). Plot your relative humidity vs height as before.
5. Calculate the correlation coefficient of temperature to relative humidity. You will have to search how to do this.
