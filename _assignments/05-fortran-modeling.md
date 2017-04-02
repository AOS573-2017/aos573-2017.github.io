---
layout: assignment
published: true
title: Fortran modeling
due: 02/27/17 13:00
---

1. Add the ability to input initial conditions and timestep length via command line or input file.
2. Try different initial conditions, timestep lengths and number of integration cycles. What happens when you start from slightly different initial conditions? What are some good timestep lengths for generating a good "butterfly"? What happens when the timestep length is too big?
3. Change the integration scheme to Leapfrog. Hint: Leapfrog requires model states at two timesteps to initiate. How do you get the second model state?
