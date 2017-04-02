---
layout: assignment
published: true
title: Matlab statistics together
due: 03/27/17 13:00
---

This assignment will mostly be done together in class, teams will be formed as groups of two (maybe one group of 3) teams with the best names start with a Zach point. Each time you participate you get a Zach point, I like teams that have a greater number of Zach points, though they don't actually affect your homework grade (it may affect your participation grade). However, at the end of class, the team with the fewest Zach points has to take the book "The Hazards of Diving in Polluted Waters" which was given to me as a white elephant gift. If you throw that away, I'll be deeply offended.

# Assignment
1. Load in 'aos573_wk8_data.mat'.

This data contains surface latent heat flux (lhf), sensible heat flux (shf), precipitation (Prec (mm/day)), surface temperature (SST), and 500hPa vertical velocity (W (m/s)). All values have been averaged over 1 hour. The first dimension is time, the second is space, the first 512 values represent an island, while the second 512 values represent an ocean.

2. Separate all variables into island and ocean components.

3. Plot sst vs time for both the island and the ocean in a subplot.

4. Calculate the standard deviation of precipitation over the island and the ocean.

5. Make a scatter plot between precipitation and W for both the island and ocean in a subplot.

6. Find the linear regression for both relationships and find their RMSE's, does the island prec predict W better, or does the ocean prec predict W better?

7. Bin both lhf and shf over the island for the range of SSTs, and plot them.

8. Of lhf, shf, and SST over the island, which one does the best job of predicting W? You guys tell me what you want to do to figure this out, and we will do it. If we don't get to this point in class, you can each work together to do this. Be sure to explain what you did, and what your results were.

Upload your final script and answer to number 8(with both names on it) to our dropbox.
