---
layout: assignment
published: true
title: Project 2
due: 05/10/2017 at Noon Wisconsin Time
---

#Project 2 Guidelines

For project 2, you will take monthly GPCP, OAFlux, and ERA-interim data and map monthly and yearly estimates of the water budget using Precipitation – Evaporation as a crude estimate.  The datasets will be provided.   GPCP and OAFlux are observation based datasets which will be compared against the (reanalysis) ERA-interim dataset.

The datasets are available for download on Learn@UW in the content section under Project Two.  All datasets are in netCDF4 format.  The data exists in 2D arrays of latitude and longitude.  

At a glance:*GPCP (https://www.esrl.noaa.gov/psd/data/gridded/data.gpcp.html) 
	*Monthly	*2.5° x 2.5° spatial resolution	*From 88.75 N to 88.75 S	*From 1.25 E to 358.75 E
	*OAFlux (http://oaflux.whoi.edu/dataproducts.html) 
	*Monthly	*1° x 1° spatial resolution	*From -89.5° S to 89.5° N	*From .5° E to 359.5° E
	*ERA-interim (http://apps.ecmwf.int/datasets/)  	*Daily 	*.75 ° x .75°	*From .5° E to 359.5° E 	*From -89.5° S to 89.5° N	*Includes snow information for the curious
	
You will be expected to grid the data to the same spatial resolution, compute P-E for the observational data and reanalysis data, and map P-E onto a global map using filled or line contours.  Your script must be automated to choose what months to plot or to plot the 2016 yearly average.  

