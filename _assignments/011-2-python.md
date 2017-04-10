---
layout: assignment
published: true
title: Week 2 Homework
due: 04/17/17
---

# Using the folder 'homework_files' in the python-week-two repo, go through all nectCDF files and bin the variable 'testlwp' by 10% limits.  

To go through a list of files in Python, use the glob module (https://docs.python.org/2/library/glob.html)

Alter your code so you can take in user input to change what variable is being binned by 10%. To read in user input from Python, use the build in function ``raw_input``.

~~~ python
user_input = raw_input('What variable would you like to plot?')
# Whatever the user enters in the command line gets assigned to user_input
~~~

For extra credit, create a function that verifies the input vased on the list of variables available from the netCDF files.  Recall, to get a list of variables from a netCDF dataset, use  ``dataset_name.variables``.

 Note: this is satellite data so there will be a fill value of None.  Be sure to filter out any values of None.



