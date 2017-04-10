---
layout: lecture
published: false
title: Tools of the Trade
---

To start today's exercise, download/clone the python-week-two from the AOS 573 github.
~~~ bash
git clone https://github.com/AOS573/python-week-two.git
~~~

## Downloading Modules from Source


To start building a module from source, you must download the source code from the module's website.  This can be a website like GitHub, where you can clone the repository in order to download the source code.  

If the source file is a .tar.gz zipped file, unzip the file using command line (in linux) by using the tar -xvzf command.

~~~ bash
 tar -xvzf module_source.tar.gz
 ~~~
 
Once unzipped enter the first level of the directory of the source code.

~~~ bash
cd module_source
~~~

Unless otherwise specified in the modules documentation, the command python setup.py build is the standard command to install any modules into your python library.  

~~~ bash
python setup.py build
~~~

setup.py will be a script in the first level of the source code that installs the module.  After installing any modules, it is always useful to run a quick check in python to verify the installation was successful.

~~~ python
import example_module
~~~

If the module wasn't successfully installed, python will throw an ImportError at the import example_module line.  If python throws any other error related to the module, double check the module doesn't have any dependencies you may not have installed.  If you have all dependencies installed or the module claims to have no dependencies, double check you didn't accidentally install a 'bleeding edge' version which may have bugs.


## Opening a Text File in Python

Opening text files in Python requires no extra module.  Simply open the file in with the syntax ``filename, 'r'`` in the open statement.

~~~ python
with open('example.txt', 'r') as f:
	#Inside of this indent the file is open
	#To open one line use f.readline()
	f.readline()
	#To loop through the rest of the lines, use f.readlines()
	for line in f.readlines():
		print line
~~~

Try doing this with the ``example.txt`` file in the python-week-two repo.  What does it print?

To write a text file, you can use the same open statement with ``filename, 'w'`` instead of 'r'.  

~~~ python
with open('write_on_own.txt', 'w') as d:
	d.write('Hello world')
	
~~~

A quick way to format a string is to use the ``'{}'.format() `` function on a string.

~~~ python
example_string = '{}, {}'.format('Hello', 'world')
print example_string
Hello, world
~~~

For mor information on strings and formating, see the documentation here: https://docs.python.org/2/library/string.html.  Text files should never be your first choice in storing data.  Towards the end of class, we will go over JSON files. 

## Opening netCDF Files in Python

If you were unable to use pip, or chose to skip the pip exercise last week, you should now go and install netCDF before trying to use the module.
~~~ bash
pip install netCDF4
~~~

Many atmospheric datasets come in the netCDF format.  Opening, editing, and creating netCDF files in Python is extremely easy.  To start, we will open a simple netCDF dataset that contains the latitude, longitude, and sensible heat.  The filename of our example netCDF file is ``example.nc``.  NetCDF files end in ``.nc``.

Inside of the repo there is a file named ``example.nc``.  To open a netCDF file in Python, you must import Dataset from the netCDF4 module.  

~~~ python
from netCDF4 import Dataset
~~~

You will use the Dataset function to open and interact with a netCDF dataset.  This same command is used when editing or creating netCDFs.  To only read the dataset, the mode in the command call is 'r'.  Be careful to always have the mode as 'r' if you only intend to read in information from the dataset.  Setting the mode as 'a' or 'w' will allow you to write or append to the dataset.

~~~ python
example_dataset = Dataset('example.nc', mode = 'r')
~~~

Interacting with the netCDF dataset is similar to interacting with dictionary objects in Python.  The dataset contains a list of variables, or keys as they would be in a dictionary.  Querying the dataset with a variable returns information on the variable including the units, the range of values expected, any fill value, and the shape of the varaible's data.  A common mistake when opening netCDF's is to not include ``[:]`` at the end when trying to gather data.  To learn information about a dataset, use ``example_dataset['variable name']``, to gather the data use ``example_dataset['variable name'][:]``.  

~~~ python
#This will print off a list of variables the dataset contains
for var in example_dataset.variables:
	print var

#This prints information on the dataset longitude array
print example_dataset['lon']

#This gathers the dataset longitude array
longitdues = example_dataset['lon'][:]
~~~

## Exercise

Now that you can read in a netCDF file, learn how you can easily interpolate between the vast atmospheric datasets available for your use.

## Storing data in Python

A very simple and easy to use module for storing data is the JSON module.  JSON allows you to make a txt object that hols the data in the original format.  JSON is extremely similar to the Pickle module built into Python, however some languages can read in Python JSON objects and vice-versa.  Creating and reading in JSON files is simple.

To create a JSON file:
~~~ python
import json
with open('dump_file.txt', 'w') as d:
	json.dump(dump_object, d)
~~~

When opening any file in Python, be sure to use the ``with open() as :`` format.  This ensures when you are no longer in the with loop, your file is closed properly.  The 'w' inside of the argument means you are in writing mode.

To read in a JSON file:
~~~ python
import json
with open('dumped_file.txt', 'r') as f:
	loaded = json.load(f)
~~~

Whatever was dumped to the file will be loaded into the loaded variable.

You can dump lists, dictionaries, tuples, etc. into JSON objects.  

A very common error when creating JSON objects is the ValueError.  JSON does not allow ``numpy`` types to be dumped to a JSON object.  When interacting with arrays using numpy, the type changes from a float to np.float32.  To dump a numpy array using JSON, first you must cast every number in the array as a float.

## Exercise

Use your knowledge of JSOn and the two JSON objects in the ``python-week-two`` repository to test your binning skills.

#Dictionaries

Dictionary objects in Python are extremely useful.  A dictionary is a quasi-class like structure that allows you to store variables within a single variable.  Information is saved to a dictionary object using a ``key``.  Querying the dictionary with a key returns the object saved.

~~~ python
this_is_a_dict = {}
this_is_a_dict['example key'] = 'hello world'
print this_is_a_dict['example key']
'hello world'

this_is_a_dict['example list'] = [1., 2., 3., 4.]
print this_is_a_dict['example list']
[1., 2., 3., 4.]
~~~

Reading in datasets is very similar to interacting with dictionary objects and their keys.  When reading in HDFs, .sav, and netCDF, you can think of the datasets as dictionaries within Python, and the variables they contain as their keys.  

## Reading in .sav Files

If you commonly go between IDL and Python, using .sav files may be an easy way to transfer data.  To do this, install the scipy module.

~~~ bash
pip install scipy
~~~

THe scipy.io readsav function allows you to easily read in a .sav file as a dictionary object.  Scipy will insert the .sav file information to a dictionary object in Python.  You can query the dictionary object with the keys to retrieve the data. 

~~~ python
from scipy.io import readsav

dictionary_object = readsav('sav_filename.sav')
~~~

