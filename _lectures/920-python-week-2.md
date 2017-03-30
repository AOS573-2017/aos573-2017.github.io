---
layout: lecture
published: false
title: Tools of the Trade
---

## Downloading Modules from Source

Today we are going to start by downloading pyHDF from source.  PyHDF is a useful module to open HDF files and easily interact with the datasets they contain.  There are other HDF opening modules in python, I prefer pyHDF because it is the most popularly used ones (for HDF4s that is) and easy to troubleshoot if I run into any problems when using it.

To start building a module from source, you must download the source code from the module's website.  This can be a website like GitHub, where you can clone the repository in order to download the source code.  

If the source file is a .tar.gz zipped file, unzip the file using command line (in linux) by using the tar -xvzf command.

~~~ bash
 $ tar -xvzf module_source.tar.gz
 ~~~
 
Once unzipped enter the first level of the directory of the source code.

~~~ bash
 $ cd module_source
~~~

Unless otherwise specified in the modules documentation, the command python setup.py build is the standard command to install any modules into your python library.  

~~~ bash
 $ python setup.py build
~~~

setup.py will be a script in the first level of the source code that installs the module.  After installing any modules, it is always useful to run a quick check in python to verify the installation was successful.

~~~ python
import example_module
~~~

If the module wasn't successfully installed, python will throw an ImportError at the import example_module line.  If python throws any other error related to the module, double check the module doesn't have any dependencies you may not have installed.  If you have all dependencies installed or the module claims to have no dependencies, double check you didn't accidentally install a 'bleeding edge' version which may have bugs.


## Exercise

Install pyHDF from source.  To help you, in addition to all of my notes, use the directions and documentation from the pyHDF's website to successfully build it from source.

