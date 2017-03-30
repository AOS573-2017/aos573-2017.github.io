---
layout: assignment
published: false
title: Path finding
due: 
---

# Assignment
Checking if files exist in python

## Pathlib
~~~ python
from pathlib import Path

example_file = Path('example.py')
example_directory = Path('./test/')
if example_file.is_file():
	print 'File exists'
	
if example_directory.is_dir():
	print 'Directory exists'
	
if example_directory.exists():
	print 'Path exists'
~~~

Try this with a variety of paths including different directories, different filenames, and with paths that exist or don't exist.

To see more functions of the pathlib module, visit https://docs.python.org/3/library/pathlib.html.

## OS

Now implement using the os module code in your script and test for a variety of paths.
Use all 21st century resources to figure out how to check if a file exists, a directory exists, and any given path exists. 

~~~ python
import os.path

example_file = 'example.py'
example_directory = './test/'

~~~


