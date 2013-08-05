#!/usr/bin/env python

import os.path
from setuptools import setup, find_packages

here = os.path.abspath(os.path.dirname(__file__))
try:
    README = open(os.path.join(here, "README")).read()
except IOError:
    README = ""

CHANGES = ""

setup(
    name="arpee",
    version="0.1.0",
    description="a random python emulation experiment",
    long_description=README + "\n\n" + CHANGES,
    author="Bertrand Janin",
    author_email="b@janin.com",
    url="https://github.com/tamentis/arpee/",
    scripts=["run.py"],
    license="ISC License (ISCL, BSD/MIT compatible)",
    packages=find_packages(),
    classifiers=[
        "Development Status :: 5 - Alpha",
        "Environment :: Console",
        "Intended Audience :: End Users/Desktop",
        "License :: OSI Approved :: ISC License (ISCL)",
        "Operating System :: MacOS :: MacOS X",
        "Operating System :: Microsoft :: Windows",
        "Operating System :: POSIX",
        "Programming Language :: Python :: 2.7",
        "Programming Language :: Python :: 3.3",
    ],
    install_requires=[
    ],
    setup_requires=[
        #"nose>=1.0",
    ],
)
