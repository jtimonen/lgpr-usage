#!/bin/sh
ipython nbconvert --to html *.ipynb
mv *.html ../docs
