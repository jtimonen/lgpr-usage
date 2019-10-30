#!/bin/sh
jupyter nbconvert --to html *.ipynb
mv *.html ../docs
