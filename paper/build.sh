#!/bin/bash

set -e

pdflatex writeup
bibtex writeup
pdflatex writeup
pdflatex writeup
