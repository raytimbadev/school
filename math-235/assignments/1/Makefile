.SUFFIXES:
.PHONY:

writeup.pdf: writeup.tex polynomials.pdf
	pdflatex writeup.tex

polynomials.pdf: makeplot.py
	python makeplot.py
	pdfcrop polynomials.pdf

