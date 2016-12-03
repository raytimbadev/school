.PHONY: all writeup.pdf

all: writeup.pdf

writeup.pdf: writeup.tex bibliography.bib
	pdflatex writeup
	-bibtex writeup
	pdflatex writeup
	pdflatex writeup
