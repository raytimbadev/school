.PHONY: all

all: writeup.pdf errington.tar.gz

writeup.pdf: writeup.tex
	pdflatex writeup.tex

.PHONY: errington.tar.gz
errington.tar.gz:
	-rm -rvf errington
	mkdir -p errington/{src,aspect,improved}
	cp -v benchmark/{runner,kernel,manyruns}.m errington/src
	cp -v benchmark/{runner,kernel_opt,manyruns}.m errington/improved
	sed -i 's/kernel/kernel_opt/' errington/improved/manyruns.m
	cp readme.md errington/src/README
	cp improved_readme.md errington/improved/README
	cp aspect_readme.md errington/aspect/README
	cp benchmark/profileaspect.m errington/aspect
	tar cvzf errington.tar.gz errington/
