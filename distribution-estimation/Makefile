## Convert svg into eps
%.eps : %.svg
	inkscape $^ -E $@ --export-ignore-filters

%.dvi : %.tex
	latex $^
	latex $^

%.pdf : %.dvi
	dvipdf $^

all: presentation

presentation: converted_images dist_est.pdf

converted_images:

clean:
	-rm *.dvi
	-rm *.log
	-rm *.pdf
	-rm *.aux
	-rm *.bm
	-rm *.toc
	-rm *\~

.PHONY: clean all
