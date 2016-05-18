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

converted_images: images/discret_dist_1.eps images/discret_dist_2.eps images/discret_dist_3.eps images/discret_dist_end.eps images/one_bandit.eps images/discret_dist_as_dist.eps

clean:
	-rm *.dvi
	-rm *.log
	-rm *.pdf
	-rm *.aux
	-rm *.bm
	-rm *.toc
	-rm *\~

.PHONY: clean all
