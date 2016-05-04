## Convert svg into eps
%.eps : %.svg
	inkscape $^ -E $@ --export-ignore-filters

%.dvi : %.tex
	latex $^
	latex $^

%.pdf : %.dvi
	dvipdf $^

all: presentation

presentation: converted_images intro_to_games.pdf

converted_images: images/blank_board.eps images/ply_1_3.eps  images/ply_1_7.eps  images/ply_2_2.eps  images/ply_2_6.eps images/graph_completion.eps  images/one_ply.eps images/ply_1_4.eps images/ply_1_8.eps images/ply_2_3.eps images/ply_2_7.eps images/graph_ex.eps images/ply_1_1.eps images/ply_1_5.eps images/ply_1_9.eps images/ply_2_4.eps images/ply_2_8.eps images/graph_setup.eps images/ply_1_2.eps images/ply_1_6.eps images/ply_2_1.eps images/ply_2_5.eps images/two_ply.eps

clean:
	-rm *.dvi
	-rm *.log
	-rm *.pdf
	-rm *.aux
	-rm *.bm
	-rm *.toc

.PHONY: clean all
