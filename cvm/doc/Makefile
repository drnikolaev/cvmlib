all: cvm.pdf

TEX = pdflatex
FILES = $(wildcard *.tex)

cvm.pdf: $(FILES)
	$(TEX) cvm.tex
	$(TEX) cvm.tex

clean:
	rm -f cvm.pdf

