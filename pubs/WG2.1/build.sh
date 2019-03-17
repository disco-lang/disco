#!/bin/sh

stack build diagrams diagrams-builder && stack exec -- pdflatex --enable-write18 disco.tex
