#lang info
(define collection "plot-container")
(define deps '("base" "draw-lib" "gui-lib" "pict-lib" "plot-lib"
                      "pict-snip-lib" "plot-gui-lib" "snip-lib"
                      "gui-doc" "pict-snip-doc" "plot-doc"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/plot-container.scrbl" ())))
(define pkg-desc "Embed plot snips in GUI applications")
(define version "0.0")
(define pkg-authors '(alexh))
