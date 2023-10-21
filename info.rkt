#lang info
(define collection "plot-container")
(define license 'LGPL-3.0-or-later)
(define pkg-desc "Embed plot snips in GUI applications")
(define version "0.0")
(define pkg-authors '( AlexHarsanyi@gmail.com ))
(define deps '("base"
               "draw-lib"
               "gui-lib"
               "pict-lib"
               "plot-lib"
               "pict-snip-lib"
               "plot-gui-lib"
               "snip-lib"
               "gui-doc"
               "pict-snip-doc"
               "plot-doc"))
(define build-deps '("scribble-lib"
                     "racket-doc"
                     "rackunit-lib"))
(define scribblings '(("plot-container.scrbl" ())))
