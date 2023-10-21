#lang racket/base

;; hover-util.rkt -- utilities for generating hover information for plots
;;
;; This file is part of plot-container -- canvas to hold plot snips
;; Copyright (c) 2019, 2020, 2023 Alex Harsányi <AlexHarsanyi@gmail.com>
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
;; License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require racket/contract
         racket/class
         racket/gui/base
         racket/draw
         racket/snip
         racket/match
         racket/math
         pict
         plot/no-gui
         pict/snip
         plot/utils
         plot
         "main.rkt")

;; Resources for drawing overlays on the plots.  Defined in one place to
;; ensure consistency across all the plots.

(define hover-tag-background (make-object color% #xff #xf8 #xdc 0.95))
(define hover-tag-item-color (make-object color% #x2f #x4f #x4f))
(define hover-tag-label-color (make-object color% #x77 #x88 #x99))
(define hover-tag-item-font (send the-font-list find-or-create-font 12 'default 'normal 'normal))
(define hover-tag-label-font (send the-font-list find-or-create-font 10 'default 'normal 'normal))
(define hover-tag-item-face (cons hover-tag-item-color hover-tag-item-font))
(define hover-tag-label-face (cons hover-tag-label-color hover-tag-label-font))

;; Can we add overlays to plot-snip% instances? This functionality is only
;; present in a development branch of the plot package, if that package is not
;; installed, the method and the overlay functionality will not be available.
(define have-plot-overlays? 'unknown)

;; Test if we can use plot overlays or not, the check is done only once and
;; the cached value is returned from than on.  If we cannot use overlays, log
;; a message as well.
(define (can-use-plot-overlays? plot-snip)
  (when (eq? have-plot-overlays? 'unknown)
    (set! have-plot-overlays?
          (object-method-arity-includes? plot-snip 'set-mouse-event-callback 1)))
  have-plot-overlays?)

;; Add CALLBACK as a mouse hover callback to PLOT-SNIP.  The plot snip is
;; checked to see if it actually has that method (since this is only present
;; in a development branch of the plot package).
(define (set-mouse-event-callback plot-snip callback)
  (when (can-use-plot-overlays? plot-snip)
    (send plot-snip set-mouse-event-callback callback)))

(define (set-overlay-renderers plot-snip renderer-tree)
  (when (can-use-plot-overlays? plot-snip)
    (send plot-snip set-overlay-renderers
          (if (null? renderer-tree) #f renderer-tree))))

;; Create a vertical rule renderer at position X to be used as an overlay.
;; This is the renderer used for all VRULES in our plots, ensuring
;; consistency.
(define (hover-vrule x)
  (vrule x #:width 1 #:style 'short-dash #:color "black"))

;; `point-pict` is introduced recently (post Racket 6.12) in the plot package,
;; might not be available.  This magic incantation avoids a compilation error
;; -- this code would not be called anyway, as we check for
;; `set-mouse-event-callback` and none of these functions are called unless
;; that one is present.
(define point-pict-1 (dynamic-require 'plot 'point-pict (λ () #f)))

;; Create a renderer that draws label, which can be either a string, a pict or
;; a list of them, to be used as an overlay.  The label is drawn at position
;; X, Y in plot coordinates.
;;
;; NOTES: any #f values in LABELS are discarded (this makes the use of
;; `hover-label` more convenient).  If multiple labels are provided they are
;; stacked vertically using `vl-append`.
(define (hover-label x y #:anchor [anchor 'auto] . labels)
  (when point-pict-1
    (if (and (= (length labels) 1) (pict? (car labels)))
        ;; Special case: a single pict passed in is displayed as is...
        (point-pict-1 (vector x y) (car labels) #:point-sym 'none #:anchor 'auto)
        ;; Otherwise create new picts and pack them into a final pict
        (let* ((p0 (for/list ((label (in-list labels)) #:when label)
                     (if (pict? label)
                         label
                         (text label hover-tag-item-font))))
               (p1 (if (= (length p0) 1)
                       (car p0)
                       (apply vl-append 3 p0)))
               (p2 (cc-superimpose
                    (filled-rounded-rectangle (+ (pict-width p1) 15)
                                              (+ (pict-height p1) 15) -0.1
                                              #:draw-border? #f
                                              #:color hover-tag-background)
                    p1)))
          (point-pict-1 (vector x y) p2 #:point-sym 'none #:anchor anchor)))))

;; Create a vertical rectangle overlay renderer between XMIN and XMAX using
;; COLOR.  The rectangle will cover the entire height of the plot between XMIN
;; and XMAX. This can be used as an overlay to highlight a region, so COLOR
;; should have an alpha channel to ensure it is transparent.
(define (hover-vrange xmin xmax color)
  (rectangles
   (list (vector (ivl xmin xmax) (ivl -inf.0 +inf.0)))
   #:line-style 'transparent
   #:alpha (send color alpha)
   #:color color))

;; Create a renderer that draws the MARKERS, which are a list of 2d positions.
;; These can be used as overlays.
(define (hover-markers markers)
  (points markers #:sym 'circle #:size 10 #:color "red" #:line-width 3))

;; Return a pict object representing a badge for displaying information on a
;; plot.  The ITEMS is a list of key-value string pairs and these are arranged
;; in a table format.
;;
;; As a special case, key can be #f, in which case only the value is rendered
;; using the "key" font and color.  This can be used to display additional
;; information about a value which will be shown underneath the value.
;;
;; NOTE: the returned pict object can be placed on a plot using
;; `add-pict-overlay`.
(define (make-hover-badge items)
  (define column-count
    (for/fold ((column-count 0)) ((item (in-list items)))
      (max column-count (length item))))
  (define picts '())
  (for ((item (in-list items)))
    (let* ((key (car item))
           (vals (reverse (cdr item)))
           (face (if key hover-tag-item-face hover-tag-label-face)))
      (for ((dummy (in-range (- column-count (add1 (length vals))))))
        (set! picts (cons (text "" face) picts)))
      (for ((val (in-list vals)))
        (set! picts (cons (text val face) picts)))
      (set! picts (cons (text (or key "") hover-tag-label-face) picts))))
  (let ((p0 (table column-count picts lc-superimpose cc-superimpose 15 3)))
    (cc-superimpose
     (filled-rounded-rectangle
      (+ (pict-width p0) 20) (+ (pict-height p0) 20) -0.05
      #:draw-border? #t
      #:border-width 0.75
      #:color hover-tag-background)
     p0)))

;; return the location of SNIP as a (cons X Y), or return #f if SNIP is not
;; shown inside an editor.
(define (get-snip-extent snip)
  (let* ((left (box 0))
         (right (box 0))
         (top (box 0))
         (bottom (box 0))
         (a (send snip get-admin))
         (e (if a (send a get-editor) #f)))
    (when e
      (send e get-snip-location snip left top #f)
      (send e get-snip-location snip right bottom #t))
    (values (unbox left) (unbox top)
            (- (unbox right) (unbox left))
            (- (unbox bottom) (unbox top)))))

(define (get-snip-location snip)
  (and snip
       (let-values ([(x y w h) (get-snip-extent snip)])
         (cons x y))))

;; Move SNIP to LOCATION, adjusting it as necessary to remain fully visible
;; inside the canvas.  Assumes the SNIP is added to an editor.
(define (move-snip-to snip location)
  (match-let (((cons x y) (or location (cons 50 50))))
    (define admin (send snip get-admin))
    (when admin
      (define editor (send admin get-editor))
      (when editor
        (define canvas (send editor get-canvas))
        ;; Adjust the coordinates X Y such that the snip is placed inside the
        ;; canvas.
        (let-values ([(width height) (send canvas get-size)]
                     [(_x _y snip-width snip-height) (get-snip-extent snip)])
          (let ((adjusted-x (max 0 (min x (- width snip-width 20))))
                (adjusted-y (max 0 (min y (- height snip-height 20)))))
            (send editor move-to snip adjusted-x adjusted-y)))))))

;; Convert the X position received by the hover callback in a histogram plot
;; back to the series and the slot withing that series.  SKIP and GAP are the
;; #:skip and #:gap arguments passed to the histogram renderer, they default
;; to DISCRETE-HISTOGRAM-GAP and DISCRETE-HISTOGRAM-SKIP parameters with
;; values of 1 and 1/8 respectively.
(define (xposition->histogram-slot xposition
                                   (skip (discrete-histogram-skip))
                                   (gap (discrete-histogram-gap)))
  (let* ((slot (exact-floor (/ xposition skip)))
         (offset (- xposition (* skip slot)))
         (series (exact-floor offset))
         (on-bar? (< (/ gap 2) (- offset series) (- 1 (/ gap 2)))))
    (if on-bar?
        (values series slot)
        (values #f #f))))

;; Return #t when the X, Y and EVENT passed on to a plot mouse callback are
;; valid to display hover information.  They are valid when X and Y are not #f
;; (they are #f when they are inside the plot snip but not on the plot itself,
;; for example in the axes area).  The mouse event must also be a motion event
;; and the SNIP must be directly under the mouse with no other snips above it.
(define (good-hover? snip x y event)

  ;; Return the editor which owns SNIP
  (define (get-editor snip)
    (let ([admin (send snip get-admin)])
      (and admin (send admin get-editor))))

  ;; Find the snip which is at the EVENT location in EDITOR (this is the
  ;; topmost snip in the editor)
  (define (find-snip editor event)
    (let ((ex (box (send event get-x)))
          (ey (box (send event get-y))))
      (send editor global-to-local ex ey)
      (define snip (send editor find-snip (unbox ex) (unbox ey)))
      snip))

  ;; Return true if SNIP is the same snip as the one under the location of
  ;; EVENT inside the editor.
  (define (same-snip snip event)
    (define other
      (let ([editor (get-editor snip)])
        (and editor (find-snip editor event))))
    ;; NOTE: the equal? call will raise an exception when comparing a plot
    ;; snip to a pict snip.  This is because the contract for the equal<%>
    ;; interface for image-snip% expects another image-snip% as an
    ;; argument. See also: https://github.com/racket/gui/issues/119
    (with-handlers
      (((lambda (e) #t) (lambda (e) #f)))
      (equal? snip other)))

  (and (real? x) (real? y)
       (is-a? event mouse-event%)
       (eq? (send event get-event-type) 'motion)
       (same-snip snip event)))

(define (plot-to-canvas renderer-tree canvas
                        #:x-min [x-min #f] #:x-max [x-max #f]
                        #:y-min [y-min #f] #:y-max [y-max #f]
                        #:width [width (plot-width)]
                        #:height [height (plot-height)]
                        #:title [title (plot-title)]
                        #:x-label [x-label (plot-x-label)]
                        #:y-label [y-label (plot-y-label)]
                        #:legend-anchor [legend-anchor (plot-legend-anchor)])
  ;; Calculate the initial size of the plot such that it fills up the entire
  ;; canvas area.  The #:width and #:height parameters are only used if this
  ;; would result in a 0 sized plot.
  ;;
  ;; The plot snip will be resized automatically as the canvas resizes, but
  ;; this initial sizing ensures there is no flickering when a snip with a
  ;; different size is inserted into the canvas forcing the canvas to resize
  ;; it immediately.
  (let-values (((w h) (send canvas cell-dimensions 1)))
    (define snip (plot-snip renderer-tree
                            #:x-min x-min #:x-max x-max
                            #:y-min y-min #:y-max y-max
                            #:width (if (> w 0) w width)
                            #:height (if (> h 0) h height)
                            #:title title
                            #:x-label x-label #:y-label y-label
                            #:legend-anchor legend-anchor))
      (send canvas set-snip snip)
      snip))


;;............................................................. provides ....

(provide/contract
 ;; NOTE all these are actually instances of 2d-plot-snip%, but the plot
 ;; library does not export that type.
 (set-mouse-event-callback (-> (is-a?/c snip%) (-> (is-a?/c snip%) (is-a?/c mouse-event%) (or/c #f number?) (or/c #f number?) any/c) any/c))
 (set-overlay-renderers (-> (is-a?/c snip%) (or/c (treeof renderer2d?) #f null) any/c))
 (hover-vrule (-> real? renderer2d?))
 (hover-label (->* (real? real?) (#:anchor anchor/c) #:rest (listof (or/c string? pict? #f)) renderer2d?))
 (hover-vrange (-> real? real? (is-a?/c color%) renderer2d?))
 (hover-markers (-> (listof (vector/c real? real?)) renderer2d?))
 (make-hover-badge (-> (listof (listof (or/c #f string?))) pict?))
 (move-snip-to (-> (is-a?/c snip%) (or/c #f (cons/c number? number?)) any/c))
 (get-snip-location (-> (or/c #f (is-a?/c snip%)) (or/c #f (cons/c number? number?))))
 (xposition->histogram-slot (->* (number?) (number? number?)
                                 (values (or/c #f exact-nonnegative-integer?)
                                         (or/c #f exact-nonnegative-integer?))))
 (plot-to-canvas (->* ((treeof (or/c renderer2d? nonrenderer?)) (is-a?/c plot-container%))
                      (#:x-min (or/c real? #f) #:x-max (or/c real? #f)
                       #:y-min (or/c real? #f) #:y-max (or/c real? #f)
                       #:width (and/c integer? positive?)
                       #:height (and/c integer? positive?)
                       #:title (or/c string? #f)
                       #:x-label (or/c string? #f) #:y-label (or/c string? #f)
                       #:legend-anchor symbol?)
                      any/c))
 (good-hover? (-> (is-a?/c snip%) (or/c real? #f) (or/c real? #f) (is-a?/c mouse-event%)
                  boolean?)))
