#lang racket/base

;; plot-container.rkt -- a canvas% capable of holding multiple plots.
;;
;; This file is part of plot-container
;; https://github.com/alex-hhh/plot-container
;; Copyright (c) 2019 Alex Harsányi <AlexHarsanyi@gmail.com>
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <http://www.gnu.org/licenses/>.

(require racket/gui/base
         racket/class
         racket/draw
         racket/math
         racket/match
         racket/contract
         pict)

;; Draw the message MSG centered in the device context DC using COLOR and FONT
;; for the rendering.
(define (draw-centered-message dc msg color font)
  (let-values (([cw ch] (send dc get-size))
               ([w h x y] (send dc get-text-extent msg font #t)))
    (send dc set-font font)
    (send dc set-text-foreground color)
    (let ((ox (- (/ cw 2) (/ w 2)))
          (oy (- (/ ch 2) (/ h 2))))
      (send dc draw-text msg ox oy))))

(define message-font
  (send the-font-list find-or-create-font 36 'default 'normal 'normal))
(define message-color "gray")

;; Helper to make the read-only-pasteboard% writable during the execution of
;; THUNK
(define (with-writable pb thunk)
  (dynamic-wind
    (lambda () (send pb set-writable #t))
    thunk
    (lambda () (send pb set-writable #f))))

;; Wrap THUNK into begin-edit-sequence / end-edit-sequence
(define (with-edit-sequence pb thunk)
  (dynamic-wind
    (lambda () (send pb begin-edit-sequence))
    thunk
    (lambda () (send pb end-edit-sequence))))

;; Combine `with-writable` and `with-edit-sequence`
(define (with-writable-edit-sequence pb thunk)
  (dynamic-wind
    (lambda ()
      (send pb set-writable #t)
      (send pb begin-edit-sequence))
    thunk
    (lambda ()
      (send pb end-edit-sequence)
      (send pb set-writable #f))))

;; pasteboard% to hold the plots and other snips, used as a helper class for
;; `plot-container%`.  Implements the following functionality:
;;
;; * a message can be displayed when the pasteboard% is empty, see
;; `set-background-message`
;;
;; * any snip inserted via `insert` will be locked and the user will not be
;; able to select or move it -- to move these snips from a program, use
;; `set-writable` or `with-writable-edit-sequence`, `with-writable`
;;
;; * any snip inserted via `add-floating-snip` or `set-floating-snip` can be
;;   moved by selecting and dragging the snip with the mouse
;;
;; * an extra pict can be added to the pasteboard% and this will be shown on
;; top of all the snips, see `set-hover-pict`
;;
(define plot-container-pasteboard%
  (class pasteboard% (init)

    ;; When #t, non-floating snips can be moved around
    (define writable? #f)
    ;; Message to be shown when there is no main snip in the canvas.
    (define background-message #f)
    ;; A (list pict x y) containing a pict which will be displayed at position
    ;; X, Y on top of all the other snips.  When #f, the pict will not be
    ;; displayed (obviously)
    (define hover #f)
    ;; Snips in this list can be moved around by the user by dragging them
    ;; with the mouse, even when writable? is #f
    (define floating-snips '())

    (define/public (set-writable w?) (set! writable? w?))

    ;; (define/augment (can-change-style? start len) writable?)
    (define/augment (can-delete? snip) writable?)
    (define/augment (can-insert? snip before x y) writable?)
    (define/augment (can-load-file? filename format) writable?)
    (define/augment (can-save-file? filename format) writable?)
    (define/augment (can-move-to? snip x y dragging?)
      (or writable? (memq snip floating-snips)))
    (define/augment (can-select? snip on?)
      (or writable? (memq snip floating-snips)))

    (define/override (can-do-edit-operation? op [recursive? #t])
      (case op
        [(copy select-all) #t]
        [else    writable?]))

    (define/augment (after-insert snip before x y)
      (when (memq snip floating-snips)
        (send this set-before snip #f)))

    (define/public (set-background-message msg)
      (set! background-message msg))

    (define/public (set-hover-pict pict x y)
      (if pict
          (set! hover (list pict x y))
          (set! hover #f)))

    (define/public (add-floating-snip snip x y)
      (with-writable-edit-sequence this
        (lambda ()
          (set! floating-snips (append floating-snips snip))
          (send this insert snip x y))))

    (define/public (set-floating-snip snip x y)
      (with-writable-edit-sequence this
        (lambda ()
          (for ((snip (in-list floating-snips)))
            (send this remove snip))
          (if snip
              (begin
                (set! floating-snips (list snip))
                (send this insert snip x y))
              (set! floating-snips '())))))

    (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
      (if before?
          (begin
            (send dc clear)
            ;; Draw a message when there is no snip in the pasteboard.
            (unless (send this find-first-snip)
              (send dc clear)
              (when background-message
                (draw-centered-message dc background-message "gray" message-font))))
          (when hover
            (match-define (list pict x y) hover)
            (define-values (cw ch) (send dc get-size))
            (let ((o 10))
              (let ((x (if (< (+ x o (pict-width pict)) cw)
                           (+ x o)
                           (- x o (pict-width pict))))
                    (y (if (< (+ y o (pict-height pict)) ch)
                           (+ y o)
                           (- y o (pict-height pict)))))
                (draw-pict pict dc x y))))))

    (super-new)
    (send this set-selection-visible #f)
    (send this set-area-selectable #f)

    ))

;; Calculate layout information for the pasteboard PB for placing a SNIP-COUNT
;; number of snips in COLUMNS with SPACING between them such that all the
;; space in the PB is filled up.  The snips will all be the same size.  The
;; function returns 6 values, being:
;;
;; HMARGIN, VMARGIN -- the origin of the top-left snip
;; COLUMNS, ROWS    -- number of columns and rows for the placement
;; CELL-WIDTH, CELL-HEIGHT - dimensions for an individual snip
;;
(define (layout-info pb snip-count columns (spacing 5))
  (define c (send pb get-canvas))
  (define rows (exact-ceiling (if (> columns 0) (/ snip-count columns) 0)))

  (define-values (w h) (send c get-client-size))
  (define hinset (send c horizontal-inset))
  (define vinset (send c vertical-inset))
  (define hmargin (send c horiz-margin))
  (define vmargin (send c vert-margin))

  (define width (- w hinset hinset hmargin hmargin))
  (define height (- h vinset vinset vmargin vmargin))
  (define cell-width
    (if (> columns 0)
        (exact-floor (/ (- width (* (sub1 columns) spacing)) columns))
        0))
  (define cell-height
    (if (> rows 0)
        (exact-floor (/ (- height (* (sub1 rows) spacing)) rows))
        0))

  (values hmargin vmargin columns rows cell-width cell-height))

;; Place SNIPS in the pasteboard PB in COLUMNS with SPACING between them.  All
;; SNIPS will be the same size and they will fill out the pasteboard.
(define (layout-snips pb snips columns (spacing 5))
  (define-values (x0 y0 c rows cwidth cheight)
    (layout-info pb (length snips) columns spacing))

  (when (and (> cwidth 0) (> cheight 0))
    (with-writable-edit-sequence pb
      (lambda ()
        (for ([(snip index) (in-indexed (in-list snips))])
          (let-values (((row col) (quotient/remainder index columns)))
            (let ((x (+ x0 (* col (+ cwidth spacing))))
                  (y (+ y0 (* row (+ cheight spacing)))))
              (send pb move-to snip x y)))
          (send pb resize snip cwidth cheight))))))

;; Contract for the plot-container% class, since we export it
(define plot-container%/c
  (class/c
   (init [parent (or/c (is-a?/c frame%) (is-a?/c dialog%)
                       (is-a?/c panel%) (is-a?/c pane%))])
   (init-field [columns positive-integer?] [spacing positive-integer?])
   (cell-dimensions (->m positive-integer? (values positive-integer? positive-integer?)))
   (clear-all (->m any/c))
   (add-plot-snip (->m (is-a?/c snip%) any/c))
   (set-plot-snip (->m (is-a?/c snip%) any/c))
   (set-plot-snips (->*m () #:rest (listof (is-a?/c snip%)) any/c))
   (add-floating-snip (->m (is-a?/c snip%) real? real? any/c))
   (set-floating-snip (->m (or/c #f (is-a?/c snip%)) real? real? any/c))
   (set-background-message (->m (or/c #f string?) any/c))
   (set-hover-pict (->m (or/c #f pict?) real? real? any/c))
   (set-hover-pict-at-mouse-event (->m (or/c #f pict?) (is-a?/c mouse-event%) any/c))))

;; An editor-canvas% for holding one or more plot snips with support for
;; additional features: floating snips for implementing plot legends that can
;; be moved around with the mouse, and hover pictures to display extra
;; information about the current position on the plot.
(define plot-container%
  (class editor-canvas%
    (init parent [style null])
    (init-field [columns 1] [spacing 5])

    (define pb (new plot-container-pasteboard%))
    (define plot-snips '())

    (super-new [parent parent]
               [editor pb]
               [style (list* 'no-hscroll 'no-vscroll style)])

    (define/override (on-size w h)
      (super on-size w h)
      (queue-callback (lambda () (layout-snips pb plot-snips columns spacing))))

    ;; Return the width and height for a snip assuming that SNIP-COUNT snips
    ;; will be placed in the container -- this can be used to create the plot
    ;; snips of the right size before they are inserted into the container so
    ;; we avoid an initial resize.
    (define/public (cell-dimensions snip-count)
      (define-values (x0 y0 c r cwidth cheight)
        (layout-info pb snip-count columns spacing))
      (values cwidth cheight))

    ;; Clear all elements from the container
    (define/public (clear-all)
      (with-writable pb
        (lambda ()
          (send pb select-all)
          (send pb clear)
          (send pb set-hover-pict #f 0 0))))

    ;; Add a plot snip to the container -- the layout of the snips will change
    ;; to accommodate the newly added snip.
    (define/public (add-plot-snip snip)
      (with-writable pb (lambda () (send pb insert snip)))
      (set! plot-snips (append plot-snips (list snip)))
      (queue-callback (lambda () (layout-snips pb plot-snips columns))))

    ;; Set the contents of the container to SNIP, removing all the other snips
    (define/public (set-plot-snip snip)
      (set-plot-snips snip))

    ;; Set the contents of the container to SNIPS, which can be multiple
    ;; snips, removing the previous set of snips from the container first.
    (define/public (set-plot-snips . snips)
      (with-writable pb
        (lambda ()
          (with-edit-sequence pb
            (lambda ()
              (for ([plot-snip (in-list plot-snips)])
                (send pb remove plot-snip))
              (for ([snip (in-list snips)])
                (send pb insert snip))
              (set! plot-snips snips)
              (layout-snips pb plot-snips columns))))))

    ;; Add a floating snip at position x, y.  A floating snip will not take
    ;; part in the grid layout and the user will be able to move this snip
    ;; with the mouse.
    (define/public (add-floating-snip snip x y)
      (send pb add-floating-snip snip x y))

    ;; Set a floating snip at position X, Y, removing any previous snips
    ;; first.
    (define/public (set-floating-snip snip x y)
      (send pb set-floating-snip snip x y))

    ;; Set a message to be displayed with the canvas is empty
    (define/public (set-background-message msg)
      (send pb set-background-message msg)
      (send this refresh))

    ;; Set a pict to be displayed at position X, Y, removing a previous hover
    ;; pict fist.  If pict is #f, the pict will be cleared
    (define/public (set-hover-pict pict x y)
      (send pb set-hover-pict pict x y)
      (send this refresh))

    ;; Same as `set-hover-pict`, but determine the X, Y coordinates from a
    ;; mouse EVENT.x
    (define/public (set-hover-pict-at-mouse-event pict event)
      (let ((ex (box (send event get-x)))
            (ey (box (send event get-y))))
        (send pb global-to-local ex ey)
        (send this set-hover-pict pict (unbox ex) (unbox ey))))

    ))

(provide/contract
 (plot-container% plot-container%/c))
