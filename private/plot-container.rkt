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

(require pict
         racket/class
         racket/contract
         racket/draw
         racket/gui/base
         racket/match
         racket/math)

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

    (inherit set-before insert remove global-to-local
             find-snip set-caret-owner find-first-snip)

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
      #t
      #;(or writable? (memq snip floating-snips)))

    (define/override (can-do-edit-operation? op [recursive? #t])
      (case op
        [(copy select-all) #t]
        [else    writable?]))

    (define/augment (after-insert snip before x y)
      (when (memq snip floating-snips)
        (set-before snip #f)))

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
          (insert snip x y))))

    (define/public (set-floating-snip snip x y)
      (with-writable-edit-sequence this
        (lambda ()
          (for ((snip (in-list floating-snips)))
            (remove snip))
          (if snip
              (begin
                (set! floating-snips (list snip))
                (insert snip x y))
              (set! floating-snips '())))))

    (define/override (on-event event)
      (let ([x (box (send event get-x))]
            [y (box (send event get-y))])
        (global-to-local x y)
        (define snip (find-snip (unbox x) (unbox y)))
        ;; Set the snip under the mouse as the caret owner, so they receive
        ;; mouse events.  This is needed by the map-snip% for mouse scroll
        ;; zoom (which are keyboard events!), but it is unclear if this is
        ;; generally the best policy.  For now, this does not interfere with
        ;; plot snips which are the other client of this package.
        (when snip
          (queue-callback
           (lambda () (set-caret-owner snip))))
        (super on-event event)))

    (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
      (if before?
          (begin
            (send dc clear)
            ;; Draw a message when there is no snip in the pasteboard.
            (unless (find-first-snip)
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

;; A group of SNIPs used for layout purposes. The CONTENTS is a list of SNIP%
;; instances or other SNIP-GROUPs which will be arranged in COLUMNS + rows
;; such that each cell has the same size (any SNIP-GROUPs in CONTENTS will
;; than split their cell size between them).
;;
;; When placing and re-sizing snips, a BORDER is left around the area and
;; SPACING pixels between the adjacent snips.
(struct snip-group (border spacing columns contents))

(define plot-container-group? snip-group?)

;; Create a snip group from ITEMS which will layout the snips vertically, in
;; one column.
(define (vgroup #:border [border 0] #:spacing [spacing 5] . items)
  (snip-group border spacing 1 items))

;; Create a snip group from ITEMS which will layout the snips horizontally in
;; one row.
(define (hgroup #:border [border 0] #:spacing [spacing 5] . items)
  (snip-group border spacing (length items) items))

;; Create a snip group from ITEMS which will lay them out in a grid of COLUMNS
;; columns, with the rows depending on the number of snips.
(define (cgroup columns #:border [border 0] #:spacing [spacing 5] . items)
  (snip-group border spacing columns items))

;; Insert into the pasteboard all the snips in the snip-group GROUP, replacing
;; any other snips in the pasteboard PB.
(define (insert-group-contents pb group)
  (match-define (snip-group border spacing columns contents) group)
  (for ([item (in-list contents)])
    (if (snip-group? item)
        (insert-group-contents pb item)
        (send pb insert item))))

;; position and resize the snips in GROUP to fill the entire area of the
;; pasteboard PM.  Assumes that the snips are already inserted into the
;; pasteboard using `insert-group-contents`.
(define (layout-group pb group)
  (define (do-layout group x y width height)
    (match-define (snip-group border spacing cols contents) group)
    (define rows (exact-ceiling (if (> cols 0) (/ (length contents) cols) 0)))
    (define-values (cwidth cheight)
      (let ([w (- width border border)]
            [h (- height border border)])
        (values (exact-floor (if (> cols 0) (/ (- w (* (sub1 cols) spacing)) cols) 0))
                (exact-floor (if (> rows 0) (/ (- h (* (sub1 rows) spacing)) rows) 0)))))
    (when (and (> cwidth 0) (> cheight 0))
      (for ([(item index) (in-indexed (in-list contents))])
        (let-values ([(row col) (quotient/remainder index cols)])
          (let ([x (+ x border (* col (+ cwidth spacing)))]
                [y (+ y border (* row (+ cheight spacing)))])
            (if (snip-group? item)
              (do-layout item x y cwidth cheight)
              (begin
                (send pb move-to item x y)
                (send pb resize item cwidth cheight))))))))

  (define c (send pb get-canvas))
  (define-values (w h) (send c get-client-size))
  ;; NOTE: the horizontal and vertical inset reduce the width and height of
  ;; the drawing area of the canvas, but not the origin.  I.e. position 0,0 in
  ;; the editor-canvas% is actually at hinset,vinset in the underlying canvas%.
  (define hinset (send c horizontal-inset))
  (define vinset (send c vertical-inset))
  (define hmargin (send c horiz-margin))
  (define vmargin (send c vert-margin))
  (define width (- w hinset hinset hmargin hmargin))
  (define height (- h vinset vinset vmargin vmargin))
  (do-layout group hmargin vmargin width height))

;; Contract for the plot-container% class, since we export it
(define plot-container%/c
  (class/c
   (init [parent (or/c (is-a?/c frame%) (is-a?/c dialog%)
                       (is-a?/c panel%) (is-a?/c pane%))])
   (init-field [columns positive-integer?] [spacing positive-integer?])
   (cell-dimensions (->*m (positive-integer?)
                          (#:columns positive-integer?
                           #:spacing positive-integer?)
                          (values positive-integer? positive-integer?)))
   (clear-all (->m any/c))
   (set-plot-snip (->m (is-a?/c snip%) any/c))
   (set-plot-snips (->*m () #:rest (listof (is-a?/c snip%)) any/c))
   (set-snip (->m (is-a?/c snip%) any/c))
   (set-snips (->*m () #:rest (listof (is-a?/c snip%)) any/c))
   (set-snips/layout (->m plot-container-group? any/c))
   (add-floating-snip (->m (is-a?/c snip%) real? real? any/c))
   (set-floating-snip (->m (or/c #f (is-a?/c snip%)) real? real? any/c))
   (set-background-message (->m (or/c #f string?) any/c))
   (set-hover-pict (->m (or/c #f pict?) real? real? any/c))
   (set-hover-pict-at-mouse-event (->m (or/c #f pict?) (is-a?/c mouse-event%) any/c))
   (export-image-to-file (->*m (path-string?)
                               ((or/c positive-integer? #f) (or/c positive-integer? #f))
                               any/c))
   ))

;; An editor-canvas% for holding one or more plot snips with support for
;; additional features: floating snips for implementing plot legends that can
;; be moved around with the mouse, and hover pictures to display extra
;; information about the current position on the plot.
(define plot-container%
  (class editor-canvas%
    (init parent [style null])
    (init-field [columns 1] [spacing 5])

    (define pb (new plot-container-pasteboard%))
    (define group #f)

    (super-new [parent parent]
               [editor pb]
               [style (list* 'no-hscroll 'no-vscroll style)])

    (define/override (on-size w h)
      (super on-size w h)
      (when group
        (queue-callback
         (lambda ()
           (with-writable pb
             (lambda ()
               (with-edit-sequence pb
                 (lambda ()
                   (layout-group pb group)))))))))

    ;; Return the width and height for a snip assuming that SNIP-COUNT snips
    ;; will be placed in the container -- this can be used to create the plot
    ;; snips of the right size before they are inserted into the container so
    ;; we avoid an initial resize.
    ;;
    ;; NOTE: the cell dimensions will be valid only if the snips are inserted
    ;; using `set-plot-snips`.  If the snips are grouped (see vgroup, hgroup
    ;; and cgroup and `set-snips/layout`, the cell dimensions for each snip
    ;; will depend on the position inside the layout grid).
    (define/public (cell-dimensions snip-count
                                    #:columns [columns columns]
                                    #:spacing [spacing spacing])
      (define-values (w h) (send this get-client-size))
      (define hinset (send this horizontal-inset))
      (define vinset (send this vertical-inset))
      (define hmargin (send this horiz-margin))
      (define vmargin (send this vert-margin))
      (define width (- w hinset hinset hmargin hmargin))
      (define height (- h vinset vinset vmargin vmargin))
      (define rows (exact-ceiling (if (> columns 0) (/ snip-count columns) 0)))
      (values (exact-floor (if (> columns 0) (/ (- width (* (sub1 columns) spacing)) columns) 0))
              (exact-floor (if (> rows 0) (/ (- height (* (sub1 rows) spacing)) rows) 0))))

    ;; Clear all elements from the container
    (define/public (clear-all)
      (with-writable pb
        (lambda ()
          (send pb select-all)
          (send pb clear)
          (send pb set-hover-pict #f 0 0)))
      (set! group #f))

    ;; Set the contents of the container to SNIP, removing all the other snips
    (define/public (set-snip snip)
      (set-snips/layout (snip-group 0 0 columns (list snip))))

    ;; Old, interface, use set-snip
    (define/public (set-plot-snip snip) (set-snip snip))

    ;; Set the contents of the container to SNIPS, which can be multiple
    ;; snips, removing the previous set of snips from the container first.
    ;; The snips will be arranged `columns'
    (define/public (set-snips . snips)
      (set-snips/layout (snip-group 0 spacing columns snips)))

    ;; Old interface, use set-snips
    (define/public (set-plot-snips . snips)
      (send/apply this set-snips snips))

    ;; Set the contents of the container to NEW-GROUP which is a snip group
    ;; created using `vgroup`, `hgroup` and `cgroup` -- this allows some
    ;; flexibility in "splitting" the container area among the snips.  Any
    ;; previous snips in the container will be removed.
    (define/public (set-snips/layout new-group)
      (with-writable pb
        (lambda ()
          (with-edit-sequence pb
            (lambda ()
              (send pb select-all)
              (send pb clear)
              (set! group new-group)
              (insert-group-contents pb group)
              (layout-group pb group))))))

    ;; Add a floating snip at position x, y.  A floating snip will not take
    ;; part in the group layout and the user will be able to move this snip
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

    ;; Export the contents of the container to an image in FILE-NAME.  The
    ;; image will be width/height in size, or if they are #f, the image will
    ;; have the same size as the canvas.
    (define/public (export-image-to-file file-name (width #f) (height #f))
      (let-values (((cw ch) (send this get-size)))
        (unless (and width height)
          (set! width (or width cw))
          (set! height (or height ch)))
        (let* ((bitmap (if (regexp-match #px".*\\.(?i:svg)" file-name)
                           #f
                           (make-bitmap width height #t)))
               (dc (if bitmap
                       (new bitmap-dc% [bitmap bitmap])
                       (new svg-dc%
                            [width width] [height height]
                            [output file-name]
                            [exists 'truncate/replace]))))
          ;; NOTE: scaling works, but makes the entire plot blurry
          (send dc scale (/ width cw) (/ height ch))
          (unless bitmap
            (send dc start-doc "export to file"))
          ;; NOTE: print-to-dc handles start-page/end-page calls
          (send (send this get-editor) print-to-dc dc 0)
          (unless bitmap
            (send dc end-doc))
          (when bitmap
            (send bitmap save-file file-name 'png)))))

    ;; Don't swallow scroll wheel events, as this canvas does not scroll.
    ;; Instead, pass these events to any snips which might need them (like the
    ;; map-snip).
    (send this wheel-step #f)

    ))

(provide/contract
 (plot-container-group? (-> any/c boolean?))
 (vgroup (->* () (#:border (or/c positive? zero?) #:spacing (or/c positive? zero?))
              #:rest (listof (or/c plot-container-group? (is-a?/c snip%)))
              plot-container-group?))
 (hgroup (->* () (#:border (or/c positive? zero?) #:spacing (or/c positive? zero?))
              #:rest (listof (or/c plot-container-group? (is-a?/c snip%)))
              plot-container-group?))
 (cgroup (->* (positive-integer?) (#:border (or/c positive? zero?) #:spacing (or/c positive? zero?))
              #:rest (listof (or/c plot-container-group? (is-a?/c snip%)))
              plot-container-group?))
 (plot-container% plot-container%/c))
