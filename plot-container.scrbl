#lang scribble/manual
@require[@for-label[plot-container
                    plot-container/hover-util
                    racket/gui/base
                    racket/base
                    racket/class
                    racket/contract
                    pict/snip
                    plot/no-gui
                    plot/snip
                    plot/utils]]

@title{plot-container -- Embed plot snips to GUI applications}
@author{Alex Harsányi}

@section{plot-container% Class}

@defmodule[plot-container]

@defclass[plot-container% editor-canvas% ()]{

  The @racket[plot-container%] class is a GUI container that can be used to
  embed @racket[snip%] objects such as plots produced by @racket[plot-snip] in
  GUI applications. It supports building interactive GUI applications which
  display different types of data, for example, when plot snips are combined
  with using @xmethod[2d-plot-snip% set-mouse-event-callback] to install
  on-hover callbacks for the plots.  The container has the following features:

  @bold{Snips} will be arranged either using a specified layout (see
  @method[plot-container% set-snips/layout]) or, by default, in rows and
  columns such that each occupy an equal amount of space -- i.e. all snips
  have the same size.  The snips will be resized dynamically if the container
  itself changes size or new snips are added.  The number of columns is
  specified when the @racket[plot-container%] is instantiated and the number
  of rows will be calculated based on the number of snips and the column
  count.  The plot snips must be any @racket[snip%], but this container class
  was originally intended to display plots produced by @racket[plot-snip],
  thus its name.  Plot snips can be added to the container using
  @method[plot-container% set-snip] or @method[plot-container% set-snips] or
  @method[plot-container% set-snips/layout].

  @bold{Floating snips} will show up on top of the plot snips and are not
  placed in rows and columns, instead the user can drag them around and they
  can be used to display additional information, such as a plot legend.  Use
  @method[plot-container% add-floating-snip] and @method[plot-container%
  set-floating-snip] to add floating snips.  Just as with the plot snips, any
  @racket[snip%] object is a valid floating snip, in particular
  @racket[pict-snip%] instances are useful for constructing images based on
  the pict package.

  A @bold{Background Message} can be set up to be shown when the plot
  container is empty, see @method[plot-container% set-background-message].
  This is useful, as the contents of the plot container can be changed
  dynamically at runtime.

  A @bold{Floating Snip} can be added using @method[plot-container%
  set-hover-pict] or @method[plot-container% set-hover-pict-at-mouse-event].
  This is intended to support implementing tooltips or displaying additional
  information when the user hovers the mouse over various plot elements.

  @defconstructor[([parent (or/c (is-a?/c frame%) (is-a?/c dialog%)
                                 (is-a?/c panel%) (is-a?/c pane%))]
                   [columns positive-integer? 1]
                   [spacing positive-integer? 5]
                   [background-message (or/c #f string?) #f])]{

    Create a new instance of a @racket[plot-container%] which will arrange
    plot @racket[snip%] objects in @racket[columns] columns with
    @racket[spacing] pixels between them.  The number of rows will be
    determined by the number of plot snips added.  Additional init argyments
    can be passed in, they will all go to the @racket[editor-canvas%] class,
    see its documentation for what options are available.

    @racket[background-message] represents the message to be shown when there
    are no snips in the container.  It can be changed using
    @method[plot-container% set-background-message].

  }

  @defmethod[(cell-dimensions [snip-count positive-integer?])
             (values real? real?)]{

    Return the dimensions of a plot snip when this @racket[plot-container%]
    would hold @racket[snip-count] snips.  The dimensions will be calculated
    based on the current size of the canvas plus the number of columns and
    spacing between snips.

    This method can be used to construct the plot snip instances with the
    correct dimensions and avoid a snip resize operation when these snips are
    added to the container.

    @bold{WARNING:} The cell dimensions are only valid if the snips are
    arranged in rows and columns by calling @method[plot-container%
    set-snips].  If a layout is used, using @method[plot-container%
    set-snips/layout], the cell dimensions returned by this method will not
    correspond to the ones assigned to the snips in the layout.

  }

  @defmethod[(clear-all) any/c]{

    Clear all snips from the container.

  }

  @defmethod[(set-snip [snip (is-a?/c snip%)]) any/c]{

    Set @racket[snip] as the only snip managed by the container, replacing any
    previous plot snips.

  }

  @defmethod[(set-snips [snip (is-a?/c snip%)] ...) any/c]{

    Set the contents of the container to the @racket[snip] instances passed in
    as paramters, replacing any previous plot snips.

  }

  @defmethod[(set-snips/layout [group plot-container-group?]) any/c]{

    Set the contents of the container to the @racket[group], which is a group
    of snips created using @racket[hgroup], @racket[vgroup] and
    @racket[cgroup].  This method allows subdividing the contents of the
    container area in a tree-like fashion.

  }

  @defmethod[(add-floating-snip [snip (is-a?/c snip%)] [x real?] [y real?]) any/c]{

    Add @racket[snip] as a @italic{floating snip} to the container and place
    it at @racket[x], @racket[y].  Any previous floating snips will be kept as
    well.

  }

  @defmethod[(set-floating-snip [snip (is-a?/c snip%)] [x real?] [y real?]) any/c]{

    Remove all existing @italic{floating snips}, than add @racket[snip] as a
    @italic{floating snip} to the container and place it at @racket[x],
    @racket[y].

  }

  @defmethod[(set-background-message [message (or/c #f string?)]) any/c]{

    Set a @racket[message] to be displayed in the plot container when it
    contains no snips at all.  If the message is @racket[#f], no message will
    be displayed.

  }

  @defmethod[(set-hover-pict [pict (or/c #f pict?)] [x real?] [y real?]) any/c]{

    Set @racket[pict] to be displayed at locations @racket[x], @racket[y], or
    hide the pict when @racket[#f].  The pict will be displayed on top of all
    other snips in the container and can be used to implement tool tips for
    the contents of the container.

  }

  @defmethod[(set-hover-pict-at-mouse-event [pict (or/c #f pict?)]
                                            [event (is-a?/c mouse-event%)])
             any/c]{

    Display @racket[pict] at the location of the mouse @racket[event].  This
    method will take the mouse event coordinates, convert them to plot
    container coordinates and call @method[plot-container% set-hover-pict].

  }

}

@defproc[(plot-container-group? [group any/c]) boolean?]{

  Check if the argument is a snip group, as produced by @racket[vgroup],
  @racket[hgroup] or @racket[cgroup].

}

@deftogether[(@defproc[(vgroup [item (listof (or/c plot-container-group? (is-a?/c snip%)))] ...
                               [#:border border (or/c positive? zero?) 0]
                               [#:spacing spacing (or/c positive? zero?) 5])
                       plot-container-group?]
             @defproc[(hgroup [item (listof (or/c plot-container-group? (is-a?/c snip%)))] ...
                              [#:border border (or/c positive? zero?) 0]
                              [#:spacing spacing (or/c positive? zero?) 5])
                      plot-container-group?]
             @defproc[(cgroup [columns positive-integer?]
                              [item (listof (or/c plot-container-group? (is-a?/c snip%)))] ...
                              [#:border border (or/c positive? zero?) 0]
                              [#:spacing spacing (or/c positive? zero?) 5])
                      plot-container-group?])]{

  Arrange all @racket[item]s with a @racket[border] around all the group and
  @racket[spacing] space between items.  All items will have the same
  dimensions and any @racket[plot-container-group?] items will have this space
  sub-divided among the items in that group.  The result of these functions
  are intended to be passed to @method[plot-container% set-snips/layout].

  @racket[vgroup] will place items in one vertical column with the height
  equally divided between all items.

  @racket[hgroup] will place items in one horizontal row with the width
  equally divided between all items.

  @racket[cgroup] will place all items in @racket[columns], with the number of
  rows depending on the number of itmes.  The width is divided equally between
  the number of columns and the height is divided equally between the number
  of rows.

}


@section{Some utility functions to use with plots and plot containers}

@defmodule[plot-container/hover-util]

This module provides a collection of helper functions for building interactive
plots.  The @racket[plot-snip] function returns a @racket[snip%] representing
the plot, and this snip has two additional methods, @method[2d-plot-snip%
set-mouse-event-callback] and @method[2d-plot-snip% set-overlay-renderers],
which help with this. See the plot documentation for @racket[2d-plot-snip%]
for more details.

@defproc[(good-hover? [snip (is-a?/c snip%)]
                      [x (or/c real? #f)]
                      [y (or/c real? #f)]
                      [event (is-a?/c mouse-event%)])
         boolean?]{

  Return @racket[#t] when the @racket[x], @racket[y] and @racket[event] passed
  to a plot snip mouse callback are valid, and hover information should be
  displayed for the point at @racket[x], @racket[y].

  The parameters are considered valid when the coordinates @racket[x] and
  @racket[y] are not @racket[#f], the mouse event is a motion event and the
  plot @racket[snip] is directly under the mouse with no other snips above it.

  The @racket[x], @racket[y] coordinates can be @racket[#f] when they are
  inside the plot snip but not on the plot itself, for example in the axes
  area.

  This function encapsulates all the logic on whether to add or clear overlay
  renderers from a plot, and allows writing hover callbacks in the following
  form:

  @; NOTE apparently the indentation of the block below is also kept in the
  @; resulting document.

@#reader scribble/comment-reader
(racketblock
(define (hover-callback snip event x y)
  (if (good-hover? snip x y event)
    ;; Need to add overlay renderers for position x,y
    (send snip #,(method 2d-plot-snip% set-overlay-renderers) ...)
    ;; Need to clear any overlay renderers
    (send snip #,(method 2d-plot-snip% set-overlay-renderers) #f))))

}

@defproc[(xposition->histogram-slot [xposition number?]
                                    [skip number? (discrete-histogram-skip)]
                                    [gap number? (discrete-histogram-gap)])
         (values (or/c #f exact-nonnegative-integer?)
                 (or/c #f exact-nonnegative-integer?))]{

  Convert the @racket[xposition] received by the hover callback in a histogram
  plot back to the series and the slot withing that series.  @racket[skip] and
  @racket[gap] are the @racket[#:skip] and @racket[#:gap] arguments passed to
  the @racket[discrete-histogram] renderer, they default to
  @racket[discrete-histogram-gap] and @racket[discrete-histogram-skip]
  parameters, just as they do for the @racket[discrete-histogram] renderer.

  Returns two values, the series, when multiple historams are plotted and the
  slot within that histogram.  Will return @racket[(values #f #f)] if the X
  position is between the bars of the histogram.
}

@defproc[(get-snip-location [snip (or/c #f (is-a?/c snip%))])
         (or/c #f (cons/c number? number?))]{

  Return the location of @racket[snip] as a @racket[(cons X Y)], or return
  @racket[#f] if @racket[snip] is not added to an editor.

  Together with @racket[move-snip-to], this function can be used to retrieve
  and save the location of any hover snips in a @racket[plot-container%] and
  restore them at a later time.
}

@defproc[(move-snip-to [snip (is-a?/c snip%)]
                       [location (or/c #f (cons/c number? number?))])
         any/c]{

  Move @racket[snip] to @racket[location], adjusting it as necessary to remain
  fully visible inside the canvas.  This is intended to be used with locations
  retrieved by @racket[get-snip-location] and unlike the
  @xmethod[plot-container% add-floating-snip], this function will
  adjust the location so that the snip visible in the container -- this is
  useful if the container has changed size since the location was retrieved
  and saved.

  Assumes the @racket[snip] is added to an editor.

}

@;{
(provide/contract
 ;; NOTE all these are actually instances of 2d-plot-snip%, but the plot
 ;; library does not export that type.

 (set-mouse-event-callback (-> (is-a?/c snip%) (-> (is-a?/c snip%) (is-a?/c mouse-event%) (or/c #f number?) (or/c #f number?) any/c) any/c))
 (set-overlay-renderers (-> (is-a?/c snip%) (or/c (treeof renderer2d?) #f null) any/c))

 (hover-vrule (-> real? renderer2d?))
 (hover-label (->* (real? real?) () #:rest (listof (or/c string? pict? #f)) renderer2d?))
 (hover-vrange (-> real? real? (is-a?/c color%) renderer2d?))
 (hover-markers (-> (listof (vector/c real? real?)) renderer2d?))

 (make-hover-badge (-> (listof (listof (or/c #f string?))) pict?))

 (plot-to-canvas (->* ((treeof (or/c renderer2d? nonrenderer?)) (is-a?/c plot-container%))
                      (#:x-min (or/c real? #f) #:x-max (or/c real? #f)
                       #:y-min (or/c real? #f) #:y-max (or/c real? #f)
                       #:width (and/c integer? positive?)
                       #:height (and/c integer? positive?)
                       #:title (or/c string? #f)
                       #:x-label (or/c string? #f) #:y-label (or/c string? #f)
                       #:legend-anchor symbol?)
                      any/c))
 )
}
