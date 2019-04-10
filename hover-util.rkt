#lang racket/base
;; hover-util.rkt -- utilities for interactive plots
;;
;; This file is part of plot-container -- canvas to hold plot snips
;; Copyright (c) 2019 Alex Harsányi <AlexHarsanyi@gmail.com>
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
(require "private/hover-util.rkt")
(provide
 set-mouse-event-callback
 set-overlay-renderers
 hover-vrule
 hover-label
 hover-vrange
 hover-markers
 make-hover-badge
 move-snip-to
 get-snip-location
 xposition->histogram-slot
 plot-to-canvas
 good-hover?)
