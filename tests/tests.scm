;;; tests.scm --- List of unit-test files for running with testeez.

;; Copyright (C) 2008, 2009, 2010 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.

;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public
;; License along with this library. If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:


((systems irclogs)
 (files
  ("query.scm"
   (srfi :8 receive)
   (srfi :19 time)
   (irclogs utils)
   (irclogs parse)
   (irclogs query))
  ((code
    (set-test-debug-errors?! #t))
   "window.scm"
   (srfi :8 receive)
   (srfi :19 time)
   (spells misc)
   (spells match)
   (wak foof-loop)
   (irclogs parse)
   (irclogs window))))

