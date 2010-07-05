;;;; evol - package.lisp
;;;; Copyright (C) 2010  Alexander Kahl <e-user@fsfe.org>
;;;; This file is part of evol.
;;;; evol is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; evol is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :cffi-regex)

(define-foreign-library libc
  (:unix "libc.so.6"))

(use-foreign-library libc)

(defcstruct registers
  (num-regs :unsigned-int)
  (start :regoff-t)
  (end :regoff-t))

(defcfun ("re_set_syntax" set-syntax) :reg-syntax (syntax :reg-syntax))

(defcfun ("re_compile_pattern" compile-pattern) :string
  (pattern :string) (length :size-t) (buffer pattern-buffer))

(defcfun ("re_compile_fastmap" compile-fastmap) :int (buffer pattern-buffer))

(defcfun ("re_search" search-string) :int
  (buffer pattern-buffer) (string :string) (length :int) (start :int)
  (range :int) (registers registers))

(defcfun ("re_search_2" search-string-2) :int
  (buffer pattern-buffer)
  (string1 :string) (length1 :int)
  (string2 :string) (length2 :int)
  (start :int) (range :int)
  (registers registers)
  (stop :int))

(defcfun ("re_match" match) :int
  (buffer pattern-buffer) (string :string) (length :int) (start :int)
  (registers registers))

(defcfun ("re_match_2" match-2) :int
  (buffer pattern-buffer)
  (string1 :string) (length1 :int)
  (string2 :string) (length2 :int)
  (start :int) (range :int)
  (registers registers)
  (stop :int))

(defcfun ("re_set_registers" set-registers) :void
  (buffer pattern-buffer) (registers registers)
  (num-regs :unsigned-int) (starts :regoff-t) (ends :regoff-t))
