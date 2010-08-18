;; Copyright 2010 Peter Goncharov

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or	
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(restas:define-module #:restas-pg-auth
    (:use #:cl #:iterate #:postmodern #:local-time)
  (:export #:*db*))

(in-package #:restas-pg-auth)

(defparameter *db* nil)
(set-local-time-cl-postgres-readers)
(let ((basepath (merge-pathnames "templates/"
                                 (asdf:component-pathname (asdf:find-system '#:cl-restas-pg-auth)))))
  (iter (for tmpl in '("login"))
        (closure-template:compile-template :common-lisp-backend
                                           (merge-pathnames (format nil "~A.tmpl" tmpl)
                                                            basepath))))