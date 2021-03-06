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

(defpackage #:cl-restas-pg-auth-asd
  (:use :cl :asdf))

(in-package #:cl-restas-pg-auth-asd)

(defsystem cl-restas-pg-auth
  :depends-on (:iterate :restas :postmodern :ironclad :local-time :cl-postgres+local-time :closure-template)
  :serial t
  :components ((:file "cl-restas-pg-auth")
	       (:file "routes")
	       (:module src
			:serial t
			:components ((:file "user")
				     (:file "session")
				     (:file "cookies")
				     (:file "interface")))))