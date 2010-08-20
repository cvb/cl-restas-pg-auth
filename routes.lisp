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

(in-package #:restas-pg-auth)

(restas:define-route login ("login"
			    :method :get)
  (if (logged-in-p)
      (restas:render-object #'restas-pg-auth.view:loged-in (list :username (logged-in-as)))
      (restas:render-object #'restas-pg-auth.view:login nil)))

(restas:define-route login/post ("login"
				 :method :post)
  (let ((username (hunchentoot:post-parameter "username"))
       (password (hunchentoot:post-parameter "password")))
    (if (maybe-login username password)
       (restas:redirect 'login)
       "Wrong credentials")))
 

(restas:define-route logout ("logout")
  (maybe-logout)
  (restas:redirect 'login))

(restas:define-route register ("register")
  "")