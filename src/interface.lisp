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

(in-package :restas-pg-auth)

(defun logged-in-p ()
  (let ((c (get-auth-cookie)))
    (if (and c (check-session-expiration c))
	t nil)))

(defun not-logged-in-p ()
  (not (logged-in-p)))

(define-condition no-user-for-session (error)
  ((s :initarg :s :reader s-of)))
(defun logged-in-as ()
  (with-connection *db*
    (let ((u (query (:select 'username :from 'userauth :where
			     (:= 'id (:select 'user-id :from 'session :where
					      (:= 's (get-auth-cookie))))))))
      (if u (car u)
	  (error 'no-user-for-session :s (get-auth-cookie))))))

(defun maybe-login (username password)
  (if (and (not (logged-in-p))
	   (same-password-p username password))
      (set-auth-cookie (s-of (make-session (get-auth-by-name username))))
      nil))

(defun maybe-logout ()
  (if (logged-in-p)
      (remove-session (get-auth-cookie))
      nil))