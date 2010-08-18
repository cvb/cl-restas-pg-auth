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

(defparameter *session-expire* nil)

(defclass session ()
  ((user-id :col-type integer :initarg :user-id :reader user-id)
   (s :col-type (varchar 255) :initarg :s :reader s-of)
   (timestamp :col-type timestamp-with-time-zone :initform (simple-date:universal-time-to-timestamp (get-universal-time))
	      :reader timestamp-of))
  (:metaclass dao-class)
  (:keys user-id s))


(defgeneric make-session (user))

(defmethod make-session ((user userauth))
  (with-connection *db*
    (insert-dao (make-instance 'session :user-id (id-of user) :s (random-passwd 64)))))

(defmethod make-session ((user string))
  (with-connection *db*
    (let ((u (select-dao 'userauth (:= 'name user))))
      (when u (make-session (car u))))))

(defun exp-to-sec (exp)
  (let ((unit (cadr exp)))
    (case unit
      ((:sec :secs :second :seconds)
       (car exp))
      ((:min :mins :minute :minutes)
       (* 60 (car exp)))
      ((:hour :hours)
       (* 60 60 (car exp)))
      (:day (* 60 60 24 (car exp)))
      (:week (* 60 60 24 7 (car exp)))
      (:month (* 60 60 24 7 30 (car exp))))))


(defgeneric check-session-expiration (session))
(defmethod check-session-expiration ((s session))
  (if (< (exp-to-sec *session-expire*)
	 (timestamp-difference (now) (timestamp-of s)))
       (with-connection *db*
	    (delete-dao s) nil)
      s))

(define-condition no-such-session (error)
  ((s :initarg :s :reader s-of)))
(defmethod check-session-expiration ((s string))
  (with-connection *db*
    (let ((s1 (select-dao 'session (:= 's s))))
      (if s1
	  (check-session-expiration (car s1))
	  (error 'no-such-session :s s)))))

(defgeneric remove-session (s))

(defmethod remove-session ((s session))
  (with-connection *db*
    (mapcar #'delete-dao (select-dao 'session (:= 'user-id (user-id s))))))

(defmethod remove-session ((s string))
  (with-connection *db*
    (let ((d (select-dao 'session (:= 's s))))
     (when d (mapcar #'delete-dao
		     (select-dao 'session
				 (:= 'user-id (user-id (car d)))))))))

(defmethod remove-session ((s integer))
  (with-connection *db*
    (let ((d (select-dao 'session (:= 'user-id s))))
     (mapcar #'delete-dao d))))

(defun remove-expired-sessions ()
  (with-connection *db*
    (mapcar #'check-session-expiration (select-dao 'session))))

(defun check-session-existence ()
  (with-connection *db*
    (table-exists-p (dao-table-name 'session))))

(defun create-session-table ()
  (with-connection *db*
    (query (dao-table-definition 'session))))

(defun init-session ()
  (when (not (check-session-existence))
    (create-session-table)))

