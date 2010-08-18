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

(defclass userauth ()
  ((id :col-type serial :reader id-of)
   (name :col-type (varchar 255) :initarg :name
         :reader country-name)
   (password :col-type (varchar 255) :initarg :password
	     :reader password-of)
   (salt :col-type (varchar 255) :initarg :salt
              :reader salt-of))
  (:metaclass dao-class)
  (:unique name)
  (:keys id))

(defun random-passwd (len)
  (let* ((chars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789!@#$%^&*()")
	 (n (length chars)))
    (coerce
     (loop for i from 1 to len
	   collect (elt chars (random n)))
     'string)))

(defun calc-hash (pass salt)
  (ironclad:byte-array-to-hex-string 
   (ironclad:digest-sequence :sha256 
			     (ironclad:ascii-string-to-byte-array 
			      (concatenate 'string  pass salt)))))

(defgeneric (setf password) (password userauth)
  (:documentation "Setf user password, hashing and salt included"))

(defmethod (setf password) (pass (user userauth))
  (let* ((salt (random-passwd 10))
	 (hpass (calc-hash pass salt))) 
    (setf (slot-value user 'password) hpass)
    (setf (slot-value user 'salt) salt)))

(defgeneric same-password-p (username password)
  (:method ((username string) (password string))
    (with-connection *db*
      (let ((u (select-dao 'userauth (:= 'name username))))
	(when u
	 (if (string= (password-of (car u)) (calc-hash password (salt-of (car u))))
	     t nil))))))

(defun make-userauth (name password)
  (let ((s (random-passwd 10)))
    (make-instance 'userauth :name name :password (calc-hash password s) :salt s)))

(define-condition same-name-exist (error)
  ((name :initarg :name :accessor name-of)))

(defun add-userauth (username password)
  (with-connection *db*
    (if (select-dao 'userauth (:= 'name username))
	(error 'same-name-exist :name username)
	(insert-dao (make-userauth username password)))))

(defun del-userauth (username)
  (with-connection *db*
    (let ((u (select-dao 'userauth (:= 'name username))))
      (when u (delete-dao (car u))))))

(defun check-userauth-existence ()
  (with-connection *db* (table-exists-p (dao-table-name 'userauth))))

(defun create-userauth-table ()
  (with-connection *db*
    (query (dao-table-definition 'userauth))))

(defun init-userauth ()
  (when (not (check-userauth-existence))
    (create-userauth-table)))