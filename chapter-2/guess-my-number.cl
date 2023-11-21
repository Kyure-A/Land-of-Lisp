;; defparameter は再度代入可能
(defparameter *small* 0)
(defparameter *big* 100)

;; defvar は不可
(defvar *test* -1)

(defun guess-my-number ()
  (ash (+ *small* *big*) -1)) ;; ash は bitshift する; 左にするときは 1, 右にするときは -1

(defun smaller ()
  (setf *big* (1- (guess-my-number)))
  (guess-my-number))

(defun bigger ()
  (setf *small* (1+ (guess-my-number)))
  (guess-my-number))

(defun start-guess-number ()
  (defparameter *small* 0)
  (defparameter *big* 100)
  (guess-my-number))

(defun correct ()
  (defparameter *small* 0)
  (defparameter *big* 100)
  (let ((message "Yeah, good!"))
    (princ message)))
