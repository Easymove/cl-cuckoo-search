;; (ql:quickload "alexandria")
(ql:quickload "anaphora")
;; (ql:quickload "bordeaux-threads")

(defpackage :cl-csa
  (:use :common-lisp :anaphora))

(in-package :cl-csa)

;;; random -------------------------------------------------------
(defun get-random-val (&rest args)
  (assert (<= (length args) 2))
  (let* ((u-bound (or (and (cdr args) (second args))
                      (car args)))
         (l-bound (or (and (cdr args) (car args))
                      0))
         (base (abs l-bound))
         (max (- u-bound l-bound)))
    (- (random max) base)))

(defun get-random-list (n &rest params)
  (loop for i from 1 to n collect (apply #'get-random-val params)))


;;; definitions --------------------------------------------------
(defvar *cuckoos-search-spec* nil)
(defvar *cuckoos* nil)

(defclass cuckoo-func ()
  ((func :reader cf-func
         :initarg :func)
   (value-gen :reader cf-value-gen
              :initarg :value-gen)))

(defclass lambda-spec ()
  ((l-bound :reader ls-l-bound
            :initarg :l-bound
            :initform 1)
   (u-bound :reader ls-u-bound
            :initarg :u-bound
            :initform 3)))

(defclass alpha-spec ()
  ((l-bound :reader as-l-bound
            :initarg :l-bound
            :initform 0.0001)
   (u-bound :reader as-u-bound
            :initarg :u-bound
            :initform 0.4)))

(defclass cuckoo-search-spec ()
  ((func :reader css-func
         :initarg :func
         :type cuckoo-func)
   (lambda-spec :reader css-lambda-spec
                :initarg :lambda-spec
                :type 'lambda-spec
                :initform (make-instance 'lambda-spec))
   (alpha-spec :reader css-alpha-spec
               :initarg :alpha-spec
               :type 'alpha-spec
               :initform (make-instance 'alpha-spec))
   (cuckoos-count :reader css-cuckoos-count
                  :initarg :cuckoos-count)
   (a-nests-count :reader css-a-nests-count
                  :initarg :a-nest-count)
   (max-generation :reader css-max-gen
                   :initarg :max-gen)
   (stop-pred :reader css-stop-pred
              :initarg :stop-pred
              :initform (constantly nil))))

;;; cuckoo ------------------------------------------------------
(defclass cuckoo ()
  ((params :accessor c-params
           :initarg :params
           :type list)
   (id :accessor c-id
       :initarg :id
       :type integer)))

(defmethod print-object ((obj cuckoo) stream)
  (format stream "#<CUCKOO ~A; ~A; ~A>" (c-id obj) (cuckoo-fitness obj) (cuckoo-value obj)))

(defgeneric get-lambda (cuckoo))
(defgeneric get-alpha (cuckoo iteration))
(defgeneric levy-flight (cuckoo gen))
(defgeneric cuckoo-fitness (cuckoo))
(defgeneric cuckoo-value (cuckoo))
(defgeneric cuckoo> (cuckoo1 cuckoo2))
(defgeneric cuckoo< (cuckoo1 cuckoo2))

(defmethod cuckoo> ((c1 cuckoo) (c2 cuckoo))
  (> (cuckoo-fitness c1) (cuckoo-fitness c2)))

(defmethod cuckoo< ((c1 cuckoo) (c2 cuckoo))
  (< (cuckoo-fitness c1) (cuckoo-fitness c2)))

(defmethod cuckoo-value ((c cuckoo))
  (apply (cf-func (css-func *cuckoos-search-spec*)) (c-params c)))

(defmethod cuckoo-fitness ((c cuckoo))
  (/ 1 (1+ (cuckoo-value c))))

(defmethod get-lambda ((c cuckoo))
  (let* ((l-spec (css-lambda-spec *cuckoos-search-spec*))
         (c-count (css-cuckoos-count *cuckoos-search-spec*))
         (l-min (ls-l-bound l-spec))
         (l-max (ls-u-bound l-spec)))
    (- l-max (/ (* (c-id c) (- l-max l-min))
                (1- c-count)))))

(defmethod get-alpha ((c cuckoo) iter)
  (let* ((a-spec (css-alpha-spec *cuckoos-search-spec*))
         (a-min (as-l-bound a-spec))
         (a-max (as-u-bound a-spec))
         (k (css-max-gen *cuckoos-search-spec*)))
    (* a-max (expt (/ a-min a-max) (/ iter k)))))

(defun levy-random (lambda-val alpha-val)
  (* (expt (/ (1+ (random 999))
              1000)
           (- (/ 1 lambda-val)))
     alpha-val
     (- (/ (1+ (random 999))
           1000)
        0.5)))

(defmethod levy-flight ((c cuckoo) gen)
  (let* ((vals (c-params c))
         (alpha-val (get-alpha c gen))
         (lambda-val (get-lambda c)))
    (make-instance 'cuckoo
                   :params (mapcar (lambda (x)
                                     (+ x (levy-random lambda-val alpha-val)))
                                   vals))))

;;; initial functions -------------------------------------------
(defun generate-initial-cuckoos ()
  (let* ((c-count (css-cuckoos-count *cuckoos-search-spec*))
         (res (make-array c-count)))
    (loop for i from 0 to (1- c-count)
       do (setf (aref res i) (make-instance 'cuckoo
                                            :params (funcall
                                                     (cf-value-gen
                                                      (css-func *cuckoos-search-spec*)))
                                            :id i)))
    res))

(defun sort-cuckoos (c-array)
  (loop
     for c across (sort c-array #'cuckoo>)
     for i from 0 to (1- (length c-array))
     do (setf (c-id c) i))
  c-array)

;;; main functions ----------------------------------------------------
(defun cuckoo-search (cuckoo-spec)
  (let* ((*cuckoos-search-spec* cuckoo-spec)
         (*cuckoos* (generate-initial-cuckoos)))
    (cuckoo-search-inner)))

(defun cuckoo-search-inner ()
  (loop for gen from 0 to (1- (css-max-gen *cuckoos-search-spec*))
     when (not (funcall (css-stop-pred *cuckoos-search-spec*)))
     do (cuckoo-step gen))
  (cuckoo-value (aref *cuckoos* 0)))

(defun refresh-cuckoos ()
  (let ((sorted-cuckoos (sort-cuckoos *cuckoos*)))
    (loop for i from (- (length sorted-cuckoos) (css-a-nests-count *cuckoos-search-spec*))
       to (1- (length sorted-cuckoos))
       do (setf (aref sorted-cuckoos i)
                (make-instance 'cuckoo
                               :params (funcall
                                        (cf-value-gen
                                         (css-func *cuckoos-search-spec*)))
                               :id i)))
    (setf *cuckoos* sorted-cuckoos)))

(defun cuckoo-step (gen)
  (loop for c across *cuckoos*
     do (let* ((new-cuckoo (levy-flight c gen))
               (cuckoo-id (random (css-cuckoos-count *cuckoos-search-spec*)))
               (rand-cuckoo (aref *cuckoos* cuckoo-id)))
          (when (cuckoo> new-cuckoo rand-cuckoo)
            (setf (c-id new-cuckoo) cuckoo-id)
            (setf (aref *cuckoos* cuckoo-id) new-cuckoo))))
  (refresh-cuckoos))

;;; tested functions ----------------------------------------------------
(defun sphere (&rest xs)
  (reduce #'+ (mapcar (lambda (x) (expt x 2)) xs)))

;;; tests ---------------------------------------------------------------
(defun test ()
  (sb-profile:profile sort-cuckoos levy-flight)
  (time
   (let ((res (cuckoo-search (make-instance 'cuckoo-search-spec
                                            :func (make-instance 'cuckoo-func
                                                                 :func #'sphere
                                                                 :value-gen (lambda () (get-random-list 10 -100 100)))
                                            :cuckoos-count 200
                                            :a-nest-count 50
                                            :max-gen 1000))))
     (format t "~%Y: ~A~%~%" res)))
  (sb-profile:report)
  (sb-profile:unprofile sort-cuckoos levy-flight))
