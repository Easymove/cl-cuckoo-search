(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "alexandria")
  (ql:quickload "anaphora")
  (ql:quickload "cl-csv")
  ;; (ql:quickload "bordeaux-threads")
  )

(defpackage :cl-csa
  (:use :common-lisp :anaphora :alexandria :cl-csv))

(in-package :cl-csa)

;; (declaim (optimize (speed 3)))

;;; random -------------------------------------------------------
(defun get-random-val (&rest args)
  (assert (<= (length args) 2))
  (let* ((u-bound (or (and (cdr args) (second args))
                      (car args)))
         (l-bound (or (and (cdr args) (car args))
                      0))
         (base (abs l-bound))
         (max (- u-bound l-bound)))
    (- (random (coerce max 'double-float))
       base)))

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
           :type list
           :initform nil)
   (id :accessor c-id
       :initarg :id
       :type integer
       :initform -1)))

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
  (* (expt (+ (random (- 1 double-float-epsilon))
              double-float-epsilon)
           (- (/ 1 lambda-val)))
     alpha-val
     (- (+ (random (- 1 double-float-epsilon))
           double-float-epsilon)
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
  (awhen (aref *cuckoos* 0)
    (values (c-params it) (cuckoo-value it))))

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

;;; x => [-100; 100]
;;; d = 50
(defun sphere (&rest xs)
  (reduce #'+ (mapcar (lambda (x) (expt x 2)) xs)))

;;; x => [-32.768; 32.768]
;;; d = 20
(defun ackley-function (&rest args)
  (let ((a 20) (b 0.2)
        (c (* 2 pi))
        (d (length args)))
    (- (+ (exp 1) a)
       (* a (exp (expt (/ (apply #'sphere args)
                          d)
                       (- (/ 1 b)))))
       (exp (/ (loop for x in args
                  sum (cos (* c x)))
               d)))))

;;; x => [-600; 600]
;;; d = 50
(defun griewank-function (&rest args)
  (1+
   (- (loop for x in args
         sum (/ (expt x 2) 4000))
      (let ((p 1))
        (loop
           for i from 1 to (1- (length args))
           for x in args
           do
             (setf p (* p (cos (/ x (expt i 0.5))))))
        p))))

;;; x => [-5.12; 5.12]
;;; d = 30
(defun rastrigin-function (&rest args)
  (+ (* 10 (length args))
     (loop
        for x in args
        sum (- (expt x 2) (* 10 (cos (* 2 pi x)))))))

;;; x => [-5; 10]
;;; d = 30
(defun rosenbrock-function (&rest args)
  (let ((params (make-array (length args)
                            :initial-contents args)))
    (labels ((%nth (n)
               (aref params n)))
      (loop
         for i from 0 to (- (length args) 2)
         sum (+ (* (expt (- (%nth (1+ i)) (expt (%nth i) 2))
                         2)
                   100)
                (expt (1- (%nth i)) 2))))))


;;; tests ---------------------------------------------------------------
(defun cuckoo-search-aux (func val-gen c-count iter-count)
  (cuckoo-search (make-instance 'cuckoo-search-spec
                                :func (make-instance 'cuckoo-func
                                                     :func func
                                                     :value-gen val-gen)
                                :cuckoos-count c-count
                                :a-nest-count (round (/ c-count) 4)
                                :max-gen iter-count)))

(defparameter *all-runs* (make-hash-table :test #'equal))

(defmacro def-run (name (&rest params) &body body)
  `(progn
     (defun ,name (,@params)
       (if *testing*
           (progn ,@body)
           (handler-case (progn ,@body)
             (error (e)
               (declare (ignore e))
               0))))
     (setf (gethash (string ',name) *all-runs*) #',name)))

(def-run sphere-run (&optional (c-count 200) (iter-count 500))
  (cuckoo-search-aux #'sphere
                     (lambda () (get-random-list 20 -100 100))
                     c-count iter-count))

(def-run ackley-run (&optional (c-count 200) (iter-count 500))
  (cuckoo-search-aux #'ackley-function
                     (lambda () (get-random-list 10 -32.768 32.768))
                     c-count iter-count))

(def-run griewank-run (&optional (c-count 200) (iter-count 500))
  (cuckoo-search-aux #'griewank-function
                     (lambda () (get-random-list 10 -600 600))
                     c-count iter-count))

(def-run rastrigin-run (&optional (c-count 200) (iter-count 500))
  (cuckoo-search-aux #'rastrigin-function
                     (lambda () (get-random-list 10 -5.12 5.12))
                     c-count iter-count))

(def-run rosenbrock-run (&optional (c-count 200) (iter-count 500))
  (cuckoo-search-aux #'rosenbrock-function
                     (lambda () (get-random-list 10 -5 10))
                     c-count iter-count))

(defmacro test-aux (&body func)
  `(time
    (multiple-value-bind (params val) (progn ,@func)
      (format t "params:~%~A~%val: ~A~%" params val))))

(defparameter *all-tests* (make-hash-table :test #'equal))

(defparameter *testing* t)

(defmacro def-test (name (&rest params) &body body)
  `(progn
     (defun ,name (,@params)
       (if *testing*
           (progn ,@body)
           (handler-case (progn ,@body)
             (error (e)
               (declare (ignore e))
               (format t "ERROR!~%")))))
     (setf (gethash (string ',name) *all-tests*) #',name)))

(def-test sphere-test ()
  (test-aux (sphere-run 200 1000)))

(def-test ackley-test ()
  (test-aux (ackley-run 400 1000)))

(def-test griewank-test ()
  (test-aux (griewank-run 600 1000)))

(def-test rastrigin-test ()
  (test-aux (rastrigin-run 800 1000)))

(def-test rosenbrock-test ()
  (test-aux (rosenbrock-run 1000 1000)))

(defun run-all-tests ()
  (format t "~%=====================================~%")
  (maphash (lambda (name func)
             (format t "Running ~A~%" name)
             (funcall func)
             (format t "-------------------------------------~%"))
           *all-tests*)
  (format t "=====================================~%"))

(defun stat-all (&optional (dump-folder (multiple-value-bind
                                              (second minute hour date month year)
                                            (get-decoded-time)
                                          (format nil "~A/cuckoo_run__~a_~a_~a__~2d_~2d_~2d/"
                                                  (sb-unix::posix-getenv "HOME")
                                                  month date year
                                                  hour minute second))))
  (ensure-directories-exist dump-folder)
  (maphash (lambda (name run)
             (with-open-file (csv (format nil "~A/~A.csv" dump-folder name)
                                  :if-exists :supersede
                                  :direction :output
                                  :if-does-not-exist :create)
               (let ((res))
                 (loop
                    for i from 100 to 2000
                    when (= (rem i 100) 0)
                    do
                      (multiple-value-bind (params val) (funcall run i)
                        (declare (ignore params))
                        (push (list i val) res)))
                 (write-csv (append '(("cuckoos" "value")) (reverse res))
                            :stream csv))))
           *all-runs*))
