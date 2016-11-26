(defpackage :cuckoo-search
  (:nicknames :c-search))


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
(defvar *nests* nil)

(defclass cuckoo-func ()
  ((func :reader cf-func
         :initarg :func)
   (l-bound :reader cf-l-bound
            :initarg :l-bound)
   (u-bound :reader cf-u-bound
            :initarg :u-bound)
   (cmp-func :reader cf-cmp-func
             :initarg :cmp-func)
   (params-count :reader cf-p-count
                 :initarg :p-count
                 :type integer)
   (value-gen :reader cf-value-gen
              :initarg :value-gen)))

(defclass lambda-spec ()
  ((l-bound :accessor ls-l-bound
            :initarg :l-bound
            :initform 1)
   (u-bound :accessor ls-u-bound
            :initarg :u-bound
            :initform 3)))

(defclass cuckoo-search-spec ()
  ((func :reader css-func
         :initarg :func
         :type cuckoo-func)
   (lambda-spec :reader css-lambda-spec
                :initarg :lambda-spec
                :type 'lambda-spec
                :initform (make-instance 'lambda-spec))
   (nests-count :reader css-nests-count
                :initarg :nests-count)
   (cuckoos-count :reader css-cuckoos-count
                  :initarg :cuckoos-count)
   (a-nests-count :reader css-a-nests-count
                  :initarg :a-nest-count)
   (max-generation :reader css-max-gen
                   :initarg :max-gen)
   (stop-pred :reader css-stop-pred
              :initarg :stop-pred
              :initform (constantly nil))))

;;; nest ---------------------------------------------------------
(defclass nest ()
  ((params :accessor n-params
           :initarg :params
           :type list)))
(defgeneric nest-value (nest))

(defmethod print-object ((obj nest) stream)
  (format stream "#<NEST ~A;>" (nest-value obj)))

(defmethod nest-value ((nest nest))
  (apply (cf-func (css-func *cuckoos-search-spec*)) (n-params nest)))

(defmethod nests-cmp ((nest1 nest) (nest2 nest))
  "t - when nest1 is better then nest2."
  (funcall (cf-cmp-func (css-func *cuckoos-search-spec*)) (nest-value nest1) (nest-value nest2)))


;;; cuckoo ------------------------------------------------------
(defclass cuckoo ()
  ((nest-id :accessor c-nest-id
            :initarg :nest-id
            :type integer)
   (id :accessor c-id
       :initarg :id
       :type integer)))
(defgeneric get-lambda (cuckoo))
(defgeneric get-a (cuckoo))
(defgeneric get-nest (cuckoo))

(defmethod print-object ((obj cuckoo) stream)
  (format stream "#<CUCKOO ~A; ~A; ~A>" (c-id obj) (c-nest-id obj) (nest-value (get-nest obj))))

(defmethod get-nest ((c cuckoo))
  (aref *nests* (c-nest-id c)))

(defmethod get-lambda ((c cuckoo))
  )

(defmethod get-a ((c cuckoo))
  )

;;; initial functions -------------------------------------------
(defun generate-initial-nests ()
  (let* ((n (css-nests-count *cuckoos-search-spec*))
         (res (make-array n)))
    (loop for i from 0 to (1- n)
       do (setf (aref res i) (make-instance 'nest :params (funcall
                                                           (cf-value-gen
                                                            (css-func *cuckoos-search-spec*))))))
    (sort-nests res)))

(defun generate-initial-cuckoos ()
  (let* ((c-count (css-cuckoos-count *cuckoos-search-spec*))
         (n-count (css-nests-count *cuckoos-search-spec*))
         (res (make-array c-count)))
    (loop for i from 0 to (1- c-count)
       do (setf (aref res i) (make-instance 'cuckoo
                                            :nest-id (random n-count)
                                            :id i)))
    (sort-cuckoos res)))

(defun sort-cuckoos (c-array)
  (loop
     for c across (sort c-array #'nests-cmp :key #'get-nest)
     for i from 0 to (1- (length c-array))
     do (setf (c-id c) i))
  c-array)

(defun sort-nests (n-array)
  (sort n-array #'nests-cmp))

;;; main functions ----------------------------------------------
(defun cuckoo-search (cuckoo-spec)
  (let* ((*cuckoos-search-spec* cuckoo-spec)
         (*nests* (generate-initial-nests))
         (*cuckoos* (generate-initial-cuckoos)))
    (cuckoo-search-inner)))

(defun cuckoo-search-inner ()
  (loop for gen from 0 to (css-max-gen *cuckoos-search-spec*)
     when (funcall (css-stop-pred *cuckoos-search-spec*))
     do (cuckoo-step))
  (n-params (aref *nests* 0)))

(defun cuckoo-step ()
  )

(print
 (cuckoo-search (make-instance 'cuckoo-search-spec
                               :func (make-instance 'cuckoo-func
                                                    :func (lambda (&rest xs)
                                                            (reduce (lambda (acc x)
                                                                      (+ acc (* x x)))
                                                                    xs))
                                                    :p-count 50
                                                    :l-bound -100
                                                    :u-bound 100
                                                    :cmp-func #'<
                                                    :value-gen (lambda () (get-random-list 50 -100 100)))
                               :nests-count 100
                               :cuckoos-count 10
                               :a-nest-count 10
                               :max-gen 100)))
