
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
            :initform 0.01)
   (u-bound :reader as-u-bound
            :initarg :u-bound
            :initform 1)
   (k :reader as-k
      :initarg :k
      :initform 10)))

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
(defgeneric nest-fitness (nest))

(defmethod print-object ((obj nest) stream)
  (format stream "#<NEST ~A;>" (nest-value obj)))

(defmethod nest-value ((nest nest))
  (apply (cf-func (css-func *cuckoos-search-spec*)) (n-params nest)))

(defmethod nest-fitness ((nest nest))
  (/ 1 (1+ (nest-value nest))))


;;; cuckoo ------------------------------------------------------
(defclass cuckoo ()
  ((nest-id :accessor c-nest-id
            :initarg :nest-id
            :type integer)
   (id :accessor c-id
       :initarg :id
       :type integer)))
(defgeneric get-lambda (cuckoo))
(defgeneric get-alpha (cuckoo iteration))
(defgeneric get-nest (cuckoo))
(defgeneric levy-flight (cuckoo gen))
(defgeneric cuckoo-fitness (cuckoo))

(defmethod print-object ((obj cuckoo) stream)
  (format stream "#<CUCKOO ~A; ~A; ~A>" (c-id obj) (c-nest-id obj) (nest-value (get-nest obj))))

(defmethod get-nest ((c cuckoo))
  (aref *nests* (c-nest-id c)))

(defmethod cuckoo-fitness ((c cuckoo))
  (nest-fitness (get-nest c)))

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
         (k (as-k a-spec)))
    (* a-max (expt (/ a-min a-max) (/ iter k)))))

(defun levy-random (lambda-val alpha-val)
  (* (expt (1+ (random 1000))
           (- (/ 1 lambda-val)))
     alpha-val
     (- (1+ (random 1000))
        0.5)))

(defmethod levy-flight ((c cuckoo) gen)
  (let* ((cur-nest (get-nest c))
         (vals (n-params cur-nest))
         (alpha-val (get-alpha c gen))
         (lambda-val (get-lambda c))
         (levy-val (levy-random lambda-val alpha-val)))
    (make-instance 'nest
                   :params (mapcar (lambda (x) (+ x levy-val)) vals))))

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
     for c across (sort c-array #'> :key #'cuckoo-fitness)
     for i from 0 to (1- (length c-array))
     do (setf (c-id c) i))
  c-array)

(defun sort-nests (n-array)
  (sort n-array #'> :key #'nest-fitness))

;;; main functions ----------------------------------------------
(defun cuckoo-search (cuckoo-spec)
  (let* ((*cuckoos-search-spec* cuckoo-spec)
         (*nests* (generate-initial-nests))
         (*cuckoos* (generate-initial-cuckoos)))
    (cuckoo-search-inner)))

(defun cuckoo-search-inner ()
  (loop for gen from 0 to (css-max-gen *cuckoos-search-spec*)
     when (not (funcall (css-stop-pred *cuckoos-search-spec*)))
     do (cuckoo-step gen))
  (nest-value (aref *nests* 0)))

(defun refresh-nests ()
  (let ((sorted-nests (sort-nests *nests*)))
    (loop for i from (- (length sorted-nests) (css-a-nests-count *cuckoos-search-spec*))
       to (1- (length sorted-nests))
       do (setf (aref sorted-nests i)
                (make-instance 'nest :params (funcall
                                              (cf-value-gen
                                               (css-func *cuckoos-search-spec*))))))
    (setf *nests* sorted-nests)))

(defun refresh-cuckoos ()
  (setf *cuckoos* (sort-cuckoos *cuckoos*)))

(defun cuckoo-step (gen)
  (loop for c across *cuckoos*
     do (let* ((new-nest (levy-flight c gen))
               (nest-id (random (css-nests-count *cuckoos-search-spec*)))
               (rand-nest (aref *nests* nest-id)))
          ;; (format t "ITERATION: ~A~%NEW: ~A~%OLD: ~A~%~%" gen new-nest rand-nest)
          (when (> (nest-fitness new-nest) (nest-fitness rand-nest))
            (setf (aref *nests* nest-id) new-nest))))
  (refresh-nests)
  (refresh-cuckoos))

(defun sphere (&rest xs)
  (reduce #'+ (mapcar (lambda (x) (expt x 2)) xs)))

(defun test ()
  (profile sort-nests sort-cuckoos levy-flight)
  (time
   (let ((res (cuckoo-search (make-instance 'cuckoo-search-spec
                                            :func (make-instance 'cuckoo-func
                                                                 :func #'sphere
                                                                 :value-gen (lambda () (get-random-list 50 -100 100)))
                                            :nests-count 200
                                            :cuckoos-count 100
                                            :a-nest-count 100
                                            :max-gen 1000))))
     (format t "~%Y: ~A~%~%" res)))
  (report)
  (unprofile sort-nests sort-cuckoos levy-flight))
