(in-package #:rest-server.util)

(defmacro agetf (key alist)
  `(cdr (assoc ,key ,alist)))

(defmacro route-validate-json (payload)
  (let ((payload-sym (gensym)))
    `(let ((,payload-sym ,payload))
       (handler-case (json:decode-json-from-string
                      ,payload-sym)
         (error (e)
           (declare (ignore e))
           (http-condition 400 "Malformed JSON: ~a" ,payload-sym))))))

(defun symbol->keyword (symbol)
  (unless (symbolp symbol)
    (error "~a is not of type SYMBOL" symbol))
  (intern (format nil "~a" symbol) :keyword))

(defun class-table-p (class)
  (let ((class (if (typep class 'symbol)
                   (find-class class)
                   class)))
    (typep class 'mito.dao.table:dao-table-class)))

(defun table-get-raw-columns (class)
  (unless (class-table-p class)
    (error "~a is not a table class" class))
  (let* ((class (if (typep class 'symbol)
                   (find-class class)
                   class))
         (all-cols
          (mapcar #'closer-mop:class-direct-slots
                 (cons class
                       (closer-mop:class-direct-superclasses
                        class)))))
    (mapcar #'closer-mop:slot-definition-name
            (remove-if-not
             (lambda (slot)
               (typep slot
                      'mito.dao.column:dao-table-column-class))
             (alexandria:flatten all-cols)))))

(defun table-get-lispy-columns (class)
  (mapcar #'symbol->keyword
          (table-get-raw-columns class)))

(defparameter *non-register-columns*
  '(:created-at :updated-at :id))

(defun table-get-lispy-register-columns (class)
  (remove-if (lambda (slot)
               (member slot *non-register-columns* :test #'eql))
             (table-get-lispy-columns class)))

(defun table-get-string-columns (class)
  (mapcar (lambda (x) (string-downcase (format nil "~a" x)))
          (table-get-lispy-columns class)))

(defun table-get-string-register-columns (class)
  (mapcar (lambda (x) (string-downcase (format nil "~a" x)))
          (table-get-lispy-register-columns class)))

(defun post-valid-data-p (class data)
  (let ((fields (table-get-lispy-register-columns class)))
    (loop for field in data
       always (and (consp field)
                   (stringp (cdr field))
                   (member (car field) fields)))))

(defun dao->alist (dao)
  (let ((class (type-of dao)))
    (loop for field in (util:table-get-lispy-columns class)
       for getter-sym = (case field
                          (:id 'mito:object-id)
                          (:created-at 'mito:object-created-at)
                          (:updated-at 'mito:object-updated-at)
                          (otherwise
                           (intern (string-upcase
                                    (concatenate 'string
                                                 (format nil "~a" class)
                                                 "-"
                                                 (format nil "~a" field)))
                                   :rest-server.db)))
       collect (cons field (funcall getter-sym dao)))))

(defun filter-alist (alist censored-keys)
  (loop for element in alist
     unless (member (car element) censored-keys :test #'equal)
     collect element))

(defparameter *censored-dao-fields*
  '(:id :created-at :updated-at :pass))

(defun dao->filtered-alist (dao)
  (filter-alist (dao->alist dao)
                *censored-dao-fields*))

(defun dao->json (dao)
  (json:encode-json-to-string
   (dao->filtered-alist dao)))
