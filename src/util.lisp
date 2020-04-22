(in-package #:rest-server.util)

(defmacro agetf (key alist)
  "Retrieves a value from ALIST which is under a
certain KEY.

Returns the associated value or NIL if not found."
  `(cdr (assoc ,key ,alist)))

(defmacro route-validate-json (payload)
  "Validates a JSON payload from a network route.

If the payload is not a valid JSON object,
automatically yields a 400 error with body
'Malformed JSON' on the route."
  (let ((payload-sym (gensym)))
    `(let ((,payload-sym ,payload))
       (handler-case (json:decode-json-from-string
                      ,payload-sym)
         (error (e)
           (declare (ignore e))
           (http-condition 400 "Malformed JSON: ~a" ,payload-sym))))))

(defun symbol->keyword (symbol)
  "Transforms a specific SYMBOL into a keyword."
  (unless (symbolp symbol)
    (error "~a is not of type SYMBOL" symbol))
  (intern (format nil "~a" symbol) :keyword))

(defun class-table-p (class)
  "Tests whether a given CLASS is declared as a
table for the database, regardless if it exists
on the database or not.

CLASS can either be a symbol for the class or
the class itself, resolved by using the
FIND-CLASS function."
  (let ((class (if (typep class 'symbol)
                   (find-class class)
                   class)))
    (typep class 'mito.dao.table:dao-table-class)))

(defun table-get-raw-columns (class)
  "Retrieves all the valid columns from a given
CLASS, as class slots.

CLASS can either be a symbol for the class or
the class itself, resolved by using the
FIND-CLASS function."
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
  "Returns all the columns from a given CLASS,
as a list of keywords.

CLASS can either be a symbol for the class or
the class itself, resolved by using the
FIND-CLASS function."
  (mapcar #'symbol->keyword
          (table-get-raw-columns class)))

(defparameter *non-register-columns*
  '(:created-at :updated-at :id)
  "List of keyword columns which are not considered
when creating a database entity from scratch.")

(defun table-get-lispy-register-columns (class)
  "Returns all the columns from a given CLASS,
as a list of keywords, removing the ones that
are not required for creating a new entity from
scratch.

CLASS can either be a symbol for the class or
the class itself, resolved by using the
FIND-CLASS function."
  (remove-if (lambda (slot)
               (member slot *non-register-columns* :test #'eql))
             (table-get-lispy-columns class)))

(defun table-get-string-columns (class)
  "Returns all the columns from a given CLASS,
as a list of strings.

CLASS can either be a symbol for the class or
the class itself, resolved by using the
FIND-CLASS function."
  (mapcar (lambda (x) (string-downcase (format nil "~a" x)))
          (table-get-lispy-columns class)))

(defun table-get-string-register-columns (class)
  "Returns all the columns from a given CLASS,
as a list of strings, removing the ones that are
not required for creating a new entity from scratch.

CLASS can either be a symbol for the class or
the class itself, resolved by using the
FIND-CLASS function."
  (mapcar (lambda (x) (string-downcase (format nil "~a" x)))
          (table-get-lispy-register-columns class)))

(defun post-valid-data-p (class data)
  "Tests whether some DATA received from a POST
request is valid for creating an entity of a specific
table CLASS.

DATA must be an alist of values, and CLASS must be
one of the declared tables for the application."
  (let ((fields (table-get-lispy-register-columns class)))
    (loop for field in data
       always (and (consp field)
                   (stringp (cdr field))
                   (member (car field) fields)))))

(defun dao->alist (dao)
  "Takes an entity DAO and turns it into an alist.

DAO must be a valid entity.

The returned alist is a list of CONS pairs, where
CAR is a keyword identifier for a field, and CDR
is the value itself."
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
  "Filters the fields from ALIST, given the
CENSORED-KEYS.

Returns a new alist, removing the fields which
keyword keys are in CENSORED-KEYS."
  (loop for element in alist
     unless (member (car element)
                    censored-keys
                    :test #'equal)
     collect element))

(defparameter *censored-dao-fields*
  '(:id :created-at :updated-at :pass)
  "Lists fields which are not supposed to be show
to someone attempting to retrieve a field.")

(defun dao->filtered-alist (dao)
  "Takes an entity DAO and turns it into a
filtered alist, removing fields which are censored
for the end-user.

DAO must be a valid entity.

The returned alist is a list of CONS pairs, where
CAR is a keyword identifier for a field, and CDR
is the value itself."
  (filter-alist (dao->alist dao)
                *censored-dao-fields*))

(defun dao->json (dao)
  "Takes an entity DAO and turns it into a
JSON-formatted string, less the fields which are
censored for the end-user.

DAO must be a valid entity.

The returned JSON is formatted as an object,
where the keys are string identifiers for fields,
and the associated values are the expected values
themselves."
  (json:encode-json-to-string
   (dao->filtered-alist dao)))
