(in-package #:rest-server.util)

(defmacro agetf (key alist)
  "Retrieves a value from ALIST which is under a
certain KEY.

Returns the associated value or NIL if not found."
  `(cdr (assoc ,key ,alist)))

(defmacro route-prepare-response (response-object
                                  &optional
                                    (http-code 200)
                                    (type "application/json"))
  `(progn
     (setf (lack.response:response-headers ,response-object)
           (append
            (lack.response:response-headers ,response-object)
            (list :content-type ,type)))
     (setf (lack.response:response-status ,response-object)
           ,http-code)))

(defmacro http-response ((&optional (http-code 200))
                         &body body)
  `(progn (route-prepare-response ningle:*response*
                                  ,http-code
                                  "application/json")
          (json:encode-json-to-string
           ,(cond ((null body)
                   `(list '(:message . "OK")))
                  ((consp (first body))
                   (first body)) ; todo: subformats
                  ((and (stringp (first body))
                        (= (length body) 1))
                   `(list '(:message . ,(first body))))
                  (t
                   `(list
                     (cons :message
                           (format nil ,@body))))))))

(defun symbol->keyword (symbol)
  "Transforms a specific SYMBOL into a keyword."
  (unless (symbolp symbol)
    (error "~a is not of type SYMBOL" symbol))
  (intern (format nil "~a" symbol) :keyword))

(defun string->keyword (string)
    "Transforms a specific STRING into a keyword.
The string is trimmed and transformed to uppercase."
  (unless (stringp string)
    (error "~a is not of type STRING" string))
  (intern (->> string
               (string-trim '(#\Space #\Return))
               string-upcase)
          :keyword))

(defun restructure-alist (alist)
  "Restructures an ALIST (possibly received
by POST request into a proper alist.

Every key in the ALIST is converted from
string to keyword."
  (loop for (a . b) in alist
     collect (cons (string->keyword a) b)))

(defun get-payload (request)
  "Takes a Ningle REQUEST object and
retrieves its payload (body parameters), as
a restructured alist fitting the rest of the
application."
  (restructure-alist
   (lack.request:request-body-parameters request)))

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
                    class)))
    (->> class
         closer-mop:class-direct-superclasses
         (cons class)
         (mapcar #'closer-mop:class-direct-slots)
         alexandria:flatten
         (remove-if-not
          (lambda (slot)
            (typep slot
                   'mito.dao.column:dao-table-column-class)))
         (mapcar #'closer-mop:slot-definition-name))))

(defun table-get-lispy-columns (class)
  "Returns all the columns from a given CLASS,
as a list of keywords.

CLASS can either be a symbol for the class or
the class itself, resolved by using the
FIND-CLASS function."
  (mapcar #'symbol->keyword
          (table-get-raw-columns class)))

(defparameter *non-register-columns*
  '(:created-at :updated-at :id :password-hash :password-salt)
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

(defun post-valid-data-p (class data &key (has-password nil))
  "Tests whether some DATA received from a POST
request is valid for creating an entity of a specific
table CLASS.

HAS-PASSWORD determines whether this data requires password
authentication. If so, this predicate obligatorily checks for
presence of a :password field.

DATA must be an alist of values, and CLASS must be
one of the declared tables for the application."
  (let ((fields (append
                 (table-get-lispy-register-columns class)
                 (if has-password '(:password) nil))))
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
       for getter-sym =
         (case field
           (:id 'mito:object-id)
           (:created-at    'mito:object-created-at)
           (:updated-at    'mito:object-updated-at)
           (:password-hash 'mito-auth:password-hash)
           (:password-salt 'mito-auth:password-salt)
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
  '(:created-at :updated-at :password-hash :password-salt)
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
  (->> *censored-dao-fields*
       (filter-alist (dao->alist dao))))

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
