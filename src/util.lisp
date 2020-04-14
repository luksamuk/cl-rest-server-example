(in-package #:rest-server.util)

(defmacro agetf (key alist)
  `(cdr (assoc ,key ,alist)))
