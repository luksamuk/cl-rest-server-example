(in-package #:rest-server.db)

(defparameter *jwt-method* :hs256)

(defparameter *jwt-salt*
  (ironclad:hex-string-to-byte-array
   "04f79366645b309340cf5c8c308e780c6db9287d9bdc7664d96649"))

(defparameter *jwt-expires-in* '((:days . 7)))

(defun gen-jwt-creation-time ()
  "Retrieve current time as time difference between now
and January 1st, 1970, in miliseconds. The date is
retrieved in seconds and then multiplied by 1000 for
JavaScript conformance."
  (-> (get-universal-time)
      (- (encode-universal-time 0 0 0 1 1 1970 0))
      (* 1000)))

(defun dbg-format-jwt-time (time)
  "Takes a time generated for any claim of a JWT and
formats it into a string, for debug purposes."
  (let ((time (-> (encode-universal-time 0 0 0 1 1 1970 0)
                  (* 1000)
                  (+ time))))
    (multiple-value-bind
          (sec min hour date month year day daylight-p zone)
        (decode-universal-time (floor time 1000))
      (declare (ignore day daylight-p))
      (format nil "~2,'0d/~2,'0d/~4,'0d ~2,'0d:~2,'0d:~2,'0d+~d"
              month date year hour min sec zone))))

(defun gen-jwt-expiration-time (creation-time params)
  "Takes a creation time in miliseconds specially generated
for JWT and returns its expiration date. The parameters must
be an alist where each pair specifies a time period and the
associated value must be a number."
  (loop for pair in params
     sum (* (cdr pair) ; lets hope sbcl constantfolds this
            (case (car pair)
              ((:years)   (* 365 24 60 60 1000))
              ((:months)  (* 30 24 60 60 1000))
              ((:days)    (* 24 60 60 1000))
              ((:hours)   (* 60 60 1000))
              ((:minutes) (* 60 1000))
              ((:seconds) 1000)
              (t 1)))
     into extra-ms
     finally (return (+ creation-time extra-ms))))

(defun gen-session-data (extra-data)
  "Generates an alist containing a JWT with issue
time and expiration time, plus some extra data that
is also signed and inserted into the token itself."
  (let* ((creation-time (gen-jwt-creation-time))
         (expiry-time   (gen-jwt-expiration-time
                         creation-time
                         *jwt-expires-in*))
         (extra-data (append extra-data
                             `(("iat" . ,creation-time)
                               ("exp" . ,expiry-time)))))
    (append extra-data
            `(("token" .
                       ,(jose:encode *jwt-method*
                                     *jwt-salt*
                                     extra-data))))))

(defun jwt-valid-p (token)
  (let* ((token-data
          (handler-case 
              (jose:decode *jwt-method*
                           *jwt-salt*
                           token)
            (jose/errors:jws-verification-error (e)
              (declare (ignore e))
              nil)))
         (iat (cdr (assoc "iat" token-data :test #'equal)))
         (exp (cdr (assoc "exp" token-data :test #'equal))))
    (if (not token-data)
        (values nil nil)
        (values
         (and (numberp iat)
              (numberp exp)
              (let ((diff (- exp iat))
                    (currt (gen-jwt-creation-time)))
                (not (minusp diff))
                (not (zerop diff))
                (< iat exp)
                (< iat currt)
                (> exp currt)))
         token-data))))

(defun request-authorized-p (bearer-token)
  (let ((parts (split-sequence #\space bearer-token)))
    (and (= 2 (length parts))
         (string= (first parts) "Bearer")
         (multiple-value-bind (result token-data)
             (jwt-valid-p (second parts))
           (or (and result (values t token-data))
               (values nil nil))))))

(defmethod control-index :around (type &optional params)
  (declare (ignore type params))
  (if (->> ningle:*request*
              lack.request:request-headers
              (gethash "authorization")
              request-authorized-p
              not)
      (util:http-response (401)
        "Authentication token is invalid.")
      (call-next-method)))

(defmethod control-show :around (type &optional params)
  (declare (ignore type params))
  (if (->> ningle:*request*
              lack.request:request-headers
              (gethash "authorization")
              request-authorized-p
              not)
      (util:http-response (401)
        "Authentication token is invalid.")
      (call-next-method)))

(defmethod control-store :around (type &optional params)
  (declare (ignore params))
  (cond ((eql type :session)
         (call-next-method))
        ((->> ningle:*request*
              lack.request:request-headers
              (gethash "authorization")
              request-authorized-p
              not)
         (util:http-response (401)
           "Authentication token is invalid."))
        (t (call-next-method))))

(defmethod control-update :around (type &optional params)
  (declare (ignore type params))
  (if (->> ningle:*request*
              lack.request:request-headers
              (gethash "authorization")
              request-authorized-p
              not)
      (util:http-response (401)
        "Authentication token is invalid.")
      (call-next-method)))

(defmethod control-delete :around (type &optional params)
  (declare (ignore type params))
  (if (->> ningle:*request*
              lack.request:request-headers
              (gethash "authorization")
              request-authorized-p
              not)
      (util:http-response (401)
        "Authentication token is invalid.")
      (call-next-method)))
