;;;; file: http.lisp
;;;; author: Alexander Schreiber <als@thangorodrim.de>
;;;; 
;;;; Web server implementation on top of lisp-network-server
;;;;
;;;; Copyright (C) 2006 Alexander Schreiber
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Library General Public
;;;; License as published by the Free Software Foundation;
;;;; version 2 of the License.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Library General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Library General Public
;;;; License along with this library; if not, write to the Free
;;;; Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;;
;;;; author : Alexander Schreiber <als@thangorodrim.de>
;;;; version: $Id$
;;;;
;;;;


(defpackage :lns.http
  (:use :common-lisp)
  (:documentation
   "Web server implementation on top of LNS.")
  (:export #:start-server
           #:stop-server
           #:create-server
           #:register-server
           #:unregister-server
           #:register-server-host-name
           #:unregister-server-host-name
           #:register-url-handler
           #:unregister-url-handler
           #:http-set-status-code
           #:http-set-content-type
           #:http-add-headers
           #:http-send-headers
           #:html-escape
           #:request-start-time
           #:request-stream
           #:request-method
           #:request-host
           #:request-uri
           #:request-path
           #:request-query
           #:request-arguments
           #:request-protocol
           #:request-headers
           #:request-server-ip
           #:request-server-port
           #:request-client-ip
           #:request-client-port
           #:get-iso8601-timestamp
           #:get-rfc1123-timestamp
           #:log-error
           #:*server-version*
           #:get-mime-type
           #:reset-connection-timeout
           #:url-handler-server-info
           #:url-handler-request-info
           #:url-handler-file
           #:url-encode-string
           #:start-framework
           ))


(in-package :lns.http)


;; type definitions
(defstruct request
  "Structure describing an incoming request."
  (start-time  nil)             ; request start time
  (stream      nil)             ; the incoming TCP stream
  (method      nil)             ; request method
  (host        nil)             ; the request host
  (uri         nil)             ; the uri supplied on the request line
  (path        nil)             ; the absolute path computed from the uri
  (query       nil)             ; the query part of the uri
  (arguments   '())             ; association list of query arguments
  (protocol    nil)             ; protocol version supplied on request line
  (headers     '())             ; association list of HTTP headers
  (server-ip   nil)             ; IP of connection on server end
  (server-port nil)             ; port of connection on server end
  (client-ip   nil)             ; IP of client
  (client-port nil))            ; remote port on client


(defstruct server
  "Structure describing an HTTP server instance."
  (name           nil)          ; name of this server instance, must be unique
  (ip             nil)          ; IP address to bind the server to
  (port           nil)          ; port to bind the server to
  (active         nil)          ; is this instance active?
  (request-count  0)            ; number of requests served
  (host-names   (make-hash-table :test #'equal))) ; active server names


;; globals

;; these four state variables are shadowed dynamically via (let ..) early
;; on in order to keep the state stored in there strictly thread-
;; (and thereby request-) local
(defvar *http-response-headers* '() "HTTP response headers for this request")
(defvar *http-headers-sent* nil "Have we already sent the headers?")
(defvar *http-status-code* 200 "HTTP status code, a bit optimistic")
(defvar *http-content-type* "text/html" "HTTP content type")


(defvar *server-version* "lns.http 0.2" "Software version of the server")


(defvar *supported-http-methods*
  '("OPTIONS" "GET" "HEAD" "POST" "PUT" "DELETE" "TRACE")
  "HTTP methods we currently support in general")

(defvar *supported-http-protocol-versions*
  '("HTTP/0.9" "HTTP/1.0" "HTTP/1.1")
  "Version identifiers of the HTTP protocol we accept in general")


;; our log file
;; currently, only a single global log file is supported, but
;; that will be changed later to one logfile per server-instance
(defvar *http-log-file* "http-requests.log" "Global LNS/HHTP log")
(defvar *http-log-file-mutex* (sb-thread:make-mutex :name "log file path"
                                                    :value nil)
  "Mutex protection for log file path variable")
(defvar *http-log-file-handle* nil "The open log file")
(defvar *http-log-file-handle-mutex*
  (sb-thread:make-mutex :name "HTTP log file handle mutex"
                        :value nil)
  "Protect the file handle")

;; the same for errors
(defvar *http-error-log-file* "http-error.log" "Global LNS/HHTP error log")
(defvar *http-error-log-file-mutex* (sb-thread:make-mutex :name
                                                         "error log file path"
                                                    :value nil)
  "Mutex protection for error log file path variable")
(defvar *http-error-log-file-handle* nil "The open log file")
(defvar *http-error-log-file-handle-mutex*
  (sb-thread:make-mutex :name "HTTP errors log file handle mutex"
                        :value nil)
  "Protect the file handle")

;; *servers* keys on the server-name, which can be any string, but
;; MUST be unique
(defvar *servers* (make-hash-table :test #'equal) "server instances")
(defvar *servers-mutex* (sb-thread:make-mutex :name  "servers instances lock"
                                              :value nil)
  "Mutex protection for access to the servers configuration") 

;; trace log, currently only used with trace-time to write the
;; output of (time ...) to a file so you can get performance data
(defvar *trace-log* #P"http-trace.log" "trace log for (time ...)")
(defvar *trace-log-mutex* (sb-thread:make-mutex :name  "trace file path lock"
                                              :value nil)
  "Mutex protection for access to the trace log file") 

(defvar *http-status-code-text* (make-hash-table :test #'equal)
  "Mapping numeriical HTTP status codes to their text messages")


;; those mappings below aren't really variables - after the initial
;; load they won't change, but using defparameter leads to
;; annoying (if correct) complaints when reloading this file
(defvar *day-of-week-to-short-weekday* (make-hash-table :test #'equal)
  "Hash mapping day of week numbers to short weekday names.")
(defvar *month-to-short-monthname* (make-hash-table :test #'equal)
  "Table mapping month numbers to month names.")


(defvar *url-encode-ok-chars* '( #\. #\/) 
  "Characters that are ok in URL-encoding and need not be encoded.
   Note: alphanumerics are automagically assumed to not need encoding.")

(defvar *mime-types-file* #P"/etc/mime.types"
        "where to find MIME type definitions.")

(defvar *file-type-to-mime-type* (make-hash-table :test #'equal)
  "Hash table mapping (lower case) pathname-types to MIME-types.")

(defvar *connection-timer* nil "Timer to terminate the connection on timeout")

(defvar *connection-timeout* 300 "connection timeout in seconds")

;; we have our own log-writer here

(defvar *log-writer-active* nil "Do we have a running logwriter?")
(defvar *log-writer-active-mutex*
  (sb-thread:make-mutex :name "log writer state mutex"
                        :value nil)
  "Protect the log writer state.")

(defvar *log-writer-interval* 30 "sleep this many seconds, flush, repeat")
(defvar *log-writer-interval-mutex*
  (sb-thread:make-mutex :name "log writer state mutex"
                        :value nil)
  "Protect the log writer state.")


;; macro stuff
(defmacro trace-time (form)
  "Runs the supplied form under time, redirecting the output of time
which writes to *trace-output* - at least under SBCL to the file specified
by *trace-log* in the current directory."
  `(let ((*trace-output* (make-string-output-stream))
         (trace-file (sb-thread:with-recursive-lock (*trace-log-mutex*)
                       *trace-log*)))
         
    (format *trace-output* "~a now tracing ~a~%"
     (get-iso8601-timestamp) ',form)
    (time ,form)
    (with-open-file (log
                     trace-file
                     :external-format :utf-8
                     :direction :output
                     :if-exists :append
                     :if-does-not-exist :create)
      (format log "~a~%" (get-output-stream-string *trace-output*)))))
                    


;; utility functions

(defun reset-connection-timeout (&optional (timeout *connection-timeout*))
  "Reset the connection timeout, expects new timeout in seconds. If
timeout is omitted, uses the default value in *connection-timeout*."
  (unwind-protect
       (sb-ext:unschedule-timer *connection-timer*)
    (sb-ext:schedule-timer *connection-timer*
                           timeout)))


(defun get-iso8601-timestamp (&optional universal-time)
    "Returns an ISO8601 timestamp, using either current time or the
  supplied universal time."
    (multiple-value-bind (ss mm hh dd mo yy) ; drop unneeded values
            (decode-universal-time 
                     (if (integerp universal-time)
                                  universal-time
                                             (get-universal-time)))
                (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D"
                                    yy mo dd hh mm ss)))


(defun get-rfc1123-timestamp (&optional universal-time)
  "Returns an RFC822 timestamp, using either current time or the
supplied universal time."
    (multiple-value-bind (ss mm hh dd mo yy dow dst tz)
      (decode-universal-time
       (if (integerp universal-time)
           universal-time
           (get-universal-time)))
    (format nil "~a, ~2,'0D ~a ~4,'0D ~2,'0D:~2,'0D:~2,'0D GMT"
            (gethash dow *day-of-week-to-short-weekday*)
            dd
            (gethash mo *month-to-short-monthname*)
            yy
            (- (+ hh tz) (if dst
                             1
                             0))
            mm
            ss)))



(defun log-request (request)
  "Write the specified request entry to the LNS/HTTP logfile."
  (sb-thread:with-recursive-lock (*http-log-file-handle-mutex*)
    (if (null *http-log-file-handle*)
        (handler-case
            (setf *http-log-file-handle*
                  (open (sb-thread:with-recursive-lock (*http-log-file-mutex*)
                          *http-log-file*)
                        :external-format :utf-8
                        :direction :output
                        :if-exists :append
                        :if-does-not-exist :create))
          (serious-condition (condition)
            (sb-posix:syslog sb-posix:log-err
                             (format nil
                                     "lns.lisp on log ~a open error: ~a"
                                     *http-log-file*
                                     condition))
            (lns:write-log "lns.http failed to open request log")
            (return-from log-request)))) ; no point in continuing
    ;; by now, we can assume the log file to be open

    (handler-case
        (format *http-log-file-handle*
                "~a client ~a requested ~a ~a with protocol ~a~%"
                (get-iso8601-timestamp)
                (request-client-ip request)
                (request-method request)
                (request-uri request)
                (request-protocol request))
      (serious-condition (condition)
        (sb-posix:syslog sb-posix:log-err
                         (format nil
                                 "lns.lisp on log ~a write error: ~a"
                                 *http-log-file*
                                 condition))
        (lns:write-log "lns.http failed to write to request log")))))




(defun log-error (message)
  "Write the specified request entry to the LNS/HTTP error logfile."
  (sb-thread:with-recursive-lock (*http-error-log-file-handle-mutex*)
    (if (null *http-error-log-file-handle*)
        (handler-case
            (setf *http-error-log-file-handle*
                  (open (sb-thread:with-recursive-lock
                            (*http-error-log-file-mutex*)
                          *http-error-log-file*)
                        :external-format :utf-8
                        :direction :output
                        :if-exists :append
                        :if-does-not-exist :create))
          (serious-condition (condition)
            (sb-posix:syslog sb-posix:log-err
                             (format nil
                                     "lns.lisp on error log ~a open error: ~a"
                                     *http-error-log-file*
                                     condition))
            (lns:write-log "lns.http failed to open error log")
            (return-from log-error)))) ; no point in continuing
    ;; by now, we can assume the log file to be open

    (handler-case
        (format *http-error-log-file-handle*
                "~a ~a~%"
                (get-iso8601-timestamp) message)
      (serious-condition (condition)
        (sb-posix:syslog sb-posix:log-err
                         (format nil
                                 "lns.lisp on error log ~a write error: ~a"
                                 *http-error-log-file*
                                 condition))
        (lns:write-log "lns.http failed to write to error log")))))



(defun split-string (source &key (separator #\/))
  "Split the provided string into separate substrings, use separator
as boundary. separator is either a single character or a list of
characters. Returns elements as list of strings."
  (let ((element nil)
        (result nil)
        (sep-list nil))
    
    (if (listp separator)
        (setf sep-list separator)
        (setf sep-list (list separator)))
    
    (dotimes (i (length source))
      (cond ((and (member (char source i) sep-list)
                  (null element)) ; skip
             ())
            ((not (member (char source i) sep-list))
             (setf element 
                   (concatenate 'string element (string 
                                                 (char source i)))))
            ((and (member (char source i) sep-list)
                  (not (null element)))
             (setf result (append result (list element)))
             (setf element nil))))
    (if (not (null element))
        (setf result (append result (list element))))
    
    result))



(defun init-mime-mapping (map-file)
  "Populate the MIME type mapping hash."
  (handler-case
      (with-open-file (mime-map
                       map-file
                       :direction :input
                       :if-does-not-exist :error)
        (let ((line nil)
              (items nil))

          (loop
           (setf line (read-line mime-map nil nil))
           (if (null line)
               (return-from init-mime-mapping))
           (if (< 0 (length line))
               (if (not (char-equal (char line 0) #\#))
                   (progn
                     (setf items (split-string line
                                               :separator '(#\Space #\Tab)))
                     
                     (if (< 1 (length items))
                         (dolist (file-type (rest items))
                           (setf (gethash (string-downcase file-type)
                                          *file-type-to-mime-type*)
                                 (first items))))))))))

    (file-error (condition)
      (log-error (format nil "failed to init MIME mapping: ~a" condition))
      (return-from init-mime-mapping))))



(defun get-mime-type (file-type)
  "Return the MIME type for the provided file-type (file extension). Returns
application/binary-data if no matching MIME type is found."
  (if (gethash (string-downcase file-type)
               *file-type-to-mime-type*)
      (gethash (string-downcase file-type)
               *file-type-to-mime-type*)
      "application/binary-data"))


(defmacro str+ (&rest args)
  "A simple shorthand wrapper for (concatenate 'string ...)"
  `(concatenate 'string ,@args))



(defun chomp (line)
  (declare (type string line))
  "Removes EOL characters from the end of the string, like in Perl."
  (if (null line)
      nil
      (string-right-trim '(#\Newline #\Return) line)))


(defun whitespacep (character)
  (declare (type character character))
  "Predicate checking wether the supplied character is whitespace."
  (cond
    ((char-equal character #\Space) t)
    ((char-equal character #\Newline) t)
    ((char-equal character #\Return) t)
    ((char-equal character #\Tab) t)))


(defun emptylinep (line)
  "Checks wether the supplied argument is an empty line which is defined
as a line that contains only CR and/or LF."
  (if (or (and (= (length line) 1)
               (or (char-equal (char line 0) #\Return)
                   (char-equal (char line 0) #\Newline)))
          (and (= (length line) 2)
               (and (char-equal (char line 0) #\Return)
                    (char-equal (char line 1) #\Newline))))
      t
      nil))




(defun html-escape (in-string)
  "Process the incoming string and escape it for output as HTML."
  (let ((out-string ""))
    (declare (type string in-string out-string))
    (dotimes (i (length in-string))
      (cond
        ((char-equal (char in-string i) #\>)
         (setf out-string (str+ out-string "&gt;")))
        ((char-equal (char in-string i) #\<)
         (setf out-string (str+ out-string "&lt;")))
        ((char-equal (char in-string i) #\&)
         (setf out-string (str+ out-string "&amp;")))
        (t
         (setf out-string (str+ out-string (string (char in-string i)))))))
    out-string))

;; our log writer


(defun http-status-code-text (code)
  "Return the text message associated with an HTTP status code."
  (if (null (gethash code *http-status-code-text*))
      (format nil "Whaddaya mean, HTTP status ~a?" code)
      (gethash code *http-status-code-text*)))



(defun http-send-headers (request)
  "Sent the HTTP status code and any accumulated HTTP headers. Will refuse
do to anything if either the headers have already been sent or the
protocol is HTTP/0.9 since this is too dumb for this."
  (if (and (null *http-headers-sent*)
           (not (string-equal (request-protocol request) "HTTP/0.9")))
      (progn
        (format (request-stream request)
                "HTTP/1.1 ~a ~a~a~a"
                *http-status-code*
                (http-status-code-text *http-status-code*)
                #\Return #\Linefeed)
        (dolist (pair *http-response-headers*)
          (format (request-stream request)
                  "~a: ~a~a~a"
                  (car pair)
                  (cdr pair)
                  #\Return #\Linefeed))
        (format (request-stream request)
                "Server: ~a~a~a"
                *server-version*
                #\Return #\Linefeed)
        (format (request-stream request)
                "Date: ~a~a~a"
                (get-rfc1123-timestamp)
                #\Return #\Linefeed)
        (format (request-stream request)
                "Content-Type: ~a~a~a~a~a"
                *http-content-type*
                #\Return #\Linefeed
                #\Return #\Linefeed)))
  (setf *http-headers-sent* t))



(defun http-add-headers (new-headers)
  "Add HTTP headers to send for the response. Accepts either a single
header represented as a cons cell or a list of headers represented as
a list of cons cells. This function will append the headers to the
list of headers to be sent. It will refuse to do anything if the headers
_have_ already been sent."
  (if (and (null *http-headers-sent*)
           (listp new-headers))
      (if (consp (first new-headers))
          (setf *http-response-headers*
                (append *http-response-headers*
                        new-headers))
          (setf *http-response-headers*
                (append *http-response-headers*
                        (list new-headers))))))


(defun http-set-status-code (code)
  "Set the HTTP status code for the request. Will refuse to do anything
if the HTTP headers have already been sent."
  (if (and (null *http-headers-sent*)
           (numberp code))
      (setf *http-status-code* code)))


(defun http-set-content-type (content-type)
  "Set the HTTP content type for the request. Will refuse to do anything
if the HTTP headers have already been sent."
  (if (null *http-headers-sent*)
      (setf *http-content-type* content-type)))



(defun default-request-handler (request)
  "The default request handler to run when no handler is found."
  (let ((netlink (request-stream request)))
    (http-add-headers (cons "X-lns.http-internal" "default page"))
    (http-set-status-code 404)
    (http-send-headers request)
    
    (format netlink
            (str+
             "<html>~%<head>~%<title>"
             "lns.http: URL http://~a:~a~a not found"
             "</title>~%</head>~%")
            (request-host request)
            (request-server-port request)
            (request-uri request))
    (format netlink
            (str+
             "<body>~%<h1>lns.http: URL not found.</h1>~%"
             "<p>The URL "
             "<a href=\"http://~a:~a~a\">http://~a:~a~a</a> "
             "you requested was not "
             "found on this server.</p>"
             "<hr><address>served by: ~a</address>")
            (request-host request)
            (request-server-port request)
            (request-uri request)
            (request-host request)
            (request-server-port request)
            (request-uri request)
            *server-version*)
    (format netlink
            "</body>~%</html>~%")))




(defun get-uri-path (uri)
  "Return the path component of the provided URI, returns NIL on failure.
Currently only understands either path-only or http:// absolute URIs."
  (cond
    ;; just a simple path?
    ((char-equal (char uri 0) #\/)
     uri)
    ;; HTTP absolute URL?
    ((string-equal (subseq uri 0 7) "http://")
     (if (> (- (length uri) 7) 0)
         (subseq (subseq uri 7)
                 (position #\/ (subseq uri 7)))
         nil))
    (t nil)))


(defun find-url-handler (server-instance host-name path)
  "Find the URL handler matching the supplied arguments."
  (if (and (not (null server-instance))
           (not (null host-name))
           (not (null path)))
      (sb-thread:with-recursive-lock (*servers-mutex*)
        (if (not (null (gethash server-instance *servers*)))
            (if (hash-table-p
                 (server-host-names
                  (gethash server-instance *servers*)))
                (if (hash-table-p
                     (gethash host-name
                              (server-host-names
                               (gethash server-instance *servers*))))
                    (gethash path
                             (gethash host-name
                                      (server-host-names
                                       (gethash server-instance
                                                *servers*))))))))))


(defun handle-invalid-request (request)
  "Handle an invalid request: basically write out an appropriate
error message and exit."
  (let ((code    nil)
        (title   nil)
        (message nil))
    
    ;; depending on which protocol version was provided/guessed we have
    ;; to generate different error messages
    (cond
      ((and (string-equal (request-protocol request) "HTTP/1.1")
            (null
             (find-if #'(lambda (item)
                          (string-equal (string-upcase (car item)) "HOST"))
                      (request-headers request))))
       (setf code 400)
       (setf title "Bad request")
       (setf message
             (str+
              (format nil "Request not understood.<BR>~%")
              (format nil "HTTP/1.1 requires a Host: header.<BR>~%"))))

      ((and (not (null (request-method request)))
            (null (member (request-method request)
                          *supported-http-methods* :test #'equal)))
       (setf code 501)
       (setf title "Method not implemented")
       (setf message
             (format nil "Method ~a to path ~a not supported<BR>~%"
                     (request-method request)
                     (request-path request))))
    
      (t
       (setf code 400)
       (setf title "Bad request")
       (setf message
             (format nil "Request not understood.<BR>~%"))))

    (if (not (string-equal (request-protocol request) "HTTP/0.9"))
        (progn
          (format (request-stream request)
                  "HTTP/1.1 ~a ~a~a~a"
                  code
                  title
                  #\Return #\Linefeed)
          (format (request-stream request)
                  "Server: ~a~a~a"
                  *server-version*
                  #\Return #\Linefeed)
          (format (request-stream request)
                  "Connection: close~a~a"
                  #\Return #\Linefeed)
          (format (request-stream request)
                  "Content-Type: text/html~a~a"
                  #\Return #\Linefeed)
          (format (request-stream request)
                  "~a~a"
                  #\Return #\Linefeed)))
        ;; HTTP previous to HTTP/1.0 is not expected to understand
        ;; proper HTTP response codes & headers, so any client not
        ;; requesting HTTP/1.0 or HTTP/1.1 (which is thereby assumed to
        ;; be a HTTP/0.9 client) gets dumped the HTML directly down
        ;; its gullet.
    (format (request-stream request)
            "<HTML>~%<HEAD>~%  <TITLE>~a - ~a</TITLE>~%</HEAD>~%"
            code title)
    (format (request-stream request)
            "<BODY>~%  <H1>~a</H1>~%~a"
            title message)
    (format (request-stream request)
            "  <ADDRESS>~a</ADDRESS>~%</BODY>~%</HTML>~%"
            *server-version*)))


(defun valid-http-requestp (request)
  "Predicate to check the supplied request wether it at least looks like
a valid HTTP request."
  (and (not (null (member (request-protocol request)
                          *supported-http-protocol-versions*
                          :test #'string-equal)))
       (not (null (member (request-method request)
                          *supported-http-methods*
                          :test #'string-equal)))
       (if (string-equal (request-protocol request) "HTTP/1.1")
           (not
            (null
             (find-if #'(lambda (item)
                          (string-equal (string-upcase (car item))
                                        "HOST"))
                      (request-headers request))))
           t)))


(defun handle-request (name request)
  "Handle the incoming request. At this stage, the request line and
the headers (if any) have already been analyzed, so we can at most
read the body and the be on with it. Selects either a matching handler
from the registered request handlers or, failing that, runs the default
request handler."
  (let ((*http-response-headers* nil)
        (*http-headers-sent* nil)
        (*http-status-code* 200)
        (*http-content-type* "text/html; charset=UTF-8"))
    (if (valid-http-requestp request)
        (let ((handler nil))
          (setf handler (find-url-handler name
                                          (request-host request)
                                          (request-path request)))
          (if (not (null handler))
              (funcall handler request)
              (default-request-handler request))
          (sb-thread:with-recursive-lock (*servers-mutex*)
            (incf (server-request-count (gethash name *servers*)))))
        (progn
          (lns:write-log "invalid request")
          (handle-invalid-request request)))))
  


(defun analyze-request (stream ip port peer-ip peer-port)
  "Request analyzer. Reads reads the request line and any header
lines from the incoming request (but not the request body) and returns
a freshly constructed request structure."
  
  (let ((method   "")  ; request method: POST, GET, ...
        (uri      "")  ; request uri, like /images/foo.png
        (headers  '()) ; association list of headers
        (host     nil) ; request host for HTTP/1.1
        (protocol "")  ; protocol version, like HTTP/1.0

        (start-time   (get-universal-time))
        
        (line         nil)
        (got-method   nil)
        (got-uri      nil)
        (got-protocol nil))

        
;; read and parse the request line
    (setf line (chomp (read-line stream nil nil)))
    (dotimes (i (length line))
      (cond
        ((not got-method)
         (if (whitespacep (char line i))
             (setf got-method t)
             (setf method (concatenate 'string
                                       method
                                       (string (char line i))))))
        ((not got-uri)
         (if (whitespacep (char line i))
             (if (not (string-equal uri ""))
                 (setf got-uri t))
             (setf uri (concatenate 'string
                                     uri
                                     (string (char line i))))))
        ((not got-protocol)
         (if (whitespacep (char line i))
             (if (not (string-equal protocol ""))
                 (setf got-protocol t))
             (setf protocol (concatenate 'string
                                         protocol
                                         (string (char line i))))))))

            
    ;; read and parse the headers
    (block header-parse
      (let ((key "")
            (value "")
            (in-key nil)
            (in-value nil))
        (loop
         (setf line (chomp (read-line stream nil nil)))
         (if (or (null line)
                 (= (length line) 0))
             (progn
               ;; flush any outstanding headers
               (if (not (string-equal key ""))
                   (progn
                     (setf headers (nconc headers (list (cons key value))))
                     (setf key "")
                     (setf value "")))
               (return-from header-parse t))
         ; check for header continuation
         (if (or (char-equal (char line 0) #\Space)
                 (char-equal (char line 0) #\Tab))
             (progn                                 ; continuation of header
               (dotimes (i (length line))
                 (setf value (concatenate 'string value
                                          (string (char line i))))))
             ; no continuation, flush key/value pair onto headers list
             ; that is, if we have any processed headers waiting
             (progn
               (if (not (string-equal key ""))
                   (progn
                     (setf headers (nconc headers (list (cons key value))))
                     (setf key "")
                     (setf value "")))
               ; start reading the next key/value pair
               (setf in-key t)
               (setf in-value nil)
               (dotimes (i (length line))
                 (cond
                   ((and in-key
                         (char-equal (char line i) #\:))
                    (setf in-key nil))
                   ((and (not in-key)
                         (not in-value)
                         (char-equal (char line i) #\Space))
                    nil) ; do nothing
                   ((and (not in-key)
                         (not in-value)
                         (not (char-equal (char line i) #\Space)))
                    (setf in-value t)
                    (setf value (concatenate 'string
                                             value
                                             (string (char line i)))))
                   (in-key
                    (setf key (concatenate 'string
                                             key
                                             (string (char line i)))))
                   (in-value
                    (setf value (concatenate 'string
                                             value
                                             (string (char line i)))))))))))))

    ;; try to guess the host part of a HTTP/1.1 request

    (setf host
          (let ((work (cdr
                       (find-if #'(lambda (item)
                                    (string-equal (string-upcase (car item))
                                                  "HOST"))
                                headers))))
            (subseq work 0 (position #\: work))))

    ;; ok, request string and headers parsed
    ;; ready to build and return the request structure

    (make-request
     :start-time  start-time
     :stream      stream
     :method      method
     :host        host
     :uri         uri
     :protocol    protocol
     :headers     headers
     :server-ip   ip
     :server-port port
     :client-ip   peer-ip
     :client-port peer-port)))


(defun hex-char-digit (hex-char)
  (declare (type character hex-char))
  "returns the decimal digit for the specified hex char, nil if it
is not a hexadecimal char (not in 0 .. 9 a .. f). Works case-insensitive"
  (let ((hex-char-digits "0123456789ABCDEF"))
    (position (char-upcase hex-char) hex-char-digits)))


(defun url-ok-char-p (testchar)
  "returns T if testchar is ok for verbatim printing in URLs, NIL if not"
  (cond ((alphanumericp testchar) t)
        ((member testchar *url-encode-ok-chars*) t)
        ( t nil)))



(defun url-escape-char (char)
  (let ((bytes (sb-ext:string-to-octets (string char)
                                        :external-format :utf-8)))
    (with-output-to-string (out)
      (map nil
           #'(lambda (byte) (format out "%~2,'0X" byte))
           bytes))))


(defun url-encode-string (input-string)
  "URL-encodes the input-string and returns an url-encoded output-string"
  (let ((work-stream (make-string-output-stream)))
    (dotimes (position (length input-string)
              (get-output-stream-string work-stream))
      (if (url-ok-char-p (char input-string position))
          (princ (char input-string position) work-stream) ; copy verbatim
          (princ (url-escape-char (char input-string position))
                 work-stream)))))


(defun url-decode-bytes (input-string)
  "Decode bytes coming in as URL-encoded string."
  (let ((byte-vect (make-array (length input-string)
                               :element-type '(unsigned-byte 8)
                               :fill-pointer 0))
        (num-low 0)
        (num-high 0)
        (skip 0))
    (dotimes (position (length input-string))
      (cond ((= skip 1) (setf num-low (hex-char-digit
                                       (char input-string position)))
             (vector-push (+ (* num-high 16) num-low) byte-vect)
             (setf skip (- skip 1)))
            ((= skip 2) (setf num-high (hex-char-digit
                                        (char input-string position)))
             (setf skip (- skip 1)))
            (t (cond ((char= (char input-string position) #\+) ; space
                      (vector-push (char-code #\Space) byte-vect))
                     ((char= (char input-string position) #\%) ; hexdigit?
                      (if (and
                           (>= (- (length input-string) position) 3)
                           (hex-char-digit (char input-string (+ position
                                                                 1)))
                           (hex-char-digit (char input-string (+ position
                                                                 2))))
                          (setf skip 2) ; hexdigits to process follow
                          
                          ;; Not escaped characters are assumed to have codes
                          ;; that fit into one byte and have the same
                          ;; representation in all charsets.
                          (vector-push (logand #xff
                                               (char-code
                                                (char input-string position)))
                                       byte-vect)))
                     (t (vector-push (logand #xff
                                             (char-code
                                              (char input-string position)))
                                     byte-vect))))))
    byte-vect))



(defun url-decode-string (input-string)
  "URL-decodes the input-string and returns an url-decoded strings, deals
correctly with partly encoded (parts encoded and parts plain) strings.
Invalid hex-code sequences are ignored and printed through verbatim."
  (sb-ext:octets-to-string (url-decode-bytes input-string)
                           :external-format :utf-8))
                            

(defun process-uri (request)
  "Does further processing on the uri, breaking it down into the
absolute path, the query and further breaking down the query into
an association list of query arguments. Returns the refined request
structure."

  ;; basic sanitization - replace not provided/empty values with
  ;; "sensible" defaults
  (if (or (null (request-uri request))
          (string-equal (request-uri request) ""))
      (setf (request-uri request) "/"))
  
  (if (or (null (request-protocol request))
          (string-equal (request-protocol request) ""))
      (setf (request-protocol request) "HTTP/0.9"))

  (if (or (null (request-method request))
            (string-equal (request-method request) ""))
      (setf (request-method request) NIL))


  
  (setf (request-path request)
        (if (null (position  #\? (request-uri request)))
            (url-decode-string (request-uri request))
            (url-decode-string
             (subseq (request-uri request)
                     0
                     (position #\? (request-uri request))))))
  
  (setf (request-query request)
        (if (null (position  #\? (request-uri request)))
            nil
            (subseq (request-uri request)
                    (1+ (position #\? (request-uri request))))))

  request)

(defun process-request-arguments (request)
  "Process any arguments the client provided for a GET/POST request,
store the information in the request structure and return it."
  
  (if (and (string-equal "POST" (request-method request))
           (not (null (find-if #'(lambda (item)
                                   (string-equal "Content-Length"
                                                 (car item)))
                               (request-headers request))))
           (not (null (find-if #'(lambda (item)
                                   (string-equal "Content-Type"
                                                 (car item)))
                               (request-headers request))))
           (string-equal "application/x-www-form-urlencoded"
                         (cdr
                          (find-if #'(lambda (item)
                                       (string-equal "Content-Type"
                                                     (car item)))
                                   (request-headers request)))))
           
           
      (progn
        (setf (request-query request)
              (make-string
               (parse-integer
                (cdr
                 (find-if #'(lambda (item)
                              (string-equal "Content-Length"
                                            (car item)))
                          (request-headers request)))
                :junk-allowed t)))
        (read-sequence (request-query request)
                       (request-stream request)
                       :start 0
                       :end (parse-integer
                             (cdr
                              (find-if #'(lambda (item)
                                           (string-equal "Content-Length"
                                                         (car item)))
                                       (request-headers request)))
                             :junk-allowed t))))
  
  
  ;; Does the request contain a query or only a path?
  (if (not (null (request-query request)))
      ;; yes, there is a query
      ;; basic structure of an HTTP query:
      ;; - key=value pairs, separated by &
      (let ((key      "")
            (value    "")
            (in-key   t)
            (in-value nil))
        
        (dotimes (i (length (request-query request)))
          (cond
            ((char-equal (char (request-query request) i) #\=)
             (setf in-key nil)
             (setf in-value t))
            
            ((char-equal (char (request-query request) i) #\&)
             (setf in-value nil)
             (setf in-key t)
             ;; flush key/value pairs
             (setf (request-arguments request)
                   (append (request-arguments request)
                           (list (cons (url-decode-string key)
                                       (url-decode-string value)))))
             (setf key "")
             (setf value ""))

            (in-key
             (setf key (concatenate 'string
                                    key
                                    (string
                                     (char (request-query request) i)))))
            
            (in-value
             (setf value (concatenate 'string
                                      value
                                      (string
                                       (char (request-query request) i)))))))
        
        ;; flush leftover key/value pairs
        (if (not (string-equal key ""))
          (setf (request-arguments request)
                (append (request-arguments request)
                        (list (cons (url-decode-string key)
                                    (url-decode-string value))))))))
  
  request)


(defun handler-abort (request type reason &key (broken-stream nil))
  "Emergency abort, a serious condition happened. Try to get an error
message to the client. If this fails too, there is still the
disconnect-and-exit error handler of LNS to fall back onto. Not exactly
elegant, but better than nothing at all. Important: if you call this because
of a stream-error on the ouput stream, you have to set :broken-stream to T
or you will start leaking file descriptors."
  (log-error
   (format nil
           "on http://~a:~a~a from client ~a url-handler-abort (~a): ~a"
           (request-host request)
           (request-server-port request)
           (request-uri request)
           (request-client-ip request)
           type
           reason))
  (if (not (null broken-stream))
      ;; Ok, that was a nasty one to find.
      ;; What happens is that our normal error handling would try to
      ;; write an error message to the client and then exit. A noble
      ;; approach in most cases ... but what happens if the _client_
      ;; drops the connection on his end? Then we are up shit creek without
      ;; a paddle and our boat is slowly filling with crap. Or rather, we
      ;; start leaking file descriptors since there is still stuff waiting to
      ;; be sucked up by the client, but the client doesn't give a rats ass
      ;; so it's going to be stuck in the outgoing buffers and the stream
      ;; never gets gc'ed. Bad. Because you're gonna run out of fd's sooner
      ;; or later and then your request-serving times are over.
      ;; The fix? If a stream-error pops up, call the handler-abort with
      ;; the :broken-stream argument set to T. We will now abstain from writing
      ;; anything down the stream and instead unceremoniously dump the stream
      ;; on the floor so fast it breaks the speed of sound on the way down.
      ;; And voila - no more file descriptor leakage in case of the client
      ;; dropping the connection. Thanks for the :abort keyword to close.
      (progn
        (log-error
         (format nil "attempting close :abort on stream"))
        (close (request-stream request) :abort t))
      (format (request-stream request)
              "~%lns.http: processing error: ~a~%" reason)))



(defun acceptor (name stream ip port peer-ip peer-port)
  "Central entry point for requests into the lns/http server. This
function takes the TCP connection, reads and analyses the request
method & uri, reads the headers and dispatches further processing
to the appropriate function."
  (let ((request nil)
        (*connection-timer* nil))

    (unwind-protect
         (progn

           ;; set us up the bomb
           (setf *connection-timer*
                 (sb-ext:make-timer #'(lambda ()
                                        (sb-ext:quit))
                                    :name "connection timeout"
                                    :thread sb-thread:*current-thread*))
           (sb-ext:schedule-timer *connection-timer*
                                  *connection-timeout*)
                 
           
           (setf request (process-request-arguments
                          (process-uri
                           (analyze-request stream
                                            ip
                                            port
                                            peer-ip
                                            peer-port))))
           (log-request request)
           
           (handler-case
               (handle-request name request)

             (stream-error (condition)
               ;; ok, we got a stream error, it most likely is something like
               ;; "Couldn't write to <stream-spec> ..." because our client
               ;; dropped the stream (called close on his end)
               ;; so trying to write anything to the stream would be pointless
               ;; we basically _have_ to drop the stream like a hot potato
               ;; ourselfes, otherwise we're gonna leak file descriptors
               ;; see definition of handler-abort for more
               (handler-abort request
                              "stream-error"
                              condition
                              :broken-stream t))
             ;; assorted other breakage, most likely shit broke
             ;; inside an url handler because of programming errors
             ;; in a web app
             (serious-condition (condition)
               (handler-abort request "serious-condition" condition))))

      (progn
        (sb-ext:unschedule-timer *connection-timer*)
        (close (request-stream request))))))





(defun create-server (&key
                      (name nil)
                      (ip nil)
                      (port nil))
  "Create a populated server structure and return it. Only returns the
server structure if all fields are loaded with non-nil values, returns
nil if any supplied field is nil."
  (if (reduce #'(lambda (a b)
                  (or a b))
              (map 'list #'null (list name ip port)))
      nil
      (make-server
       :name        name
       :ip          ip
       :port        port
       :active      nil
       :host-names  (make-hash-table :test #'equal))))



(defun register-server (server)
  "Register the supplied server structure. Does very basic verification
of server structure. Refuses to re-register already registered server
names."
  (if (and (not (null server))
           (not (reduce #'(lambda (a b)
                            (or a b))
                        (map 'list
                             #'null
                             (list
                              (server-name server)
                              (server-ip server)
                              (server-port server)))))
           (lns:valid-ip-addressp
            (sb-bsd-sockets:make-inet-address (server-ip server)))
           (and (numberp (server-port server))
                (< 0 (server-port server))
                (> 65536 (server-port server)))
           (stringp (server-name server)))
      (progn
        (setf (server-active server) nil)       ; just in case
        (sb-thread:with-recursive-lock (*servers-mutex*)
          (if (null (gethash (server-name server) *servers*))
              (progn
                (setf (gethash (server-name server) *servers*) server)
                t)
              nil)))))

                


(defun unregister-server (name)
  "Unregisters the server instance of the given name. Will refuse to
unregister active servers, these have to be stopped first. Returns t
if successfull, nil if not."
  (sb-thread:with-recursive-lock (*servers-mutex*)
    (if (and (not (null name))
             (not (null (gethash name *servers*)))
             (null (server-active (gethash name *servers*))))
        (progn
          (remhash name *servers*)
          t)
        nil)))


(defun register-server-host-name (server-instance-name host-name)
  "Add a hostname to a registered server. Will destroy existing entries."
  (sb-thread:with-recursive-lock (*servers-mutex*)
    (if (and (not (null server-instance-name))
             (not (null host-name)))
        (if (not (null (gethash server-instance-name *servers*)))
            (setf (gethash host-name
                           (server-host-names (gethash server-instance-name
                                                       *servers*)))
                  (make-hash-table :test #'equal))))))


(defun unregister-server-host-name (server-instance-name host-name)
  "Remove a hostname from a registered server."
  (sb-thread:with-recursive-lock (*servers-mutex*)
    (if (and (not (null server-instance-name))
             (not (null host-name)))
        (if (not (null (gethash server-instance-name *servers*)))
            (remhash host-name
                     (server-host-names (gethash server-instance-name
                                          *servers*)))))))




(defun start-server (name)
  "Start the server instance identified by name. Will only start server
instances that are:
 - registered and
 - not currently active
Returns t on success and nil on failure."
  (let ((ip   nil)
        (port nil)
        (activate-server nil))
    (sb-thread:with-recursive-lock (*servers-mutex*)
      (if (and (not (null (gethash name *servers*))) ; server registered?
               (null (server-active (gethash name *servers*)))) ; active?
          (progn
            (setf ip   (server-ip   (gethash name *servers*)))
            (setf port (server-port (gethash name *servers*)))))
      (if
       (if (and (not (null ip))
                (not (null port)))
           (setf activate-server t)
           nil)
       (progn
         (setf (server-active (gethash name *servers*)) t)
         t)
       nil))
    ;; That is a bit roundabout but avoids starting the network listener
    ;; with the *servers-mutex* lock held
    (if activate-server
        (if (null
             (lns:register-service-handler ip
                                           port
                                           #'(lambda (stream
                                                      ip
                                                      port
                                                      peer-ip
                                                      peer-port)
                                               (acceptor name
                                                         stream
                                                         ip
                                                         port
                                                         peer-ip
                                                         peer-port))))
            ;; hope we never hit this code path
            (sb-thread:with-recursive-lock (*servers-mutex*)
              (setf (server-active (gethash name *servers*)) nil))))))


(defun stop-server (name)
  "Stop a currently active server instance. Will only stop server instances
that are:
 - registered and
 - active.
Returns t on success and nil on failure."
  (sb-thread:with-recursive-lock (*servers-mutex*)
    (if (and (not (null (gethash name *servers*)))
             (not (null (server-active (gethash name *servers*)))))
        (progn
          (lns:unregister-service-handler (server-ip
                                           (gethash name *servers*))
                                          (server-port
                                           (gethash name *servers*)))
          ;; we happily assume killing the listener worked ...
          (setf (server-active (gethash name *servers*)) t)
          t)
        nil)))
  




(defun register-url-handler (server-instance host-name path handler)
  "Register a URL handler for the specified path and hostname in the
specified server-instance."
  (sb-thread:with-recursive-lock (*servers-mutex*)
    (if (not
         (null
          (gethash host-name
                   (server-host-names (gethash server-instance
                                               *servers*)))))
        (setf (gethash path
                       (gethash host-name
                                (server-host-names
                                 (gethash server-instance
                                          *servers*))))
              handler))))

  

(defun unregister-url-handler (server-instance host-name path)
  "Remove the registered URL-handler."
  (sb-thread:with-recursive-lock (*servers-mutex*)
    (remhash path
             (gethash host-name
                      (server-host-names
                       (gethash server-instance *servers*))))))




;; predefind URL handlers
(defun url-handler-server-info (request)
  "Display some server information."
  (let ((netlink (request-stream request)))
    (http-add-headers (cons "X-lns.http-internal" "server-info"))
    (http-send-headers request)

    (format netlink
            "<html>~%<head>~%<title>lns.http server-info</title>~%</head>~%")
    (format netlink
            "<body>~%<h1>lns.http server-info</h1>~%")

    (format netlink "<h2>general information</h2>~%")
    (format netlink "<p>~%server-version: ~a</p>~%" *server-version*)

    (format netlink "<h2>thread information</h2>~%")
    (format netlink
            "<p>~%total number of threads: ~a</p>~%"
            (length (sb-thread:list-all-threads)))
    (format netlink
            "<p>~%maximum allowed  number of threads: ~a</p>~%"
            (lns:get-max-threads))

    (format netlink "list of running threads:~%")
    (format netlink "<ul>~%")
    (format netlink
            "~a"
            (reduce #'(lambda (one two)
                        (concatenate 'string one two))
                    (map 'vector
                         #'(lambda (item)
                             (concatenate 'string
                                          "<li>"
                                          (html-escape (format nil "~a" item))
                                          (format nil "</li>~%")))
                         (sb-thread:list-all-threads))))
    (format netlink "</ul>~%")


    (format netlink "<h2>memory information</h2>~%<ul>~%")
    (format netlink
            "<li>bytes consed between GCs: ~a bytes</li>~%"
            (sb-ext:bytes-consed-between-gcs))
    (format netlink
            "<li>bytes consed since startup: ~a bytes</li>~%"
            (let ((byte-count (sb-ext:get-bytes-consed))
                  (divisor 1)
                  (unit "KB"))
              (cond
                ((> byte-count 1073741824)
                 (setf unit "GB")
                 (setf divisor 1073741824))
                ((> byte-count 1048576)
                 (setf unit "MB")
                 (setf divisor 1048576))
                ((> byte-count 1024)
                 (setf unit "KB")
                 (setf divisor 1024)))
              (format nil "~a ~a"
                      (float (/ (round (* byte-count 100) divisor) 100))
                      unit)))
                    
    (format netlink
            "<li>CPU time spent for GC since startup: ~a milliseconds</li>~%"
            sb-ext:*gc-run-time*)
    (format netlink "</ul>~%")

    (format netlink
            "<pre>~%~a</pre>~%"
            (let ((*standard-output* (make-string-output-stream)))
              (room t)
              (get-output-stream-string *standard-output*)))

    (format netlink "<h2>Lisp & system environment</h2>~%<ul>~%")
    (format netlink
            "<li>machine type: ~a</li>~%"
            (machine-type))
    (format netlink
            "<li>machine version: ~a</li>~%"
            (machine-version))
    (format netlink
            "<li>machine instance: ~a</li>~%"
            (machine-instance))
    (format netlink
            "<li>software type: ~a</li>~%"
            (software-type))
    (format netlink
            "<li>software version: ~a</li>~%"
            (software-version))
    (format netlink
            "<li>lisp implemenation type: ~a</li>~%"
            (lisp-implementation-type))
    (format netlink
            "<li>lisp implemenation version: ~a</li>~%"
            (lisp-implementation-version))
    (format netlink "</ul>~%")

    
    (format netlink
            "</body>~%</html>~%")))


(defun url-handler-request-info (request)
  "URL handler to show the internal request information."
  (format (request-stream request)
          "HTTP/1.1 200 OK~a~a" #\Return #\Linefeed)
  (format (request-stream request)
          "Connection: close~a~a"
          #\Return #\Linefeed)
  (format (request-stream request)
          "~a~a"
          #\Return #\Linefeed)
  (format (request-stream request)
          "<html>~%<head><title>lns/http</title><head>~%")
  (format (request-stream request)
          "<body>~%<h1>lns/http</h1>~%")
  (format (request-stream request)
          "lns/http request info page handler.~%")
  (format (request-stream request)
          "The incoming request was:~%<pre>~%")
  (format (request-stream request)
          "~a~%<pre>~%" (html-escape (format nil "~a" request)))
  (format (request-stream request)
          "</body></html>~%"))



(defun url-handler-file (request root-path)
  "URL handler to deliver files straight from the filesystem."
  (let ((netlink (request-stream request))
        (file-path nil))

    (setf file-path
          (cdr 
           (find-if #'(lambda (item)
                        (string-equal "path"
                                      (car item)))
                    (request-arguments request))))

    (http-add-headers (cons "X-lns.http-internal" "local file handler"))

    ;; first, check for acceptable paths and deny service
    ;; if any mischief is detected
    
    (cond
      ;; absolute paths are not allowed on general principle
      ((char-equal (char file-path 0) #\/)
       (http-set-status-code 403)
       (http-send-headers request)
       (format netlink
               (str+
                "<html>~%<head>"
                "<title>absolute paths not allowed</title>"
                "</head><body>~%"
                "absolute paths are not allowed for file requests"
                "<hr>"
                "<address>served by: ~a</address>"
                "</body>~%</html>~%")
               *server-version*))
      ;; somebody trying to sneak some directory traversal past us?
      ((search "/../" file-path)
       (http-set-status-code 403)
       (http-send-headers request)
       (format netlink
               (str+
                "<html>~%<head>"
                "<title>directory traversal not allowed</title>"
                "</head><body>~%"
                "directory traversal is not allowed for file requests"
                "<hr>"
                "<address>served by: ~a</address>"
                "</body>~%</html>~%")
               *server-version*))
      ;; ok, everything fine so far, let's work on delivering the file
      (t (handler-case
             (with-open-file (file-handle
                              (str+ root-path "/" file-path)
                              :direction :input
                              :if-does-not-exist nil
                              :element-type '(unsigned-byte 8))
               (if (null file-handle)
                   (progn
                     (http-set-status-code 404)
                     (http-send-headers request)
                     (format netlink
                             (str+
                              "<html>~%<head>"
                              "<title>file not found</title>"
                              "</head><body>~%"
                              "file ~a not found"
                              "<hr>"
                              "<address>served by: ~a</address>"
                              "</body>~%</html>~%")
                             file-path
                             *server-version*))
                   (let ((file-size 0)
                         (file-buffer nil)
                         (file-block-size 65536)
                         (file-transfer-size 0))


                     (http-set-status-code 200)
                     (setf file-size (file-length file-handle))
                     (http-add-headers (cons "Content-Length"
                                             file-size))
                     (http-set-content-type
                      (get-mime-type
                       (pathname-type file-path)))
                     (http-send-headers request)
                     (setf file-buffer
                           (make-sequence '(vector (unsigned-byte 8))
                                          file-block-size))
                     (loop
                      (setf file-transfer-size
                            (read-sequence file-buffer file-handle))
                      (if (equal 0 file-transfer-size)
                          (return-from url-handler-file)
                          (write-sequence file-buffer netlink))))))
           (file-error (condition)
             (http-set-status-code 403)
             (http-send-headers request)
             (format netlink
                     (str+
                      "<html>~%<head>"
                      "<title>file open failed</title>"
                      "</head><body>~%"
                      "error opening requested file ~a: ~a"
                      "<hr>"
                      "<address>served by: ~a</address>"
                      "</body>~%</html>~%")
                     file-path
                     (let ((printed-condition (format nil
                                                      "~a"
                                                      condition)))
                       (subseq printed-condition
                               (1+ (position #\: printed-condition))
                               (length printed-condition)))
                       *server-version*)))))))


(defun log-flush ()
  "Flush the logs for the lns.http package."
  (sb-thread:with-recursive-lock (*http-log-file-handle-mutex*)
    (if (not (null *http-log-file-handle*))
        (progn
          (close *http-log-file-handle*)
          (setf *http-log-file-handle* nil)))
    (handler-case
        (setf *http-log-file-handle*
              (open (sb-thread:with-recursive-lock (*http-log-file-mutex*)
                      *http-log-file*)
                    :direction :output
                    :external-format :utf-8
                    :if-exists :append
                    :if-does-not-exist :create))
      (serious-condition (condition)
        (sb-posix:syslog sb-posix:log-err
                         (format nil
                                 "http.lisp opening log file ~a failed: ~a"
                                 (sb-thread:with-recursive-lock
                                     (*http-log-file-mutex*)
                                   *http-log-file*)
                                 condition))
        (lns:write-log
         (format nil
                 "failed to open request file ~a"
                 (sb-thread:with-recursive-lock (*http-log-file-mutex*)
                   *http-log-file*)))
        (setf *http-log-file-handle* nil)))) ; just in case

  (sb-thread:with-recursive-lock (*http-error-log-file-handle-mutex*)
    (if (not (null *http-error-log-file-handle*))
        (progn
          (close *http-error-log-file-handle*)
          (setf *http-error-log-file-handle* nil)))
    (handler-case
        (setf *http-error-log-file-handle*
              (open (sb-thread:with-recursive-lock
                        (*http-error-log-file-mutex*)
                      *http-error-log-file*)
                    :direction :output
                    :external-format :utf-8
                    :if-exists :append
                    :if-does-not-exist :create))
      (serious-condition (condition)
        (sb-posix:syslog sb-posix:log-err
                         (format nil
                                 "http.lisp opening log file ~a failed: ~a"
                                 (sb-thread:with-recursive-lock
                                     (*http-error-log-file-mutex*)
                                   *http-log-file*)
                                 condition))
        (lns:write-log
         (format nil
                 "failed to open request file ~a"
                 (sb-thread:with-recursive-lock (*http-error-log-file-mutex*)
                   *http-error-log-file*)))
        (setf *http-error-log-file-handle* nil)))) ; just in case

  )

(defun log-flush-old ()
  "Flush the lns.log, called by log-writer."
  (let ((log-file-path))

    ;; the access log
    (setf log-file-path
          (sb-thread:with-recursive-lock (*http-log-file-mutex*)
            *http-log-file*))
    (sb-thread:with-recursive-lock (*http-log-file-handle-mutex*)
      (if (not (null *http-log-file-handle*))
          (close *http-log-file-handle*)) ; should force a flush
      ;; always re-open it
      (setf *http-log-file-handle*
            (open log-file-path
                  :direction :output
                  :external-format :utf-8
                  :if-exists :append
                  :if-does-not-exist :create)))
    
        ;; the error log
    (setf log-file-path
          (sb-thread:with-recursive-lock (*http-error-log-file-mutex*)
            *http-error-log-file*))
    (sb-thread:with-recursive-lock (*http-error-log-file-handle-mutex*)
      (if (not (null *http-error-log-file-handle*))
          (close *http-log-file-handle*)) ; should force a flush
      ;; always re-open it
      (setf *http-error-log-file-handle*
            (open log-file-path
                  :direction :output
                  :external-format :utf-8
                  :if-exists :append
                  :if-does-not-exist :create)))))




(defun log-writer ()
  "Our logs are written asynchronously to avoid the logwriter bogging
down request handling. This means that they only get written if we
generate more data than fits whatever buffers are there. To reduce this
effect, we use this log-writer who basically closes/opens the logfile
every *log-writer-interval* seconds."
  (unwind-protect
       (loop
        (sleep (sb-thread:with-recursive-lock (*log-writer-interval-mutex*)
                 *log-writer-interval*))
        (log-flush))
    (log-flush)))  ; that should take care of flush-on-process-exit



;; init lookup tables - under normal operations, those are read-only,
;; so we need no mutex protection

(setf (gethash 100 *http-status-code-text*) "Continue")
(setf (gethash 101 *http-status-code-text*) "Switching Protocols")
(setf (gethash 200 *http-status-code-text*) "OK")
(setf (gethash 201 *http-status-code-text*) "Created")
(setf (gethash 202 *http-status-code-text*) "Accepted")
(setf (gethash 203 *http-status-code-text*) "Non-Authoritative Information")
(setf (gethash 204 *http-status-code-text*) "No Content")
(setf (gethash 205 *http-status-code-text*) "Reset Content")
(setf (gethash 206 *http-status-code-text*) "Partial Content")
(setf (gethash 300 *http-status-code-text*) "Multiple Choices")
(setf (gethash 301 *http-status-code-text*) "Moved Permanently")
(setf (gethash 302 *http-status-code-text*) "Found")
(setf (gethash 303 *http-status-code-text*) "See Other")
(setf (gethash 304 *http-status-code-text*) "Not Modified")
(setf (gethash 305 *http-status-code-text*) "Use Proxy")
(setf (gethash 307 *http-status-code-text*) "Temporary Redirect")
(setf (gethash 400 *http-status-code-text*) "Bad Request")
(setf (gethash 401 *http-status-code-text*) "Unauthorized")
(setf (gethash 402 *http-status-code-text*) "Payment Required")
(setf (gethash 403 *http-status-code-text*) "Forbidden")
(setf (gethash 404 *http-status-code-text*) "Not Found")
(setf (gethash 405 *http-status-code-text*) "Method Not Allowed")
(setf (gethash 406 *http-status-code-text*) "Not Acceptable")
(setf (gethash 407 *http-status-code-text*) "Proxy Authentication Required")
(setf (gethash 408 *http-status-code-text*) "Request Timeout")
(setf (gethash 409 *http-status-code-text*) "Conflict")
(setf (gethash 410 *http-status-code-text*) "Gone")
(setf (gethash 411 *http-status-code-text*) "Length Required")
(setf (gethash 412 *http-status-code-text*) "Precondition Failed")
(setf (gethash 413 *http-status-code-text*) "Request Entity Too Large")
(setf (gethash 414 *http-status-code-text*) "Request-URI Too Long")
(setf (gethash 415 *http-status-code-text*) "Unsupported Media Type")
(setf (gethash 416 *http-status-code-text*) "Requested Range Not Satisfiable")
(setf (gethash 417 *http-status-code-text*) "Expectation Failed")
(setf (gethash 500 *http-status-code-text*) "Internal Server Error")
(setf (gethash 501 *http-status-code-text*) "Not Implemented")
(setf (gethash 502 *http-status-code-text*) "Bad Gateway")
(setf (gethash 503 *http-status-code-text*) "Service Unavailable")
(setf (gethash 504 *http-status-code-text*) "Gateway Timeout")
(setf (gethash 505 *http-status-code-text*) "HTTP Version Not Supported")

(setf (gethash 0 *day-of-week-to-short-weekday*) "Mon")
(setf (gethash 1 *day-of-week-to-short-weekday*) "Tue")
(setf (gethash 2 *day-of-week-to-short-weekday*) "Wed")
(setf (gethash 3 *day-of-week-to-short-weekday*) "Thu")
(setf (gethash 4 *day-of-week-to-short-weekday*) "Fri")
(setf (gethash 5 *day-of-week-to-short-weekday*) "Sat")
(setf (gethash 6 *day-of-week-to-short-weekday*) "Sun")

(setf (gethash  1 *month-to-short-monthname*) "Jan")
(setf (gethash  2 *month-to-short-monthname*) "Feb")
(setf (gethash  3 *month-to-short-monthname*) "Mar")
(setf (gethash  4 *month-to-short-monthname*) "Apr")
(setf (gethash  5 *month-to-short-monthname*) "May")
(setf (gethash  6 *month-to-short-monthname*) "Jun")
(setf (gethash  7 *month-to-short-monthname*) "Jul")
(setf (gethash  8 *month-to-short-monthname*) "Aug")
(setf (gethash  9 *month-to-short-monthname*) "Sep")
(setf (gethash 10 *month-to-short-monthname*) "Oct")
(setf (gethash 11 *month-to-short-monthname*) "Nov")
(setf (gethash 12 *month-to-short-monthname*) "Dec")


(defun start-framework ()
  "Start the lns.http server framework."
  ;; init MIME type mapping, this is also read only during normal operation
  (init-mime-mapping *mime-types-file*)
  ;; right now, only start the logwriter thread
  (sb-thread:with-mutex (*log-writer-active-mutex* :value t :wait-p t)
    (if (null *log-writer-active*)
        (setf *log-writer-active*
              (sb-thread:make-thread #'(lambda ()
                                         (log-writer))
                                     :name "lns.http log writer thread")))))


