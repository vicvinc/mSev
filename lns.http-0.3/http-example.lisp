;;;; file: http-example.lisp
;;;; author: Alexander Schreiber <als@thangorodrim.de>
;;;; 
;;;; Examples for the web server implementation on top of lisp-network-server
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

;;; to use the example:
;;; 
;;; (load "lns.lisp")
;;; (load "http.lisp")
;;; (load "http-example.lisp")
;;; (lns.http.example:start-test-server)
;;; and point your browser at http://localhost:8080/
  
(defpackage :lns.http.example
  (:use :common-lisp :lns.http)
  (:documentation
   "Example for the web server implementation on top of LNS.")
  (:export #:start-test-setup
           ))


(in-package :lns.http.example)
; (use-package :lns.http)


(defmacro str+ (&rest args)
  "A simple shorthand wrapper for (concatenate 'string ...)"
  `(concatenate 'string ,@args))


(defun url-handler-root (request)
  "Our example root."
  (let ((netlink (request-stream request)))
    (http-send-headers request)

    (format netlink
            "<html>~%<head>~%<title>lns.http examples</title>~%</head>~%")
    (format netlink
            (str+
            "<body>~%<h1>lns.http examples</h1>~%"
            "<p>Welcome to the example web site for the lns.http "
            "web server. The example web site contains some very simple "
            "things: ~%"
            "<ul>~%"
            "  <li>a simple <a href=\"/hello\">Hello, world</a></li>~%"
            "  <li>a test for <a href=\"/get-test\">HTTP GET</a></li>~%"
            "  <li>a test for <a href=\"/post-test\">HTTP POST</a></li>~%"
            "  <li>some <a href=\"/server-info\">server information</a></li>~%"
            "  <li><a href=\"/request-info\">request information</a></li>~%"
            "  <li><a href=\"/file?path=README\">local file README</a></li>~%"
            "  <li>and the <a href=\"/foo\">default error page</a></li>~%"
            "</ul>~%</p>~%"))

    (format netlink
            "</body>~%</html>~%")))



(defun url-handler-hello (request)
  "Simple example request handler saying hello world."
  (let ((netlink (request-stream request)))
    (http-add-headers (cons "X-Greeting" "Hello, world!"))
    (http-send-headers request)

    (format netlink
            "<html>~%<head>~%  <title>Hello</title>~%</head>~%")
    (format netlink
            "<body>~%<h1>Hello, world</h1>~%Hello world from lns/http!~%")
    (format netlink
            "</body>~%</html>~%")))


(defun url-handler-get-test (request)
  "Simple test for handling GET requests with arguments."
  (let ((netlink (request-stream request)))
    (http-add-headers (cons "X-lns.http-test" "get"))
    (http-send-headers request)

    (format netlink
            "<html>~%<head>~%  <title>lns.http test: GET</title>~%</head>~%")
    (format netlink
            "<body>~%<h1>lns.http test: GET</h1>~%")
    (format netlink
            (str+
             "<form method=\"GET\" action=\"/get-test\">~%"
             "your text: "
             "<input value=\"~a\" name=\"yourtext\" size=\"64\" type=\"text\">"
             "~%<input type=\"submit\" value=\"submit\">~%"
             "</form>")
            (if (not (null (assoc "yourtext"
                                  (request-arguments request)
                                  :test #'equal)))
                (cdr (assoc "yourtext"
                            (request-arguments request)
                            :test #'equal))
                ""))
    (format netlink
            "</body>~%</html>~%")))



(defun url-handler-post-test (request)
  "Simple test for handling POST requests with arguments."
  (let ((netlink (request-stream request))
        )

    (http-add-headers (cons "X-lns.http-test" "post"))
    (http-send-headers request)

    (format netlink
            "<html>~%<head>~%  <title>lns.http test: POST</title>~%</head>~%")
    (format netlink
            "<body>~%<h1>lns.http test: POST</h1>~%")
    (format netlink
            (str+
             "<form method=\"POST\" action=\"/post-test\">~%"
             "your text: "
             "<input value=\"~a\" name=\"yourtext\" size=\"64\" type=\"text\">"
             "<input value=\"foo\" name=\"bar\" size=\"64\" type=\"hidden\">"
             "~%<input type=\"submit\" value=\"submit\">~%"
             "</form>")
            (if (not (null (assoc "yourtext"
                                  (request-arguments request)
                                  :test #'equal)))
                (cdr (assoc "yourtext"
                            (request-arguments request)
                            :test #'equal))
                ""))

    (format netlink
            (str+
             "<form method=\"POST\" action=\"/request-info\">~%"
             "your text (debug): "
             "<input value=\"~a\" name=\"yourtext\" size=\"64\" type=\"text\">"
             "<input value=\"foo\" name=\"bar\" size=\"64\" type=\"hidden\">"
             "~%<input type=\"submit\" value=\"submit\">~%"
             "</form>")
            (if (not (null (assoc "yourtext"
                                  (request-arguments request)
                                  :test #'equal)))
                (cdr (assoc "yourtext"
                            (request-arguments request)
                            :test #'equal))
                ""))

    (format netlink
            "</body>~%</html>~%")))




(defun start-test-setup ()
  "Start a minimal test setup"

  ;; first, set log files to /tmp

  (lns:set-log-file #P"/tmp/lns.log")
  (sb-thread:with-mutex (lns.http::*http-log-file-mutex* :value t :wait-p t)
    (setf lns.http::*http-log-file* #P"/tmp/lns-http.log"))
  (sb-thread:with-mutex (lns.http::*http-error-log-file-mutex*
                         :value t :wait-p t)
    (setf lns.http::*http-error-log-file* #P"/tmp/lns-http-error.log"))

  
  (register-server (create-server :name "test"
                                  :ip "0.0.0.0"
                                  :port 8080))
  (register-server-host-name "test" "localhost")
  (register-url-handler "test"
                        "localhost"
                        "/hello"
                        #'(lambda (request)
                            (url-handler-hello request)))
  (register-url-handler "test"
                        "localhost"
                        "/get-test"
                        #'(lambda (request)
                            (url-handler-get-test request)))
  (register-url-handler "test"
                        "localhost"
                        "/post-test"
                        #'(lambda (request)
                            (url-handler-post-test request)))
  (register-url-handler "test"
                        "localhost"
                        "/server-info"
                        #'(lambda (request)
                            (lns.http:url-handler-server-info request)))
  (register-url-handler "test"
                        "localhost"
                        "/request-info"
                        #'(lambda (request)
                            (lns.http:url-handler-request-info request)))
  (register-url-handler "test"
                          "localhost"
                          "/"
                          #'(lambda (request)
                              (url-handler-root request)))
  (register-url-handler "test"
                          "localhost"
                          "/file"
                          #'(lambda (request)
                              (lns.http:url-handler-file request ".")))
  
  (start-server "test"))

