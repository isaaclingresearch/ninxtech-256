(in-package :256)

(eval-when (:load-toplevel)
  )
;;; HTTP(S) 
(setq *show-lisp-errors-p* t) ;; set this to show error files in /priv/errors

;; define server config
;;;; these are set in $HOME/.bashrc to be accessible in the sbcl repl 
(defvar *256-http-port* (parse-integer (uiop:getenv "HTTP_PORT")))
(defvar *256-https-port* (parse-integer (uiop:getenv "HTTPS_PORT")))
(defvar *256-ssl-cert* (uiop:getenv "SSL_CERT"))
(defvar *256-ssl-key* (uiop:getenv "SSL_KEY"))
(defvar *256-url* (uiop:getenv "HOST"))

;; SERVER SETUP FUNCTIONS

;; redirect all traffic to https
(defclass http-to-https-acceptor (hunchentoot:acceptor) ())
(defmethod hunchentoot:acceptor-dispatch-request ((acceptor http-to-https-acceptor) request)
  (hunchentoot:redirect (hunchentoot:request-uri request)
                        :protocol :https :port *256-https-port*))

(defvar *256-ssl-acceptor* (make-instance 'easy-routes-ssl-acceptor :port *256-https-port*
					  :ssl-certificate-file *256-ssl-cert*
					  :ssl-privatekey-file *256-ssl-key*
					  :document-root (truename "~/common-lisp/256/priv/")
					  :error-template-directory (truename "~/common-lisp/256/priv/errors/")))

(defvar *256-http-acceptor* (make-instance 'http-to-https-acceptor :port *256-http-port*))

;; set logging to files
(setf (acceptor-message-log-destination *256-ssl-acceptor*) (truename "~/common-lisp/256/logs/message.log"))
(setf (acceptor-access-log-destination *256-ssl-acceptor*) (truename "~/common-lisp/256/logs/access.log"))
;; don't allow persistent connections
;; this is because the server was not responding to requests, with a 503, and the error logs were showing too many threads.
;; still investigation, but maybe the connections were sending a keep alive header.
(setf (acceptor-persistent-connections-p *256-http-acceptor*) nil)
(setf (acceptor-persistent-connections-p *256-ssl-acceptor*) nil)

;; after reviewing the taskmaster section of the docs, either of two things happened, because i was having one active connections
;; 1). the connections persisted, I don't why that is, but i have stopped persistent connections.
;; 2). The taskmaster ran out of threads, or the max accept was exceeded by the active requests.
;; 3). this is the solution, stop persistent connections above, then increase the threads to 1000, and max accept to 1500.

(let ((http-taskmaster (slot-value *256-http-acceptor* 'taskmaster))
      (https-taskmaster (slot-value *256-ssl-acceptor* 'taskmaster)))
  (setf (slot-value http-taskmaster 'hunchentoot::max-thread-count) 10000)
  (setf (slot-value http-taskmaster 'hunchentoot::max-accept-count) 15000)
  (setf (slot-value https-taskmaster 'hunchentoot::max-thread-count) 10000)
  (setf (slot-value https-taskmaster 'hunchentoot::max-accept-count) 15000))

(defun start-server (&key log-to-file)
  "Start the server"
  (hunchentoot:start *256-http-acceptor*)
  (hunchentoot:start *256-ssl-acceptor*)

  (if log-to-file
      (progn ;; set logging to files
	(setf (acceptor-message-log-destination *256-ssl-acceptor*) (truename "~/common-lisp/256/logs/message.log"))
	(setf (acceptor-access-log-destination *256-ssl-acceptor*) (truename "~/common-lisp/256/logs/access.log")))
      (progn ;; set logging to files
	(setf (acceptor-message-log-destination *256-ssl-acceptor*) *terminal-io*)
	(setf (acceptor-access-log-destination *256-ssl-acceptor*) *terminal-io*))))

(defun stop-server ()
  "Stop the server"
  (when (started-p *256-http-acceptor*)
    (stop *256-http-acceptor*))
  (when (started-p *256-ssl-acceptor*)
    (stop *256-ssl-acceptor*)))

(defun restart-server (&key log-to-file)
  (stop-server)
  (start-server :log-to-file log-to-file))
