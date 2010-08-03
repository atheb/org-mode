;;; org-protocol-httpd.el --- provides a small http server that
;;;                           accepts org-protocol commands as path
;;;                     
;; Copyright 2010 Andreas Burtzlaff
;;
;; Author: Andreas Burtzlaff < andreas at burtzlaff dot de >
;;
;; Credits go to Christopher Wellons < mosquitopsu at gmail dot com >,
;; whose "Emacs web server" inspired me to write this code.
;; http://nullprogram.com/blog/2009/05/17/             
;;
;; Version: 0.1
;; Keywords: org-mode org-protocol http
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;; Commentary:
;;
;; -------------------------------------------------------------------------
;; |                        * IMPORTANT NOTE: *                            |
;; |   PLEASE MAKE SURE TO RESTRICT ACCESS TO PORT 15187 TO LOCALHOST !!!  |
;; -------------------------------------------------------------------------
;;
;; Summary:
;; --------
;;
;; An HTTP-server listening on port 15187 responds to GET-requests where the 
;; path is either: 
;;   - an org-protocol action, e.g. "org-protocol://capture://...".
;;     In this case the associated action from 
;;     `org-protocol-protocol-alist' is executed.
;;
;;   - an org-protocol-httpd action, starting with "org-protocol-httpd://...".
;;     In this case the associated function from 
;;     `org-protocol-httpd-protocol-alist' is evaluated and its value is
;;     returned to the client.
;;
;; Installation:
;; -------------
;; 
;; Put:
;;
;; (require 'org-protocol-httpd)
;; (org-protocol-httpd-start-server)
;; 
;; into .emacs
;;
;; Usage:
;; ------
;; 
;; To trigger org-protocol actions use requests like:
;; 
;; http://localhost:15187/org-protocol://capture://http%3A%2F%2Forgmode.org%2F/Org-Mode%3A%20Your%20Life%20in%20Plain%20Text/
;;
;; Please refer to the documentation of org-protocol for detailed
;; information on the different actions available:
;; 
;; http://orgmode.org/worg/org-contrib/org-protocol.php
;;
;; In Firefox it is possible to create a bookmark with the following location, 
;; to invoke capture:
;;
;; javascript:{var req=new XMLHttpRequest();
;;             req.open('GET','http://localhost:15187/org-protocol://capture://'+
;;                            encodeURIComponent(location.href)+'/'+
;;                            encodeURIComponent(document.title)+'/'+
;;                            encodeURIComponent(window.getSelection()),true);
;;             req.send(null);}
;;
;;
;;


(require 'org-protocol)

(defgroup org-protocol-httpd nil
"A server implementation for external interaction with org-mode.

An HTTP-server that extends the capabilities of org-protocol by
allowing data to be returned to the client.
"
:group 'org-protocol)

(defcustom org-protocol-httpd-port 15187
"The port at which the server will be accepting connections."
:group 'org-protocol-httpd
:type 'integer)

(defconst org-protocol-httpd-process-name "org-protocol-httpd")

(defvar org-protocol-httpd-status-strings
  '((200 . "OK")
    (204 . "No content")
    (400 . "Bad Request")
    (500 . "Internal Server Error"))
  "Mapping of Http 1.0 status codes to strings.")

(defvar org-protocol-httpd-the-protocol "org-protocol-http")

(defconst org-protocol-httpd-protocol-alist-default nil
  "Default protocols to use with org-protocol-http requests.
See `org-protocol-httpd-protocol-alist' for a description of this variable.")


(defcustom org-protocol-httpd-protocol-alist nil
  "A list of custom handlers for org-protocol-httpd requests.

List elements have the form:

  (module-name :protocol protocol :function func :mime mime)

where

protocol : the protocol string to detect in the path of a URL.
           If a protocol \"foo-protocol\" is defined, `org-protocol-httpd-filter'
           will respond to paths starting with \"org-protocol-httpd:/foo-protocol:/\"
           by returning the value of the associated function.

function : the function that actually handles the request.
           It takes the path of the URL without the protocol parts as argument
           If the function returns a string value, it is returned to the client
           as response to the GET-request.

mime     : a mime string that the functions return string is be interpreted as.
           This string is the Content-Type of the HTTP response. 
"
  :group 'org-protocol-httpd
  :type '(alist))


(defun org-protocol-httpd-start-server ()
  "Start org-protocol-httpd server."
  (interactive)
  (org-protocol-httpd-stop)
  (make-network-process
   :name org-protocol-httpd-process-name
   :server t
   :service org-protocol-httpd-port
   :filter 'org-protocol-httpd-filter
   :family 'ipv4
   :coding 'utf-8))

(defun org-protocol-httpd-stop ( &optional process-to-stop)
  "Stop org-protocol-httpd server."
  (interactive)
  (let ((process (or process-to-stop
                     org-protocol-httpd-process-name)))
    (when (process-status process)
      (delete-process process))))

(defun org-protocol-httpd-filter (process header)
  "Respond to query if the path in the http header `header' has
valid org-protocol or org-protocol-httpd format and the subprotocol is registered in
`org-protocol-protocol-alist-default' or `org-protocol-httpd-protocol-alist', respectively."
  (condition-case err
      (progn
        (let* ((header-alist (org-protocol-httpd-parse-header header))
               (path 
		(car (split-string 
		      (replace-regexp-in-string 
		       "^/" "" 
		       (cdr (assoc "GET" header-alist)))
		      "?")))
               responseString 
               (responseStatus 500))
          (cond ((string-match (concat "^" org-protocol-the-protocol ":") path)
                 ;; let org-protocol handle the path
                 ;; TODO: Find a way to check whether path has been
                 ;;       digested without errors. Use an error handler
                 ;;       for this?
                 (org-protocol-httpd-send-response process 204 "text/plain" "")
                 (org-protocol-check-filename-for-protocol 
		  path
		  nil
		  nil 
		  (lambda () );;(org-protocol-httpd-send-response process 204 "text/plain" "")
		  ))
                ((string-match 
		  (concat "^" org-protocol-httpd-the-protocol "://\\([^:/]+\\)://\\(.*\\)") path)
                 (let ((sub-protocol-string (match-string 1 path))
                       sub-protocol-entry)
                   (when (null sub-protocol-string)
                     (error "Error parsing sub protocol!"))
                   (setq sub-protocol-entry 
			 (assoc sub-protocol-string 
				org-protocol-httpd-protocol-alist))
                   (when (null sub-protocol-entry)
                     (error "No sub protocol." ))
                   (setq sub-protocol-entry (cdr sub-protocol-entry))
                   (let* ((sub-protocol-function    (plist-get sub-protocol-entry :function))
                          (sub-protocol-kill-client (plist-get sub-protocol-entry :kill-client))
                          (sub-protocol-arguments   (match-string 2 path))
                          responseString)  
                     (cond (sub-protocol-kill-client
                            (org-protocol-httpd-send-response process 204 "text/plain" "")
                            (funcall sub-protocol-function sub-protocol-arguments))
                           (t
                            (setq responseString 
                                  (funcall 
                                   sub-protocol-function
                                   sub-protocol-arguments))
                            (org-protocol-httpd-send-response 
			     process 200 
			     (plist-get sub-protocol-entry :mime)
			     responseString))))))
                (t 
                 (org-protocol-httpd-send-response process 400 "text/plain" "")))))
    ;; error handler
    (quit 
     (message "Quit in org-protocol-httpd-filter")
     (when (eq (process-status process) 'open)
       (org-protocol-httpd-send-response process 500 "text/plain" "")))
    (error
     (when (eq (process-status process) 'open)
       (org-protocol-httpd-send-response process 500 "text/plain" ""))
     (message "Caught error in org-protocol-httpd-filter: %s" err))))

(defun org-protocol-httpd-parse-header (header)
  "Parse a http header into an alist by splitting each line at the
first space or \": \"."
  (let* ((lines (split-string header "\r?\n"))
         (firstLine (split-string (car lines) " ")))
    (cons
     (cons (car firstLine) (nth 1 firstLine))
     (mapcar 
      (lambda (line) 
        (let ((line-components (split-string line ": " line)))
          (cons 
           (car line-components) 
           (mapconcat 'identity (cdr line-components) ": "))))
      (cdr lines)))))

(defun org-protocol-httpd-send-response (process status mime string)
  "Send a string as response to the currently processed request."
  (process-send-string 
   process 
   (concat (org-protocol-httpd-generate-header mime status (length string))
	   string))
  (process-send-eof process)
  (delete-process process))

(defun org-protocol-httpd-generate-header (mime status content-length)
  (let ((status-string (cdr (assoc status org-protocol-httpd-status-strings))))
    (when (null mime)
      (error "Unknown mime type"))
    (when (null status-string)
      (error "Unknown status type: %d" status))
    (concat "HTTP/1.1 " 
            (number-to-string status)
            " "
            status-string 
            "\nContent-Type: "
            mime "\n"
            "Content-length: " (number-to-string content-length) "\n"
            "Connection: close\n"
           "\n")))

(defun org-protocol-httpd-escape-xml-attribute-chars (unescaped-string)
  "Escape characters for use in xml attributes."
  (replace-regexp-in-string 
   "\"" "&quot;" 
   (replace-regexp-in-string
    "'" "&apos;"
    (replace-regexp-in-string
     ">" "&gt;"
     (replace-regexp-in-string
      "<" "&lt;"
      (replace-regexp-in-string
       "&" "&amp;"
       unescaped-string)))))
  )

(provide 'org-protocol-httpd)