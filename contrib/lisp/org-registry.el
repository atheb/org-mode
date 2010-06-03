;;; org-registry.el --- a registry for Org links
;;
;; Copyright 2007, 2008 Bastien Guerry
;;
;; Emacs Lisp Archive Entry
;; Filename: org-registry.el
;; Version: 0.1a
;; Author: Bastien Guerry <bzg AT altern DOT org>
;; Maintainer: Bastien Guerry <bzg AT altern DOT org>
;; Keywords: org, wp, registry
;; Description: Shows Org files where the current buffer is linked
;; URL: http://www.cognition.ens.fr/~guerry/u/org-registry.el
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;
;; This library add a registry to your Org setup.
;;
;; Org files are full of links inserted with `org-store-link'. This links
;; point to e-mail, webpages, files, dirs, info pages, man pages, etc.
;; Actually, they come from potentially *everywhere* since Org lets you
;; define your own storing/following functions.
;;
;; So, what if you are on a e-mail, webpage or whatever and want to know if
;; this buffer has already been linked to somewhere in your agenda files?
;;
;; This is were org-registry comes in handy.
;;
;;     M-x org-registry-show will tell you the name of the file
;; C-u M-x org-registry-show will directly jump to the file
;;
;; In case there are several files where the link lives in:
;;
;;     M-x org-registry-show will display them in a new window
;; C-u M-x org-registry-show will prompt for a file to visit
;;
;; Add this to your Org configuration:
;;
;; (require 'org-registry)
;; (org-registry-initialize)
;;
;; If you want to update the registry with newly inserted links in the
;; current buffer: M-x org-registry-update
;;
;; If you want this job to be done each time you save an Org buffer,
;; hook 'org-registry-update to the local 'after-save-hook in org-mode:
;;
;; (org-registry-insinuate)

;;; Code:

(eval-when-compile
  (require 'cl))

(defgroup org-registry nil
  "A registry for Org."
  :group 'org)

(defcustom org-registry-file
  (concat (getenv "HOME") "/.org-registry.el")
  "The Org registry file."
  :group 'org-registry
  :type 'file)

(defcustom org-registry-find-file 'find-file-other-window
  "How to find visit files."
  :type 'function
  :group 'org-registry)

(defvar org-registry-alist nil
  "An alist containing the Org registry.")

(defvar org-registry-get-entries-hook nil
  "A hook that is called after the links in a file have been collected.")

;;;###autoload
(defun org-registry-show (&optional visit)
  "Show Org files where there are links pointing to the current
buffer."
  (interactive "P")
  ;;  (org-registry-initialize)
  (let* ((blink (or (org-remember-annotation) ""))
	 (link (when (string-match org-bracket-link-regexp blink)
	         (match-string-no-properties 1 blink)))
	 (desc (or (and (string-match org-bracket-link-regexp blink)
	        	(match-string-no-properties 3 blink)) "No description"))
	 (files (org-registry-assoc-all link))
	 file point selection tmphist)
    (cond ((and files visit)
	   ;; result(s) to visit
	   (cond ((< 1 (length files))
		  ;; more than one result
		  (setq tmphist (mapcar (lambda(entry)
					  (format "%s (%d) [%s]"
						  (nth 3 entry) ; file
						  (nth 2 entry) ; point
						  (nth 1 entry))) files))
		  (setq selection (completing-read "File: " tmphist
						   nil t nil 'tmphist))
		  (string-match "\\(.+\\) (\\([0-9]+\\))" selection)
		  (setq file (match-string 1 selection))
		  (setq point (string-to-number (match-string 2 selection))))
		 ((eq 1 (length files))
		  ;; just one result
		  (setq file (nth 3 (car files)))
		  (setq point (nth 2 (car files)))))
	   ;; visit the (selected) file
	   (funcall org-registry-find-file file)
	   (goto-char point)
	   (unless (org-before-first-heading-p)
	     (org-show-context)))
	  ((and files (not visit))
	   ;; result(s) to display
	   (cond  ((eq 1 (length files))
		   ;; show one file
		   (message "Link in file %s (%d) [%s]"
			    (nth 3 (car files))
			    (nth 2 (car files))
			    (nth 1 (car files))))
		  (t (org-registry-display-files files link))))
	  (t (message "No link to this in org-agenda-files")))))

(defun org-registry-display-files (files link)
  "Display files in a separate window."
  (switch-to-buffer-other-window
   (get-buffer-create " *Org registry info*"))
  (erase-buffer)
  (insert (format "Files pointing to %s:\n\n" link))
  (let (file)
    (while (setq file (pop files))
      (insert (format "%s (%d) [%s]\n" (nth 3 file)
		      (nth 2 file) (nth 1 file)))))
  (shrink-window-if-larger-than-buffer)
  (other-window 1))

(defun org-registry-assoc-all (link &optional registry)
  "Return all associated entries of LINK in the registry."
  (when (file-exists-p link)
    (setq link (expand-file-name link)))
  (org-registry-find-all 
   (lambda (entry) (string= link (car entry)))
   registry))

(defun org-registry-find-all (test &optional registry)
  "Return all entries satisfying `test' in the registry."
  (delq nil 
        (mapcar 
         (lambda (x) (and (funcall test x) x)) 
         (or registry org-registry-alist))))

;;;###autoload
(defun org-registry-visit ()
  "If an Org file contains a link to the current location, visit
this file."
  (interactive)
  (org-registry-show t))

;;;###autoload
(defun org-registry-initialize (&optional from-scratch)
  "Initialize `org-registry-alist'.
If FROM-SCRATCH is non-nil or the registry does not exist yet,
create a new registry from scratch and eval it. If the registry
exists, eval `org-registry-file' and make it the new value for
`org-registry-alist'."
  (interactive "P")
  (cond ((or from-scratch (not (file-exists-p org-registry-file)))
         ;; create a new registry
         (setq org-registry-alist nil)
         (mapc
          (lambda (file)
            (setq org-registry-alist 
                  (nconc 
                   org-registry-alist 
                   (org-registry-get-entries (expand-file-name file)))))
          (org-agenda-files))
         (when from-scratch
           (org-registry-create org-registry-alist)))
        (t ;; eval the registry file
         (with-temp-buffer
           (insert-file-contents org-registry-file)
           (eval-buffer)))))

;;;###autoload
(defun org-registry-insinuate ()
  "Call `org-registry-update' after saving in Org-mode.
Use with caution.  This could slow down things a bit."
  (interactive)
  (add-hook 'org-mode-hook
	    (lambda() (add-hook 'after-save-hook
				'org-registry-update t t))))

(defun org-registry-get-entries (file)
  "List Org links in FILE that will be put in the registry."
  (let (bufstr result inside-fstree-block)
    (with-temp-buffer
      ;;(buffer-disable-undo)
      (setq buffer-undo-list t)
      (insert-file-contents file)
      ;; need to set org-mode in order for org-heading-components to
      ;; work
      (org-mode)
      (goto-char (point-min))
      (setq inside-fstree-block nil)
      (while (re-search-forward (org-registry-regexp-or-fstree-block org-angle-link-re) nil t)
        (let ((match-end-point (match-end 0)))
          (goto-char (match-beginning 0))
          (cond ((looking-at "^#\\+BEGIN: +fstree")
                 (setq inside-fstree-block t)
                 (goto-char match-end-point))
                ((looking-at "^#\\+END")
                 (when inside-fstree-block
                   (setq inside-fstree-block nil))
                 (goto-char match-end-point))
                ((looking-at org-angle-link-re)
                 (let* ((point (match-beginning 0))
                        (link (match-string-no-properties 0))
                        (desc (or (match-string-no-properties 0) "No description"))
                        heading-components heading-id)
                   (save-excursion 
                     (unless (org-before-first-heading-p)
                       ;;(org-back-to-heading)
                       (setq heading-components (org-heading-components))
                       (setq heading-id (org-id-get)))
                     (setq result (cons 
                                   (list link desc point file heading-components (point) inside-fstree-block)
                                   result)))
                   (goto-char match-end-point))))))
      (goto-char (point-min))
      (setq inside-fstree-block nil)
      (while (re-search-forward (org-registry-regexp-or-fstree-block org-bracket-link-regexp) nil t)
        (let ((match-end-point (match-end 0)))
          (goto-char (match-beginning 0))
          (cond ((looking-at "^#\\+BEGIN: +fstree")
                 (setq inside-fstree-block t)
                 (goto-char match-end-point))
                ((looking-at "^#\\+END")
                 (when inside-fstree-block
                   (setq inside-fstree-block nil))
                 (goto-char match-end-point))
                ((looking-at org-bracket-link-regexp)
                 (let* ((point (match-beginning 0))
                        (link (match-string-no-properties 1))
                        (desc (or (match-string-no-properties 3) "No description"))
                        heading-components heading-id)
                   (save-excursion 
                     (unless (org-before-first-heading-p)
                       ;;(org-back-to-heading)
                       (setq heading-components (org-heading-components))
                       (setq heading-id (org-id-get)))
                     (setq result (cons 
                                   (list link desc point file heading-components (point) inside-fstree-block)
                                   result)))
                   (goto-char match-end-point))))))
      (goto-char (point-min))
      (let (returnList)
        ;; because the return value of run-hook-with-args cannot be trusted
        ;; use returnList instead
        (run-hook-with-args 'org-registry-get-entries-hook file)
        (setq result (nconc result returnList))
        ))
    (message "Get entries finished for file: %s" file)
    result))

;;;###autoload
(defun org-registry-update ()
  "Update the registry for the current Org file."
  (interactive)
  (unless (org-mode-p) (error "Not in org-mode"))
  (let* ((from-file (expand-file-name (buffer-file-name))))
    (when
	(member 
	 (file-truename from-file) 
	 (mapcar 
	  (lambda (file) (file-truename file)) 
	  (org-agenda-files)))
      (let* ((new-entries (org-registry-get-entries from-file))
	     (old-entries-for-other-files
	      (org-registry-find-all 
	       (lambda (entry) 
		 (not (string= (nth 3 entry) from-file))))))
	(setq org-registry-alist 
	      (nconc
	       new-entries
	       old-entries-for-other-files))
	(org-registry-create org-registry-alist)))))

(defun org-registry-create (entries)
  "Create `org-registry-file' with ENTRIES."
  (let (entry)
    (with-temp-buffer
      ;;(buffer-disable-undo)
      (setq buffer-undo-list t)
      (find-file org-registry-file)
      (erase-buffer)
      (insert
       (with-output-to-string
	 (princ ";; -*- emacs-lisp -*-\n")
	 (princ ";; Org registry\n")
	 (princ ";; You shouldn't try to modify this buffer manually\n\n")
	 (princ "(setq org-registry-alist\n'(\n")
	 (while entries
	   (when (setq entry (pop entries))
	     (prin1 entry)
	     (princ "\n")))
	 (princ "))\n")))
      (save-buffer)
      (kill-buffer (current-buffer))))
  (message "Org registry created"))

(defun org-registry-regexp-or-fstree-block (regexp)
  "Generates a regexp that matches either `regexp' or an org-fstree
block."
  (concat "\\(" regexp "\\|^#\\+BEGIN: +fstree\\|^#\\+END\\)"))

(provide 'org-registry)

;;; org-registry.el ends here
