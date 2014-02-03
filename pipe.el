;;; pipe.el --- pipes with emacslisp  --- -*- lexical-binding: t -*-

;; Copyright (C) 2014  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: processes, comm
;; Version: 0.0.1
;; Url: https://github.com/nicferrier/emacs-pipe
;; Package-requires: ((noflet "0.0.11"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Pipe functions to help you have processes communicate with
;; EmacsLisp more easily.

;;; Code:

(require 'noflet)

(defun pipe/thunk-bind (thunk &optional sym)
  "Return a function (&rest args) which calls THUNK.

When THUNK executes a function SYM (or `thuncall') is bound which
returns `args'.

Eg:

 (funcall
   (pipe/thunk-bind (lambda ()
                      (apply 'print (thuncall))))
   \"one two three\")

prints the string \"one two three\"."
  (let* ((fnsym (or sym 'thuncall))
         (old-func (condition-case err 
                       (symbol-function fnsym)
                     (void-function nil))))
    (lambda (&rest args)
      (unwind-protect ; we have to make our own flet
           (progn
             (fset fnsym (lambda (&rest any) args))
             (funcall thunk))
        (if old-func
            (fset fnsym old-func)
            (fmakunbound fnsym))))))

(defun* pipe/buffer-lines (buffer &key (line-ending "\n") (delete t))
  "Return a list of lines from BUFFER or `nil' if there are none.

`:line-ending' may be specified but it \"\n\" by default.

If `:delete' is specified then the region defining the lines is
deleted.  `:delete' defaults to `t'"
  (catch :escape
    (with-current-buffer buffer
      (let* ((last (save-excursion
                     (save-match-data
                       (goto-char (point-max))
                       (+ (length line-ending)
                          (or
                           (re-search-backward line-ending nil t)
                           (throw :escape nil)))))))
        (split-string 
         (prog1
             (buffer-substring (point-min) last)
           (when delete (delete-region (point-min) last))) 
         line-ending t)))))

(defun pipe-shell-command (command thunk &optional name)
  "Pipe the shell COMMAND to the THUNK function.

Within THUNK the function `pipe-read' can be called to return the
next value from the pipe."
  (let* ((prcname (or name (format "*%s*" "proc-receive"))) ; FIXME -uniqufy
         (proc (start-process-shell-command prcname (concat " " prcname) command))
         eof)
    (nolexflet ((thunk-bind (proc) ; bind the proc to the thunk to allow eof handling
                  (pipe/thunk-bind
                   (lambda ()
                     (noflet ((pipe-read ()
                                (if (not eof)
                                    (funcall this-fn)
                                    ;; Else it's eof - mark the process and throw
                                    (process-put proc :eof :eof)
                                    (throw :eof :eof))))
                       (funcall thunk)))
                   'pipe-read)))
      (set-process-filter
        proc (lambda (fproc data)
               (with-current-buffer (process-buffer fproc)
                 (save-excursion
                   (goto-char (point-max))
                   (insert data)))
               (let ((lines (pipe/buffer-lines (process-buffer fproc))))
                 (when lines (-each lines (thunk-bind fproc))))))
      (set-process-sentinel
        proc (lambda (sproc status)
               (case (intern (car (split-string status)))
                 ((exited finished)
                  (setq eof t)
                  (funcall (thunk-bind sproc))))))
      proc)))

(defun pipe-eof (proc)
  "Has PROC been marked EOF?"
  (eq :eof (process-get proc :eof)))

(defvar pipe-eof-wait-time 0.1
  "The time that `wait-for-pipe-eof' delays while waiting.")

(defmacro* wait-for-pipe-eof (proc &rest body)
  "Wait for EOF on the PROC and then execute BODY."
  (declare (indent 1))
  (let ((procv (make-symbol "procv")))
    `(let ((,procv ,proc))
       (while (not (pipe-eof ,procv)) (sleep-for pipe-eof-wait-time))
       (progn ,@body))))

(defmacro pipe (command &rest body)
  "Pipe COMMAND through BODY.

Inside BODY the function `pipe-read' takes a function argument
which will be called with a data from COMMAND or `:eof' will be
thrown."
  (declare (indent 1))
  (let ((cmdvar (make-symbol "cmdvar")))
    `(let ((,cmdvar ,command))
       (if (stringp ,cmdvar)
           (pipe-shell-command ,cmdvar (lambda () ,@body))))))

(defun pipe-demo ()
  "Demonstrate the `pipe' macro."
  (let* (lst)
    (wait-for-pipe-eof
        (pipe "ls -la ~/"
          (catch :eof
            (push (car (pipe-read)) lst)))
      lst)))

(defun pipe-imaginary-demo ()
  ;; This WILL NOT WORK YET
  (pipe
      (progn
        (maildir/imap-check-connect)
        (let ((maildir/imap-log (get-buffer-create "*maildir-imap-log*"))
              (maildir/imap-message-doit doit))
          ;; pipe-send would automatically send back to this pipe
          (pipe-send (mapcar 'maildir/imap-message (maildir/imap-search)))))
    (catch :eof
      (apply 'maildir/update (pipe-read)))))


(let (collect)
  (wait-for-pipe-eof
      (pipe "ls -la ~/work"
        (catch :eof
          (-each 
           (->> (pipe-read)
             (-keep (lambda (a) a)))
           (lambda (a) (push a collect)))))
    collect))

(provide 'pipe)

;;; pipe.el ends here
