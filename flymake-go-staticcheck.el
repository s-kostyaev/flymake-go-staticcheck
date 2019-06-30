;;; flymake-go-staticcheck.el --- Go staticcheck linter for flymake  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sergey Kostyaev

;; Author: Sergey Kostyaev <feo.me@ya.ru>
;; Version: 0.1.0
;; Keywords: languages, tools
;;
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

;;; URL: https://github.com/s-kostyaev/flymake-go-staticcheck

;;; Package-Requires: ((emacs "25"))

;;; Commentary:

;; A Flymake backend for Go using staticcheck.

;;; Code:

(defgroup flymake-go-staticcheck nil
  "Flymake backend for Go using staticcheck"
  :group 'programming
  :prefix "flymake-go-staticcheck-")

(defcustom flymake-go-staticcheck-executable "staticcheck"
  "Name or path to executable for staticcheck linter."
  :type 'string
  :group 'flymake-go-staticcheck)

(defcustom flymake-go-staticcheck-executable-args nil
  "Extra arguments to pass to staticcheck."
  :type 'string
  :group 'flymake-go-staticcheck)

(defvar flymake-go-staticcheck--message-regex "^\\([^:]*\\):\\([0-9]+\\):\\([0-9]*\\):[[:space:]]*\\(.*\\)"
  "Internal variable.
Regular expression definition to match staticcheck messages.")

(defvar-local flymake-go-staticcheck--process nil
  "Internal variable.
Handle to the linter process for the current buffer.")

(defun flymake-go-staticcheck--ensure-binary-exists ()
  "Internal function.
Throw an error and tell REPORT-FN to disable itself if `flymake-go-staticcheck-executable' can't be found."
  (unless (executable-find flymake-go-staticcheck-executable)
    (error (message "can't find '%s' in exec-path - try M-x set-variable flymake-go-staticcheck-executable maybe?"
                    flymake-go-staticcheck-executable))))

(defun flymake-go-staticcheck--report (staticcheck-stdout-buffer source-buffer)
  "Create Flymake diag messages from contents of STATICCHECK-STDOUT-BUFFER, to be reported against SOURCE-BUFFER.
Return a list of results."
  (with-current-buffer staticcheck-stdout-buffer
    (goto-char (point-min))
    (let ((results '()))
      (while (not (eobp))
        (when (looking-at flymake-go-staticcheck--message-regex)
          (let* ((filename (match-string 1))
                 (lineno (string-to-number (match-string 2)))
                 (column (string-to-number (match-string 3)))
                 (msg (match-string 4))
                 (src-pos (flymake-diag-region source-buffer lineno column)))
            (if (string= (buffer-file-name source-buffer)
                         (expand-file-name filename))
                (push (flymake-make-diagnostic source-buffer
                                               (car src-pos)
                                               (min (buffer-size source-buffer) (cdr src-pos))
                                               :error
                                               msg)
                      results))))
        (forward-line 1))
      results)))

(defun flymake-go-staticcheck--create-process (source-buffer callback)
  "Internal function.
Create linter process for SOURCE-BUFFER which invokes CALLBACK once linter is finished.
CALLBACK is passed one argument, which is a buffer containing stdout from linter."
  (when (process-live-p flymake-go-staticcheck--process)
    (kill-process flymake-go-staticcheck--process))
  (setq flymake-go-staticcheck--process
        (make-process
         :name "flymake-go-staticcheck"
         :connection-type 'pipe
         :noquery t
         :buffer (generate-new-buffer " *flymake-go-staticcheck*")
         :command (list flymake-go-staticcheck-executable
                        (or flymake-go-staticcheck-executable-args "")
                        (buffer-file-name source-buffer))
         :sentinel (lambda (proc &rest ignored)
                     (when (and (eq 'exit (process-status proc))
                                (with-current-buffer source-buffer (eq proc flymake-go-staticcheck--process)))
                       (let ((proc-buffer (process-buffer proc)))
                         (funcall callback proc-buffer)
                         (kill-buffer proc-buffer)))))))

(defun flymake-go-staticcheck--check-and-report (source-buffer flymake-report-fn)
  "Internal function.
Run go-staticcheck against SOURCE-BUFFER and use FLYMAKE-REPORT-FN to report results."
  (flymake-go-staticcheck--create-process
   source-buffer
   (lambda (go-staticcheck-stdout)
     (funcall flymake-report-fn (flymake-go-staticcheck--report go-staticcheck-stdout source-buffer))))
  (with-current-buffer source-buffer
    ;; (process-send-string flymake-go-staticcheck--process (buffer-string))
    ;; (process-send-eof flymake-go-staticcheck--process)
    ))

(defun flymake-go-staticcheck--checker (flymake-report-fn &rest ignored)
  "Internal function.
Run go-staticcheck on the current buffer, and report results using FLYMAKE-REPORT-FN.  All other parameters are currently IGNORED."
  (flymake-go-staticcheck--check-and-report (current-buffer) flymake-report-fn))

;;;###autoload
(defun flymake-go-staticcheck-enable ()
  "Enable Flymake and add flymake-go-staticcheck as a buffer-local Flymake backend."
  (interactive)
  (flymake-go-staticcheck--ensure-binary-exists)
  (flymake-mode t)
  (add-hook 'flymake-diagnostic-functions 'flymake-go-staticcheck--checker nil t))


(provide 'flymake-go-staticcheck)
;;; flymake-go-staticcheck.el ends here
