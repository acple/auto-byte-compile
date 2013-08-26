;;; auto-byte-compile.el --- auto byte compile

;;; Code:

(require 'bytecomp)

(defcustom auto-byte-compile-no-ask nil
  "auto-byte-compile"
  :type 'boolean
  :group 'auto-byte-compile)

(defvar auto-byte-compile-buffer "*auto-byte-compile*")

(defun auto-byte-compile ()
  (let ((file (buffer-file-name)))
    (and (string-match "\\.el$" file)
         (not no-byte-compile)
         (or auto-byte-compile-no-ask
             (file-exists-p (byte-compile-dest-file file))
             (y-or-n-p (format "Compile \"%s\" ?" (buffer-name))))
         (auto-byte-compile-run file))))

(defun auto-byte-compile-run (file)
  (let* ((inhibit-quit t)
         (process-connection-type nil)
         (process (start-process
                   (file-relative-name file)
                   (get-buffer-create auto-byte-compile-buffer)
                   "emacs" "-batch" "-eval"
                   "(batch-byte-compile)"
                   file)))
    (set-process-sentinel process 'auto-byte-compile-process-sentinel)
    (set-process-filter process 'auto-byte-compile-process-filter)
    (set-process-query-on-exit-flag process nil)))

(defun auto-byte-compile-process-sentinel (process state)
  (if (string= state "finished\n")
      (message "auto-byte-compile: \"%s\" compiled successfully."
               (process-name process))
    (message "auto-byte-compile: \"%s\" %s"
             (process-name process)
             (substring state 0 -1))))

(defun auto-byte-compile-process-filter (process output)
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (goto-char (process-mark process))
      (princ (format "%s\nauto byte compile: \"%s\"\n%s%s\n\n"
                     (make-string 80 ?=)
                     (process-name process)
                     output
                     (current-time-string))
             (process-buffer process)))))

(add-hook 'after-save-hook 'auto-byte-compile)

(provide 'auto-byte-compile)
