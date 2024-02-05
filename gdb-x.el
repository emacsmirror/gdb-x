;;; gdb-x.el --- Improve GDB-MI user interface -*- lexical-binding: t -*-
;;
;; Copyright © 2023 Sergio Pastor Pérez
;;
;; Author: Sergio Pastor Pérez <sergio.pastorperez@outlook.es>
;; Version: 0.0.1
;; URL: https://codeberg.org/shepherd/gdb-x
;; Keywords: extensions
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Extra utilities for improving the user interface of gdb-mi.el.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'gud)
(require 'gdb-mi)
(require 'hl-line)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Recenter buffer when stepping and ensure `hl-line' is updated if enabled. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun gdb-x--gud-source-center (true-file line)
  "Recenter GUD source buffer around the program counter source line.
TRUE-FILE is the GUD file to recenter and LINE is the last source line
executed."
  (let ((bf (gud-find-file true-file)))
    (save-excursion
	  (with-selected-window (get-buffer-window bf)
	    (save-restriction
          (goto-char (point-min)) (forward-line (1- line))
	      (recenter))))))

;; `recenter' GUD source buffer window.
(advice-add #'gud-display-line :after #'gdb-x--gud-source-center)

;; Centre GUD source buffer window and enable `hl-line' if needed.
(defun gdb-x--disassembly-highlight-and-recenter ()
  "Make sure that `hl-line' gets updated after updating disassembly buffer.
Also ensure that the last executed line is centred."
  (let* ((buffer (gdb-get-buffer 'gdb-disassembly-buffer))
	     (window (get-buffer-window buffer 0)))
    (when (and window (featurep 'hl-line))
	  (with-current-buffer buffer
	    (cond
	     (global-hl-line-mode
	      (goto-char gdb-disassembly-position)
	      (global-hl-line-highlight))
	     ((and hl-line-mode hl-line-sticky-flag)
	      (goto-char gdb-disassembly-position)
	      (hl-line-highlight))))
	  (with-selected-window window
	    (recenter)))))

(advice-add #'gdb-disassembly-handler-custom :after
	        #'gdb-x--disassembly-highlight-and-recenter)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Restore window configuration on exit. Taken from             ;;
;; 'https://www.doof.me.uk/2019/06/09/making-emacs-gud-usable'. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst gdb-x--gud-window-register ?c)

(defun gdb-x-gud-quit ()
  "Exit GUD."
  (interactive)
  (gud-basic-call "quit"))

(add-hook 'gud-mode-hook
	      (lambda ()
	        (gud-tooltip-mode)
	        (window-configuration-to-register gdb-x--gud-window-register)))

(advice-add 'gud-sentinel :after
	        (lambda (proc _)
		      (when (memq (process-status proc) '(signal exit))
		        (jump-to-register gdb-x--gud-window-register)
		        (bury-buffer))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Many windows arrangement. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun gdb-x--display-side-buffer (buf direction slot width)
  "Show buffer BUF, and make that window a side window.
Read `display-buffer' for more information on the meaning of DIRECTION, SLOT and
WIDTH."
  (let ((buf-mode (with-current-buffer buf
		            major-mode)))
    (display-buffer-in-side-window buf
				                   `((mode . ,buf-mode)
				                     (side . ,direction)
				                     (slot . ,slot)
				                     (window-width . ,width)
				                     (window-parameters
					                  (no-delete-other-windows . t))))))

(defun gdb-x--display-locals-buffer (&optional thread)
  "Display the local variables of current GDB stack.
Read `gdb-get-buffer-create' for more information on the meaning of THREAD."
  (interactive)
  (gdb-x--display-side-buffer (gdb-get-buffer-create 'gdb-locals-buffer thread)
			                'left
			                0
			                0.15))

(defun gdb-x--display-stack-buffer (&optional thread)
  "Display GDB backtrace for current stack.
Read `gdb-get-buffer-create' for more information on the meaning of THREAD."
  (interactive)
  (gdb-x--display-side-buffer (gdb-get-buffer-create 'gdb-stack-buffer thread)
			                'left
			                1
			                0.15))

(defun gdb-x--display-breakpoints-buffer (&optional thread)
  "Display GDB breakpoints.
Read `gdb-get-buffer-create' for more information on the meaning of THREAD."
  (interactive)
  (gdb-x--display-side-buffer (gdb-get-buffer-create 'gdb-breakpoints-buffer thread)
			                'left
			                2
			                0.15))

(defun gdb-x--display-gdb-buffer ()
  "Display GUD buffer."
  (interactive)
  (gdb-x--display-side-buffer gud-comint-buffer
			                'bottom
			                0
			                0.5))

(defun gdb-x--display-io-buffer ()
  "Display IO of debugged program in a separate window."
  (interactive)
  (gdb-x--display-side-buffer (gdb-get-buffer-create 'gdb-inferior-io)
			                'bottom
			                1
			                0.5))

(defun gdb-x--get-non-dedicated-windows ()
  "Get Emacs windows that are not dedicated windows."
  (seq-filter #'(lambda (win) (not (window-dedicated-p win))) (window-list)))

(defun gdb-x--display-atom-buffer (buf direction slot width)
  "Show buffer BUF, and make that window a side window.
Read `display-buffer' for more information on the meaning of DIRECTION, SLOT and
WIDTH."
  (let ((buf-mode (with-current-buffer buf
		            major-mode))
	    (parent (or (car gdb-source-window-list)
		            (car (gdb-x--get-non-dedicated-windows)))))
    (with-selected-window (display-buffer-in-atom-window buf
				                                         `((mode . ,buf-mode)
                                                           (dedicated . t)
				                                           (side . ,direction)
				                                           (slot . ,slot)
				                                           (window . ,parent)
				                                           (window-width . ,width)))
      (window-preserve-size nil t t)))) ; Ensure that the window is not resized horizontally.

(defun gdb-x--display-disassembly-buffer (&optional thread)
  "Display GDB disassembly information.
Read `gdb-get-buffer-create' for more information on the meaning of THREAD."
  (interactive)
  (gdb-x--display-side-buffer (gdb-get-buffer-create 'gdb-disassembly-buffer thread)
			                'right
			                0
			                80))

;;;###autoload
(define-minor-mode gdb-x-many-windows-mode
  "Minor mode to toggle the display of all relevant GUD side windows."
  :global t
  :group 'gdb-mi
  :init-value nil ;; Don't enable the mode by default
  :lighter " gdb-many"
  (if gdb-x-many-windows-mode
      (progn
        (when-let (gdb-src-buf (gdb-get-source-buffer))
          (display-buffer-full-frame gdb-src-buf nil)
          (setq gdb-source-window-list (list
                                        (get-buffer-window gdb-src-buf))))
        (gdb-x--display-breakpoints-buffer)
        (gdb-x--display-disassembly-buffer)
        (gdb-x--display-locals-buffer)
        (gdb-x--display-stack-buffer)
        (gdb-x--display-io-buffer)
        (select-window (gdb-x--display-gdb-buffer)))
    (let ((window--sides-inhibit-check t))
      (set-frame-parameter
       nil 'window-state (window-state-get (frame-root-window nil)))
      (let ((ignore-window-parameters t))
        (delete-other-windows (window-main-window nil))
        (select-window (gdb-x--display-gdb-buffer))))))


(provide 'gdb-x)
;;; gdb-x.el ends here.
