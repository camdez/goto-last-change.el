;;; goto-last-change.el --- Move point through buffer-undo-list positions  -*- lexical-binding: t; -*-

;; Copyright © 2003 Kevin Rodgers

;; Author: Kevin Rodgers <ihs_4664@yahoo.com>
;; Created: 17 Jun 2003
;; Version: 1.3.0
;; Package-Requires: ((emacs "24.3"))
;; Keywords: convenience
;; Homepage: https://github.com/camdez/goto-last-change.el

;; Contributors:
;;   Attila Lendvai <attila.lendvai@gmail.com> (line distance and auto marks)

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; After installing goto-last-change.el in a `load-path' directory and
;; compiling it with `M-x byte-compile-file', load it with
;;      (require 'goto-last-change)
;; or autoload it with
;;      (autoload 'goto-last-change "goto-last-change"
;;        "Set point to the position of the last change." t)
;;
;; You may also want to bind a key to `M-x goto-last-change', e.g.
;;      (global-set-key "\C-x\C-\\" 'goto-last-change)

;; goto-last-change.el was written in response to the following:
;;
;; From: Dan Jacobson <jidanni@jidanni.org>
;; Newsgroups: gnu.emacs.bug
;; Subject: function to go to spot of last change
;; Date: Sun, 15 Jun 2003 00:15:08 +0000 (UTC)
;; Sender: news <news@main.gmane.org>
;; Message-ID: <mailman.7910.1055637181.21513.bug-gnu-emacs@gnu.org>
;; NNTP-Posting-Host: monty-python.gnu.org
;;
;;
;; Why of course, a function to get the user to the spot of last changes
;; in the current buffer(s?), that's what emacs must lack.
;;
;; How many times have you found yourself mosying [<-not in spell
;; checker!?] thru a file when you wonder, where the heck was I just
;; editing?  Well, the best you can do is hit undo, ^F, and undo again,
;; to get back.  Hence the "burning need" for the additional function,
;; which you might name the-jacobson-memorial-function, due to its brilliance.
;; --
;; http://jidanni.org/ Taiwan(04)25854780

;;; Code:

(defgroup goto-last-change nil
  "Move point through `buffer-undo-list' positions."
  :group 'convenience)

(defcustom goto-last-change-minimal-line-distance 10
  "Default minimal line distance for consecutive \\[goto-last-change] calls.
Consecutive calls skip changes fewer than this many lines from
point.  The first call always goes to the last change."
  :type 'natnum
  :group 'goto-last-change)

(defvar goto-last-change-undo nil
  "The `buffer-undo-list' entry of the previous \\[goto-last-change] command.")
(make-variable-buffer-local 'goto-last-change-undo)

;;;###autoload
(defun goto-last-change (&optional mark-point minimal-line-distance)
  "Set point to the position of the last change.
Consecutive calls set point to the position of the previous change.
With a prefix arg (optional arg MARK-POINT non-nil), set mark so \
\\[exchange-point-and-mark]
will return point to the current position.
MINIMAL-LINE-DISTANCE defaults to `goto-last-change-minimal-line-distance'."
  (interactive "P")
  (when (eq buffer-undo-list t)
    (user-error "No undo information in this buffer"))
  (when mark-point
    (push-mark))
  (unless minimal-line-distance
    (setq minimal-line-distance goto-last-change-minimal-line-distance))
  (let (position
        undo
        (undo-list (if (and (eq this-command last-command)
                            goto-last-change-undo)
                       (cdr (memq goto-last-change-undo buffer-undo-list))
                     buffer-undo-list)))
    (while (and undo-list
                (or (not position)
                    (eql position (point))
                    (and minimal-line-distance
                         ;; The first invocation always goes to the
                         ;; last change, subsequent ones skip changes
                         ;; closer (to point) than `minimal-line-distance'.
                         (memq last-command '(goto-last-change
                                              goto-last-change-with-auto-marks))
                         (< (count-lines (min position (point-max)) (point))
                            minimal-line-distance))))
      (setq undo (car undo-list))
      (cond ((and (consp undo) (integerp (car undo)) (integerp (cdr undo)))
             ;; (BEG . END)
             (setq position (cdr undo)))
            ((and (consp undo) (stringp (car undo))) ; (TEXT . POSITION)
             (setq position (abs (cdr undo))))
            ((and (consp undo) (eq (car undo) t))) ; (t HIGH . LOW)
            ((and (consp undo) (null (car undo)))
             ;; (nil PROPERTY VALUE BEG . END)
             (setq position (cdr (last undo))))
            ((and (consp undo) (eq (car undo) 'apply))
             ;; (apply DELTA BEG END FUN-NAME . ARGS) or (apply FUN-NAME . ARGS)
             (when (integerp (cadr undo))
               (setq position (nth 3 undo))))
            ((and (consp undo) (markerp (car undo)))) ; (MARKER . DISTANCE)
            ((integerp undo))                         ; POSITION
            ((null undo))                             ; nil
            (t (error "Invalid undo entry: %s" undo)))
      (setq undo-list (cdr undo-list)))
    (cond (position
           (setq goto-last-change-undo undo)
           (goto-char (min position (point-max))))
          ((and (eq this-command last-command)
                goto-last-change-undo)
           (setq goto-last-change-undo nil)
           (user-error "No further undo information"))
          (t
           (setq goto-last-change-undo nil)
           (user-error "Buffer not modified")))))

;;;###autoload
(defun goto-last-change-with-auto-marks (&optional minimal-line-distance)
  "Call `goto-last-change', setting the mark only on the first of a series.
MINIMAL-LINE-DISTANCE is passed to `goto-last-change'; interactively,
it is the prefix argument."
  (interactive (list (and current-prefix-arg
                          (prefix-numeric-value current-prefix-arg))))
  (goto-last-change (not (or (eq last-command 'goto-last-change-with-auto-marks)
                             (eq last-command t)))
                    minimal-line-distance))

(provide 'goto-last-change)
;;; goto-last-change.el ends here
