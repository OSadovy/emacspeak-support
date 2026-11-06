;;; emacspeak-corfu.el --- Speech-enable Corfu -*- lexical-binding: t; -*-
;; Description: Speech-enable Corfu, a modern in-buffer completion interface
;; Keywords: Emacspeak, Audio Desktop, Corfu, completion

;;;   Copyright:
;; This file is not part of GNU Emacs, but the same permissions apply.
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; Corfu is a modern in-buffer completion UI that uses Emacs's native
;; completion engine. This module speech-enables Corfu's UI to provide
;; auditory feedback for completion candidates.

;;; Code:

;;   Required modules:

(eval-when-compile (require 'cl-lib))
(require 'emacspeak-preamble)
(require 'corfu nil 'noerror)

;;;  Silence byte-compiler about special variables:

(defvar corfu--candidates)
(defvar corfu--index)
(defvar corfu--metadata)
(defvar completion-in-region-mode)

;;;  Forward declarations:

(declare-function corfu--update "corfu" (&optional interruptible))

;;;  Map faces to voices:

(voice-setup-add-map
 '((corfu-default voice-smoothen)
   (corfu-current voice-bolden)
   (corfu-bar voice-monotone)
   (corfu-border voice-smoothen)
   (corfu-annotations voice-annotate)
   (corfu-deprecated voice-monotone-extra)))

;;;  Bookkeeping variables for UI state:

(defvar-local emacspeak-corfu--prev-candidate nil
  "Previously spoken candidate.")

(defvar-local emacspeak-corfu--prev-index nil
  "Index of previously spoken candidate.")

;;;  Helper functions:

(defun emacspeak-corfu--current-candidate ()
  "Return the currently selected candidate in Corfu."
  (when (and (bound-and-true-p corfu--candidates)
             (bound-and-true-p corfu--index)
             (>= corfu--index 0)
             (< corfu--index (length corfu--candidates)))
    (nth corfu--index corfu--candidates)))

(defun emacspeak-corfu--candidate-with-annotation ()
  "Return current candidate with its annotation if available."
  (let ((candidate (emacspeak-corfu--current-candidate)))
    (when candidate
      (let* ((metadata (and (boundp 'corfu--metadata) corfu--metadata))
             (annotate (and metadata
                            (completion-metadata-get metadata 'annotation-function)))
             (annotation (and annotate (funcall annotate candidate))))
        (if annotation
            (concat candidate " " (propertize annotation 'personality 'voice-annotate))
          candidate)))))

(defun emacspeak-corfu--speak-candidate ()
  "Speak current candidate with position info."
  (let ((candidate (emacspeak-corfu--candidate-with-annotation))
        (total (length corfu--candidates))
        (index (1+ corfu--index)))
    (when candidate
      (dtk-speak (format "%s %d of %d" candidate index total)))))

(defun emacspeak-corfu--speak-input ()
  "Speak the input being completed, and how many candidates there are."
  (let ((total (length corfu--candidates)))
    (tts-with-punctuations
     'all
     (dtk-speak (format "%s %d candidate%s"
                        (buffer-substring (car completion-in-region--data)
                                          (cadr completion-in-region--data))
                        total (if (= total 1) "" "s"))))))

;;;  Advice interactive commands:

(defadvice corfu-insert (around emacspeak pre act comp)
  "Speak the inserted text."
  (let ((start (point)))
    ad-do-it
    (when (ems-interactive-p)
      (emacspeak-icon 'complete)
      (emacspeak-speak-region start (point)))
    ad-return-value))

(defadvice corfu-quit (after emacspeak pre act comp)
  "Speak quit feedback."
  (when (ems-interactive-p)
    (dtk-stop 'all)
    (emacspeak-icon 'close-object)))

(defadvice corfu-reset (after emacspeak pre act comp)
  "Speak reset feedback."
  (when (ems-interactive-p)
    (emacspeak-icon 'item)
    (dtk-speak "reset")))

(defadvice corfu-insert-separator (after emacspeak pre act comp)
  "Speak separator insertion."
  (when (ems-interactive-p)
    (emacspeak-icon 'item)
    (dtk-speak "separator")))

(defadvice corfu-complete (around emacspeak pre act comp)
  "Speak the completed input."
  (let ((beg (car completion-in-region--data))
        (tick (buffer-chars-modified-tick)))
    ad-do-it
    (when (ems-interactive-p)
      (emacspeak-icon 'complete)
      (when (/= tick (buffer-chars-modified-tick))
        (tts-with-punctuations
         'all (dtk-speak (buffer-substring beg (point))))))
    ad-return-value))

;;; Batch advice for navigation commands:

(cl-loop
 for (f icon) in
 '((corfu-next select-object)
   (corfu-previous select-object)
   (corfu-first large-movement)
   (corfu-last large-movement)
   (corfu-scroll-up scroll)
   (corfu-scroll-down scroll))
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak the newly selected candidate."
     (when (ems-interactive-p)
       (emacspeak-icon ',icon)
       (emacspeak-corfu--speak-candidate)))))

;;;  Advice internal update function to speak candidate changes:

(defadvice corfu--update (after emacspeak pre act comp)
  "Speak candidate updates as they happen."
  (when (and (boundp 'corfu--index)
             (boundp 'corfu--candidates)
             corfu--candidates)
    (let ((new-cand (emacspeak-corfu--current-candidate)))
      (cond
       ;; Opened with nothing selected, because the input is already a valid
       ;; answer -- a completed directory, say -- so the input is the news.
       ((and (null emacspeak-corfu--prev-index) (null new-cand))
        (emacspeak-corfu--speak-input))
       ((not (equal emacspeak-corfu--prev-candidate new-cand))
        (when (and (not (equal corfu--index emacspeak-corfu--prev-index))
                   (>= corfu--index 0))
          (emacspeak-icon 'select-object))
        (when new-cand
          (emacspeak-corfu--speak-candidate))))
      (setq-local
       emacspeak-corfu--prev-candidate new-cand
       emacspeak-corfu--prev-index corfu--index))))

(defadvice corfu--in-region-1 (around emacspeak pre act comp)
  "Speak a completion inserted without opening the popup.
Corfu inserts a sole match directly, from whichever command completed."
  (let ((beg (ad-get-arg 0))
        (tick (buffer-chars-modified-tick)))
    ad-do-it
    (unless (or completion-in-region-mode
                (= tick (buffer-chars-modified-tick)))
      (emacspeak-icon 'complete)
      (tts-with-punctuations
       'all (dtk-speak (buffer-substring beg (point)))))
    ad-return-value))

;;;  Setup hooks:

(defun emacspeak-corfu--completion-hook ()
  "Hook for completion-in-region-mode changes."
  (cond
   (completion-in-region-mode
    (emacspeak-icon 'open-object))
   (t
    (setq-local emacspeak-corfu--prev-candidate nil
                emacspeak-corfu--prev-index nil))))

(defun emacspeak-corfu-setup ()
  "Setup Emacspeak support for Corfu."
  (add-hook 'completion-in-region-mode-hook #'emacspeak-corfu--completion-hook))

(eval-after-load "corfu" #'emacspeak-corfu-setup)

;;;  Enable/Disable support:

(defvar emacspeak-corfu--advice-list
  '((corfu-insert around)
    (corfu-quit after)
    (corfu-reset after)
    (corfu-insert-separator after)
    (corfu-complete around)
    (corfu-next after)
    (corfu-previous after)
    (corfu-first after)
    (corfu-last after)
    (corfu-scroll-up after)
    (corfu-scroll-down after)
    (corfu--update after)
    (corfu--in-region-1 around))
  "List of advised functions for Emacspeak Corfu support.")

(defun emacspeak-corfu-enable ()
  "Enable Emacspeak support for Corfu."
  (interactive)
  (dolist (advice emacspeak-corfu--advice-list)
    (ad-enable-advice (car advice) (cadr advice) 'emacspeak)
    (ad-activate (car advice)))
  (emacspeak-corfu-setup)
  (message "Enabled Emacspeak Corfu support"))

(defun emacspeak-corfu-disable ()
  "Disable Emacspeak support for Corfu."
  (interactive)
  (dolist (advice emacspeak-corfu--advice-list)
    (ad-disable-advice (car advice) (cadr advice) 'emacspeak)
    (ad-activate (car advice)))
  (remove-hook 'completion-in-region-mode-hook #'emacspeak-corfu--completion-hook)
  (message "Disabled Emacspeak Corfu support"))

;;;  Provide the module:

(provide 'emacspeak-corfu)

;;; emacspeak-corfu.el ends here
