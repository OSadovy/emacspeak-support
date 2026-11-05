;;; emacspeak-corfu-tests.el --- Tests for Corfu speech -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Speech tests for emacspeak-corfu, run against the installed Corfu with its
;; popup drawing stubbed out: batch Emacs has no frames to draw it in.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'corfu)
(require 'emacspeak-corfu)

(defvar corfu--candidates)
(defvar corfu--index)
(defvar corfu--metadata)

(defmacro emacspeak-corfu-test--recording (&rest body)
  "Run BODY, returning what it spoke and the icons it played, in order."
  (declare (indent 0))
  `(let (events)
     (cl-letf (((symbol-function 'dtk-speak)
                (lambda (text) (push (list 'speak (substring-no-properties text))
                                     events)))
               ((symbol-function 'emacspeak-icon)
                (lambda (icon) (push (list 'icon icon) events)))
               ((symbol-function 'corfu--popup-support-p) #'always)
               ((symbol-function 'corfu--popup-show) #'ignore)
               ((symbol-function 'corfu--popup-hide) #'ignore))
       ,@body)
     (nreverse events)))

(ert-deftest emacspeak-corfu-speaks-candidate-before-position ()
  (let ((corfu--candidates '("print" "printf"))
        (corfu--index 1)
        (corfu--metadata nil))
    (should (equal (emacspeak-corfu-test--recording
                     (emacspeak-corfu--speak-candidate))
                   '((speak "printf 2 of 2"))))))

(ert-deftest emacspeak-corfu-speaks-annotation-between-candidate-and-position ()
  (let ((corfu--candidates '("print" "printf"))
        (corfu--index 0)
        (corfu--metadata
         `(metadata (annotation-function . ,(lambda (_) "<builtin>")))))
    (should (equal (emacspeak-corfu-test--recording
                     (emacspeak-corfu--speak-candidate))
                   '((speak "print <builtin> 1 of 2"))))))

(defun emacspeak-corfu-test--complete (input candidates)
  "Insert INPUT in the current buffer and complete it from CANDIDATES.
Completion goes through Corfu, as `corfu-mode' arranges."
  (insert input)
  (setq-local completion-in-region-function #'corfu--in-region)
  (let ((completion-at-point-functions
         (list (lambda () (list (- (point) (length input)) (point) candidates)))))
    (completion-at-point)))

(defun emacspeak-corfu-test--open (input candidates)
  "Insert INPUT in the current buffer and open Corfu on CANDIDATES.
Corfu computes its candidates after the command that opened it, which a
test is not; `corfu--exhibit' is what its post-command hook runs."
  (emacspeak-corfu-test--complete input candidates)
  (corfu--exhibit)
  (should completion-in-region-mode))

(ert-deftest emacspeak-corfu-speaks-a-sole-match-inserted-without-popup ()
  (with-temp-buffer
    (let ((events
           (emacspeak-corfu-test--recording
             (emacspeak-corfu-test--complete "pri" '("print")))))
      (should (equal (buffer-string) "print"))
      (should-not completion-in-region-mode)
      (should (equal events '((icon complete) (speak "print")))))))

(ert-deftest emacspeak-corfu-leaves-an-opened-popup-to-announce-itself ()
  (with-temp-buffer
    (let ((events
           (emacspeak-corfu-test--recording
             (emacspeak-corfu-test--complete "os.pat" '("os.path" "os.pathsep")))))
      (should (equal (buffer-string) "os.path"))
      (should completion-in-region-mode)
      (should (equal events '((icon open-object)))))))

(ert-deftest emacspeak-corfu-complete-speaks-the-completed-input ()
  (with-temp-buffer
    (let ((events
           (emacspeak-corfu-test--recording
             (emacspeak-corfu-test--open "os.pa" '("os.path" "os.pathsep" "os.pardir"))
             (setq corfu--index 0)
             (call-interactively #'corfu-complete))))
      (should (equal (buffer-string) "os.path"))
      (should (equal (last events 2) '((icon complete) (speak "os.path")))))))

(ert-deftest emacspeak-corfu-complete-with-nothing-to-add-plays-only-the-icon ()
  (with-temp-buffer
    (let ((events
           (emacspeak-corfu-test--recording
             (emacspeak-corfu-test--open "os.pa" '("os.path" "os.pardir"))
             (setq corfu--index -1)
             (call-interactively #'corfu-complete))))
      (should (equal (buffer-string) "os.pa"))
      (should (equal (car (last events)) '(icon complete))))))

;;; emacspeak-corfu-tests.el ends here
