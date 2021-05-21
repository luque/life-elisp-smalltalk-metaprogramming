;;; pharo.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 John Doe
;;
;; Author: John Doe <https://github.com/rzamo>
;; Maintainer: John Doe <john@doe.com>
;; Created: May 11, 2021
;; Modified: May 11, 2021
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/rzamo/pharo
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
(require 'url)

(defconst default-repl-port 1701)
(defconst class-template "
        %s subclass: #%s
        instanceVariableNames: '%s'
        classVariableNames: '%s'
        package: '%s'")
(defconst compile-method-template "
        %s
        compile: ('%s', String cr, '%s')
        classified: '%s'")
(defconst compile-class-method-template "
        %s class
        compile: ('%s', String cr, '%s')
        classified: '%s'")

(defvar pharo-repl-port)

(defun setup-pharo-environment (&optional version arch)
  (let* ((version (or version "stable"))
         (arch (or arch "64"))
         (pharoZeroConfUrl (format "https://get.pharo.org/%s/%s+vm" arch version)))
    (message (format "Installing Pharo VM and Image from [%s]..." pharoZeroConfUrl))
    (shell-command (format "curl %s | bash" pharoZeroConfUrl))
    (message "Pharo VM/Image installed OK!")
    )
  )

(defun load-repl-server (&optional port)
  ;; ./pharo Pharo.image eval --save
  ;; "(ZnServer on: 1701) bindingAddress: NetNameResolver localHostAddress; delegate: ZnReadEvalPrintDelegate new; start"
  (setq pharo-repl-port (or port default-repl-port))
  (shell-command (format "./pharo Pharo.image eval --save '(ZnServer on: %d) bindingAddress: NetNameResolver localHostAddress; delegate: ZnReadEvalPrintDelegate new; register; start'" pharo-repl-port))
  )

(defun launch-pharo-repl ()
  (start-process "pharo-ui" "pharo-ui process" "./pharo-ui" "Pharo.image")
)

(defun url-pharo-post (body)
  (message (format "body: %s" body))
  (url-http-post (format "http://localhost:%d/repl" pharo-repl-port) body)
)

(defun eval-pharo (expression)
  (url-pharo-post expression)
)

(defun new-class (superclass name package instanceVariablesList classVariablesList)
  (let ((instanceVariables (mapconcat 'identity instanceVariablesList " "))
        (classVariables (mapconcat 'identity classVariablesList " ")))
    (eval-pharo (format class-template superclass name instanceVariables classVariables package))
    )
)

(defun new-morph (name package instanceVariablesList classVariablesList)
  (new-class "Morph" name package instanceVariablesList classVariablesList))

(defun add-method (class name body classifier)
  (eval-pharo (format compile-method-template class name body classifier)))

(defun add-class-method (class name body classifier)
  (eval-pharo (format compile-class-method-template class name body classifier)))

(provide 'pharo)
;;; pharo.el ends here
