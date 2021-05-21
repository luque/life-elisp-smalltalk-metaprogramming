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

(defun url-pharo-post (body)
  (message (format "body: %s" body))
  (url-http-post "http://localhost:1701/repl" body))

(defun eval-pharo (expression)
  (url-pharo-post expression))


(defconst class-template "
        %s subclass: #%s
        instanceVariableNames: '%s'
        classVariableNames: '%s'
        package: '%s'")

(defun new-class (superclass name package instanceVariablesList classVariablesList)
  (let ((instanceVariables (mapconcat 'identity instanceVariablesList " "))
        (classVariables (mapconcat 'identity classVariablesList " ")))
    (eval-pharo (format class-template superclass name instanceVariables classVariables package))
    )
)

(defun new-morph (name package instanceVariablesList classVariablesList)
  (new-class "Morph" name package instanceVariablesList classVariablesList))

(defconst compile-method-template "
        %s
        compile: ('%s', String cr, '%s')
        classified: '%s'")

(defconst compile-class-method-template "
        %s class
        compile: ('%s', String cr, '%s')
        classified: '%s'")

(defun add-method (class name body classifier)
  (eval-pharo (format compile-method-template class name body classifier)))

(defun add-class-method (class name body classifier)
  (eval-pharo (format compile-class-method-template class name body classifier)))

(provide 'pharo)
;;; pharo.el ends here
