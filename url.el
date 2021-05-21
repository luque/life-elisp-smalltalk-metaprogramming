;;; url.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 John Doe
;;
;; Author: John Doe <https://github.com/rzamo>
;; Maintainer: John Doe <john@doe.com>
;; Created: May 11, 2021
;; Modified: May 11, 2021
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/rzamo/url
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defun url-http-post (url body)
  "Send BODY to URL as a POST request."
  (let (
        (response-string nil)
        (url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "text/plain")))
        (url-request-data (encode-coding-string body 'utf-8)))
    (switch-to-buffer
     (url-retrieve-synchronously url))
    (goto-char (point-min))
    (re-search-forward "\n\n")
    (setq response-string
          (buffer-substring-no-properties (point) (point-max)))
    (kill-buffer (current-buffer))
    response-string))


(provide 'url)
;;; url.el ends here
