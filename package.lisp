;;;; package.lisp

(defpackage #:acdp
  (:use #:cl)
  (:export #:start #:stop #:pause #:resume
	   #:seek #:jump #:previous #:next
	   #:status #:status-json
	   #:cd-info-json #:cd-info
	   #:sectors-to-seconds #:time-s #:time-m
	   #:*binary* #:*current-player*))
