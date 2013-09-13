;;;; cl-acdp.lisp

(in-package #:acdp)

(defparameter *binary* "acdp" ;assumes acdp to be in $PATH
  "The default path/command to call the acdp binary.")
(defvar *current-player* nil
  "Contains the last player that has been started.")
(defvar *next-track-hook* nil
  "A function taking the track as an argument,
called, when the track changes.")
(defvar *current-track* 0)
(defparameter *player-lock* (bt:make-lock))
;(defvar *stop-condition* (bt:make-condition-variable))

;;; helpers

(defun send-command (player cmd)
  "Sends a command to the player."
  (when (and player (eq :running (external-program:process-status player)))
    (let ((stream (external-program:process-input-stream player)))
      (write-line cmd stream)
      (finish-output stream))))

(defun json-val (object key)
  (cdr (assoc key object)))

(defun make-status-hook (track)
  (lambda (process)
    (when (and (equal '(:exited 0) (multiple-value-list
				    (external-program:process-status process)))
	       (< track (json-val (cd-info) :trackcount)))
      (start (1+ track))
      (when *next-track-hook*
	(funcall *next-track-hook* (1+ track))))))

;;; api

(defun start (track &key (bin *binary*))
  "=> player object (object of the player's process)
Starts the player to play track `track`.
If bin is given, it denotes the command to start the process,
e.g. `acdp -i /dev/cdrom1` or `/opt/bin/acdp`.
The default value is the value of `*binary*`."
  (bt:with-lock-held (*player-lock*)
    (when *current-player*
      (stop))
    (setf *current-track* track)
    (format t "Starting track ~a~%" track)
    (setf *current-player*
	  (external-program:start bin (list (format nil "~a"  track))
				  :status-hook (make-status-hook track)
				  :input :stream :output :stream
				  #+ccl :sharing #+ccl :lock))
    (format t "Started track ~a, pid ~a~%" track
	    (external-program:process-id *current-player*))))

(defun pause (&optional (player *current-player*))
  "Pauses `player`. If `player` is omitted, `*current-player*` is used."
  (send-command player "pause"))

(defun resume (&optional (player *current-player*))
  "Resumes `player`. If `player` is omitted, `*current-player*` is used."
  (send-command player "resume"))

(defun stop (&optional (player *current-player*))
  "Stops `player`. If `player` is omitted, `*current-player*` is used."
  (format t "Stopping process ~a~%" (external-program:process-id player))
  (send-command player "stop")
  (when (eq :running (external-program:process-status player))
    (external-program:signal-process player 15))
  (format t "Stopped process ~a~%" (external-program:process-id player)))

(defun seek (sectors &optional (player *current-player*))
  "Seeks `player` by `sectors` sectors.
If `player` is omitted, `*current-player*` is used."
  (send-command player (format nil "seek ~a" sectors)))

(defun jump (sector &optional (player *current-player*))
  "Jumps to `sector`.
If `player` is omitted, `*current-player*` is used."
  (send-command player (format nil "jump ~a" sector)))

(defun next (&key (player *current-player*) (track *current-track*))
  (when (< track (or (json-val (cd-info) :trackcount) 0))
    (start (1+ track))
    (when *next-track-hook*
      (funcall *next-track-hook* (1+ track)))))

(defun previous (&key (player *current-player*) (track *current-track*))
  (if (or (> (or (nth-value 2 (status)) 0) 300) (<= track 1))
      (jump 0 player)
      (progn
	(start (1- track))
	(when *next-track-hook*
	  (funcall *next-track-hook*)))))

(defun status-json (&optional (player *current-player*))
  (if (and *current-player*
	   (eq (external-program:process-status player) :running))
      (let ((in (external-program:process-input-stream player))
	    (out (external-program:process-output-stream player)))
	(clear-input out)
	(write-line "status" in)
	(finish-output in)
	(read-line out))
      "{\"status\": \"exited\", \"track\": 0, \"sector\": 0, \"length\": 0}"))

(defun status (&optional (player *current-player*))
  "=> (values status track sector length)
Returns status information from `player`.
`status` is either `:playing`, `:paused`, or `:exited`.
`track` is the current track (1 based index).
`sector` is the current sector.
`length` is the track's total number of sectors.
If  `status` is `:exited`, all other values are `0`.
"
  (handler-case
      (let* ((jo (cl-json:decode-json-from-string (status-json player)))
	     (status (intern (string-upcase (json-val jo :status)) :keyword))
	     (track (json-val jo :track))
	     (sec (json-val jo :sector))
	     (len (json-val jo :length)))
	(values status track sec len))
    (t (e) (princ e) nil)))

(defun cd-info-json (&optional (bin *binary*))
  (let ((out (make-string-output-stream)))
    (handler-bind (#+sbcl(sb-int::character-decoding-error
			  (lambda (c) (use-value ""))))
      (external-program:run bin '("-q") :output out)
      (get-output-stream-string out))))

(defun cd-info (&optional (bin *binary*))
  (handler-case
      (cl-json:decode-json-from-string (cd-info-json bin))
    (t (e) (princ e) nil)))

;;; utility functions

(defun sectors-to-seconds (sectors &optional time-p)
  "=> seconds or (values seconds minutes)
converts a number of sectors to seconds.
If time-p is given and true-ish, the time is returned in minutes plus seconds,
seconds as the first and minutes as the secondary value."
  (let ((s (truncate sectors 75)))
    (if time-p
	(values (rem s 60) (truncate s 60))
	s)))

(defun time-s (sectors)
  "=> seconds
Converts a number of sectors to a time in minutes plus seconds (e.g. 31:19).
Returns the \"second\" part (19)."
  (rem (truncate sectors 75) 60))

(defun time-m (sectors)
  "=> minutes
Converts a number of sectors to a time in minutes plus seconds (e.g. 31:19).
Returns the minute part (31)."
  (truncate sectors 4500)) ;4500 = 60 * 75
