;;;; acdp-server.lisp

(in-package #:acdp-server)

(defvar *app* (make-instance 'ningle:<app>))
(defvar *server* nil)
(defvar *cv* (bt:make-condition-variable))
(defvar *cd-info* nil)
(defvar *sprintf.js* "/*! sprintf.js | Copyright (c) 2007-2013 Alexandru Marasteanu <hello at alexei dot ro> | 3 clause BSD license */(function(e){function r(e){return Object.prototype.toString.call(e).slice(8,-1).toLowerCase()}function i(e,t){for(var n=[];t>0;n[--t]=e);return n.join(\"\")}var t=function(){return t.cache.hasOwnProperty(arguments[0])||(t.cache[arguments[0]]=t.parse(arguments[0])),t.format.call(null,t.cache[arguments[0]],arguments)};t.format=function(e,n){var s=1,o=e.length,u=\"\",a,f=[],l,c,h,p,d,v;for(l=0;l<o;l++){u=r(e[l]);if(u===\"string\")f.push(e[l]);else if(u===\"array\"){h=e[l];if(h[2]){a=n[s];for(c=0;c<h[2].length;c++){if(!a.hasOwnProperty(h[2][c]))throw t('[sprintf] property \"%s\" does not exist',h[2][c]);a=a[h[2][c]]}}else h[1]?a=n[h[1]]:a=n[s++];if(/[^s]/.test(h[8])&&r(a)!=\"number\")throw t(\"[sprintf] expecting number but found %s\",r(a));switch(h[8]){case\"b\":a=a.toString(2);break;case\"c\":a=String.fromCharCode(a);break;case\"d\":a=parseInt(a,10);break;case\"e\":a=h[7]?a.toExponential(h[7]):a.toExponential();break;case\"f\":a=h[7]?parseFloat(a).toFixed(h[7]):parseFloat(a);break;case\"o\":a=a.toString(8);break;case\"s\":a=(a=String(a))&&h[7]?a.substring(0,h[7]):a;break;case\"u\":a>>>=0;break;case\"x\":a=a.toString(16);break;case\"X\":a=a.toString(16).toUpperCase()}a=/[def]/.test(h[8])&&h[3]&&a>=0?\"+\"+a:a,d=h[4]?h[4]==\"0\"?\"0\":h[4].charAt(1):\" \",v=h[6]-String(a).length,p=h[6]?i(d,v):\"\",f.push(h[5]?a+p:p+a)}}return f.join(\"\")},t.cache={},t.parse=function(e){var t=e,n=[],r=[],i=0;while(t){if((n=/^[^\\x25]+/.exec(t))!==null)r.push(n[0]);else if((n=/^\\x25{2}/.exec(t))!==null)r.push(\"%\");else{if((n=/^\\x25(?:([1-9]\\d*)\\$|\\(([^\\)]+)\\))?(\\+)?(0|'[^$])?(-)?(\\d+)?(?:\\.(\\d+))?([b-fosuxX])/.exec(t))===null)throw\"[sprintf] huh?\";if(n[2]){i|=1;var s=[],o=n[2],u=[];if((u=/^([a-z_][a-z_\\d]*)/i.exec(o))===null)throw\"[sprintf] huh?\";s.push(u[1]);while((o=o.substring(u[0].length))!==\"\")if((u=/^\\.([a-z_][a-z_\\d]*)/i.exec(o))!==null)s.push(u[1]);else{if((u=/^\\[(\\d+)\\]/.exec(o))===null)throw\"[sprintf] huh?\";s.push(u[1])}n[2]=s}else i|=2;if(i===3)throw\"[sprintf] mixing positional and named placeholders is not (yet) supported\";r.push(n)}t=t.substring(n[0].length)}return r};var n=function(e,n,r){return r=n.slice(0),r.splice(0,0,e),t.apply(null,r)};e.sprintf=t,e.vsprintf=n})(typeof exports!=\"undefined\"?exports:window);")

(defparameter *css* "
body {
  width: 100%;
  font-family: serif;
  font-size: 16px;
  line-height: 24px;
  margin:0;
  color: rgb(29, 35, 38);
}

a {
  color: rgb(29, 35, 38);
}

.content {
  margin: 0 auto;
  max-width: 35em;
  padding-top: 120px;
}

header.control a {
  padding: 24px 1em;
  color: #fff
}

div.play {
  text-align: right;
  margin-right: 0;
  margin-left: auto;
  float: right;
}

section {
  padding-top: 24px;
  padding-bottom: 23px;
  border-bottom: 1px solid rgb(29, 35, 38);
}

section.tracks {
  border: none;
}

header.control {
  text-align: center;
  padding: 24px 0;
  position: fixed;
  width: 100%;
  background-color: rgb(29, 35, 38);
  color: #fff;
  height: 72px;
}

header.control canvas {
  margin-top: 24px;
  height: 24px;
  width: 90%;
  max-width: 35em;
}

section.status {
  text-align: center;
}

p.status {
  text-transform: lowercase;
}

.track-artist {
  font-style: italic;
  font-size: 14px;
}

.total-time {
  text-align: right;
}

.label {
  font-style: italic;
  width: 4em;
  float: left;
}

.total-label {
  font-style: italic;
  margin-right: 1em;
}

h1, h2 {
  text-align: center;
}

li {
  margin: 24px 0;
}

ol {
  padding: 0;
}

@media (max-width: 970px), (max-device-width: 970px) {
  body {
    font-size: 32px;
    line-height: 48px;
  }

  .content {
    max-width: 100%;
    width: 90%;
    padding-top: 240px;
  }

  section {
    padding-top: 48px;
    padding-bottom: 47px;
  }

  section.disc-info {
    padding-top: 0;
  }

  header.control {
    padding: 48px 0;
    height: 144px;
  }

  header.control canvas {
    margin-top: 48px;
    height: 48px;
  }

  .track-artist {
    font-size: 28px;
  }

  .label {
    float: none;
    margin-top: 48px;
  }

  li {
    margin: 48px 0;
  }

  ol {
    padding-left: 40px;
  }
}
")

(defparameter *js*
  (ps
    (defvar *player-status* "STOPPED")
    (defvar *track-length* 0)
    (defvar *slider* nil)
    (defvar *mq* "(max-width: 970px), (max-device-width: 970px)")
    
    (defun clamp (x a b)
      (if (< x a) a (if (> x b) b x)))
    
    (defun -slider (canvas set-cb move-cb)
      (setf (@ this canvas) canvas
	    (@ this set-cb) set-cb
	    (@ this move-cb) move-cb
	    (@ this value) 0
	    (@ this mouse-down) false
	    (@ this enabled) t
	    (@ this u) 12)
      (setf (@ canvas height) (@ canvas client-height)
	    (@ canvas width) (@ canvas client-width))
      (let ((slider this))
	(setf (@ canvas onmousedown)
	      (lambda (evt)
		(when (and (== (@ evt button) 0) (@ slider enabled))
		  (setf (@ slider mouse-down) t)
		  ((@ slider move) (- (@ evt page-x)
				      (chain slider canvas offset-left))))))
	(setf (@ canvas onmousemove)
	      (lambda (evt)
		(when (and (@ slider mouse-down) (@ slider enabled))
		  ((@ slider move) (- (@ evt page-x)
				      (chain slider canvas offset-left))))))
	(setf (@ canvas onmouseup)
	      (lambda (evt)
		(when (and (@ slider mouse-down) (@ slider enabled))
		  (setf (@ slider mouse-down) false)
		  (when (@ slider set-cb)
		    ((@ slider set-cb) (@ slider value)))))))
      ((@ this draw)))
    
    (setf (@ -slider prototype move)
	  (lambda (x)
	    (let ((u (@ this u)))
	      (setf (@ this value)
		    (if (> (chain this canvas width) 0)
			(clamp (/ (- x u) (- (chain this canvas width) (+ u u)))
			       0 1)
			0)))
	    (when (@ this move-cb)
	      ((@ this move-cb)))
	    ((@ this draw))))
    
    (setf (@ -slider prototype draw)
	  (lambda ()
	    (let ((u (@ this u))
		  (dc (chain this canvas (get-context "2d")))
		  (w (chain this canvas width))
		  (h (chain this canvas height))
		  (v (@ this value)))
	      ;(setf (@ dc fill-style) "rgb(29, 35, 38)")
	      ((@ dc clear-rect) 0 0 w h)
	      (setf (@ dc stroke-style) "#fff")
	      ((@ dc begin-path))
	      ((@ dc move-to) u u)
	      ((@ dc line-to) (- w u) u)
	      ((@ dc close-path))
	      ((@ dc stroke))
	      (when (@ this enabled)
		(setf (@ dc fill-style) "#fff")
		((@ dc begin-path))
		((@ dc arc) (+ u (* v (- w u u)))
		 u (- u 2) 0 (* 2 (@ -math -p-i)))
		((@ dc close-path))
		((@ dc fill))))))
    
    (setf (@ -slider prototype set-value)
	  (lambda (val)
	    (unless (@ this mouse-down)
	      (setf (@ this value) val)
	      ((@ this draw)))))
    
    (setf (@ -slider prototype enable)
	  (lambda (enabled)
	    (setf (@ this enabled) enabled)
	    ((@ this draw))))
    
    (defun send-command (url)
      (let ((xhr (new (-x-m-l-http-request))))
	((@ xhr open) "GET" url t)
	((@ xhr send))))
    
    (defun call-api (url callback)
      (let ((xhr (new (-x-m-l-http-request))))
	((@ xhr open) "GET" url t)
	(setf (@ xhr onreadystatechange)
	      (lambda ()
		(when (and (equal 4 (@ xhr ready-state))
			   (equal 200 (@ xhr status)))
		  (funcall callback xhr))))
	((@ xhr send))))
    
    (defun play (track)
      (send-command (+ "start/" track)))
    
    (defun stop ()
      (send-command "stop"))
    
    (defun pause ()
      (send-command "pause"))
    
    (defun resume ()
      (send-command "resume"))
    
    (defun seek (offset)
      (send-command (+ "seek/" offset)))
    
    (defun jump (sector)
      (send-command (+ "jump/" sector)))
    
    (defun next ()
      (send-command "next"))
    
    (defun prev ()
      (send-command "prev"))
    
    (defun get-status ()
      (call-api "status" #'status-cb))
    
    (defun t2s (time)
      (let ((m (floor time 60))
	    (s (% time 60)))
	(sprintf "%'02d:%'02d" m s)))
    
    (defun status-cb (xhr)
      (try
       (let* ((status ((@ -j-s-o-n parse) (@ xhr response-text)))
	      (time (floor (@ status sector) 75))
	      (track-total (floor (@ status length) 75)))
	 (setf (chain document (get-element-by-id "status") inner-h-t-m-l)
	       (who-ps-html
		(:p :class "status" (@ status status))
		(:p "track " (@ status track) " – " (t2s time)
		    " of " (t2s track-total))))
	 (when (== (@ status status) "playing")
	   ((@ *slider* set-value) (/ (@ status sector) (@ status length))))
	 (setf *track-length* (@ status length))
	 (when (!= *player-status* (@ status status))
	   (setf *player-status* (@ status status))
	   ((@ *slider* enable) t)
	   (setf (chain document (get-element-by-id "controlbar") inner-h-t-m-l)
		 (case (@ status status)
		   ("playing"
		    (who-ps-html
		     (:a :href "javascript:prev()" "prev")
		     (:a :href "javascript:pause()" "pause")
		     (:a :href "javascript:stop()" "stop")
		     (:a :href "javascript:next()" "next")))
		   ("paused"
		    (who-ps-html
		     (:a :href "javascript:prev()" "prev")
		     (:a :href "javascript:resume()" "resume")
		     (:a :href "javascript:stop()" "stop")
		     (:a :href "javascript:next()" "next")))
		   (t ((@ *slider* enable) false)
		    (who-ps-html (:a :href "javascript:play(1)" "play")))))))
       (:catch (e)
	 (when *player-status*
	   ((@ *slider* enable) false)
	   (setf (chain document (get-element-by-id "controlbar") inner-h-t-m-l)
		 (case *player-status*
		   ("playing"
		    (who-ps-html (:a "prev") (:a "pause") (:a "stop") (:a "next")))
		   ("paused"
		    (who-ps-html (:a "prev") (:a "resume") (:a "stop") (:a "next")))
		   (t (who-ps-html (:a "play"))))))
	 (setf *player-status* nil)))
      (set-timeout #'get-status 200))
    
    (defun mq-cb (mq)
      (let ((c (@ *slider* canvas)))
	(setf (@ c height) (@ c client-height)
	      (@ c width) (@ c client-width)))
      (setf (@ *slider* u) (if (@ mq matches) 24 12))
      ((@ *slider* draw)))
    
    (defun init ()
      (set-timeout #'get-status 200)
      (setf *slider* (new (-slider ((@ document get-element-by-id) "slidercv")
				   (lambda (val)
				     (jump (round (* val *track-length*))))
				   nil)))
      (let ((mq ((@ window match-media) *mq*)))
	((@ mq add-listener) #'mq-cb)
	(setf (@ window onresize)
	      (lambda (evt) (mq-cb ((@ window match-media) *mq*))))
	(mq-cb mq)))))

(defun start (&optional (port 5000))
  (setf *server* (clack:clackup *app* :port port)))

(defun stop ()
  (clack:stop *server*))

(defun run ()
  (start)
  (let ((lock (bt:make-lock)))
    (bt:with-lock-held (lock)
      (bt:condition-wait *cv* lock)))
  (stop))

(defun quit ()
  (bt:condition-notify *cv*))

(defun build ()
  (start)
  (stop)
  (trivial-dump-core:save-executable "acdp_server" #'run))

;;; functions

(defun geta (alist key)
  (cdr (assoc key alist)))

(defun t2s (time)
  (format nil "~2,'0d:~2,'0d"
	  (truncate time 60)
	  (rem time 60)))

(defun call-and-redirect (fn &optional (url "/"))
  (lambda (params)
    (funcall fn) nil))

(defun read-cd ()
  (setf *cd-info* (acdp:cd-info)))

(defun start-page (params)
  (read-cd)
  (multiple-value-bind (status track time-s track-total-s) (acdp:status)
    (let ((total (geta *cd-info* :seconds))
	  (time (truncate time-s 75))
	  (track-total (truncate track-total-s 75)))
      (if *cd-info*
	  (spinneret:with-html-string
	    (:doctype)
	    (:html
	     (:head
	      (:title ("ACDP - ~a" (geta *cd-info* :title)))
	      (:link :rel "stylesheet" :type "text/css" :href "main.css")
	      (:script :type "application/javascript" :src "main.js")
	      (:script :type "application/javascript" :src "sprintf.js"))
	     (:body :onload "javascript:init()"
		    (:header.control
		     (:div#controlbar
		      (:a :href "javascript:play(1)" "play"))
		     (:canvas#slidercv))
		    (:div.content
		     (:section.status#status
		      (:p.status status)
		      (:p "track " track "–" (t2s time) " of " (t2s track-total)))
		     (:section.disc-info
		      (:div.disc-title
		       (:div.label "Title:") (geta *cd-info* :title))
		      (:div.disc-artist
		       (:div.label "Artist:") (geta *cd-info* :artist))
		      (:div.disc-year (:div.label "Year:") (geta *cd-info* :year)))
		     (:section.tracks
		      (:ol (dolist (track (geta *cd-info* :tracks))
			     (:li
			      (:div.track-title (geta track :title))
			      (:div.track-artist (geta track :artist))
			      (:div.track-length
			       (t2s (geta track :length))
			       (:div.play
				(:a :href (format nil "javascript:play(~a)"
						  (geta track :number))
				    "play"))))))
		      (:div.total-time
		       (:span.total-label "Total time:")
		       (t2s total)))))))
	  (spinneret:with-html-string
	    (:doctype)
	    (:html
	     (:head
	      (:title "ACDP - no CD")
	      (:link :rel "stylesheet" :type "text/css" :href "main.css"))
	     (:body
	      (:header.control
	       (:h1 "Sorry, no CD found."))
	      (:div.content
	       (:p :style "text-align: center;"
		   "Please insert an Audio CD.")))))))))

;;; web api

(setf (ningle:route *app* "/start/:track")
      (lambda (params)
	;(format t "starting track ~a~%" (getf params :track))
	(acdp:start (parse-integer (getf params :track)))
	nil))

(setf (ningle:route *app* "/pause")
      (call-and-redirect #'acdp:pause))

(setf (ningle:route *app* "/resume")
      (call-and-redirect #'acdp:resume))

(setf (ningle:route *app* "/stop")
      (call-and-redirect #'acdp:stop))

(setf (ningle:route *app* "/next")
      (call-and-redirect #'acdp:next))

(setf (ningle:route *app* "/prev")
      (call-and-redirect #'acdp:previous))

(setf (ningle:route *app* "/seek/:sectors")
      (lambda (params)
	(acdp:seek (getf params :sectors))))

(setf (ningle:route *app* "/jump/:sector")
      (lambda (params)
	(acdp:jump (getf params :sector))))

(setf (ningle:route *app* "/status")
      (lambda (params)
	(setf (clack.response:headers ningle:*response* :content-type)
	      "application/json")
	(acdp:status-json)))

(setf (ningle:route *app* "/quit")
      (call-and-redirect #'quit))

;; web app
(setf (ningle:route *app* "/")
      #'start-page)

(setf (ningle:route *app* "/main.css")
      (lambda (params)
	(setf (clack.response:headers ningle:*response* :content-type)
	      "text/css")
	*css*))

(setf (ningle:route *app* "/main.js")
      (lambda (params)
	(setf (clack.response:headers ningle:*response* :content-type)
	      "application/javascript")
	*js*))

(setf (ningle:route *app* "/sprintf.js")
      (lambda (params)
	(setf (clack.response:headers ningle:*response* :content-type)
	      "application/javascript")
	*sprintf.js*))
