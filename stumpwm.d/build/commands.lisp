(in-package :stumpwm)
(defvar *passwords* '())
#+devtime
(progn
  (defcommand start-swank () ()
    "Start a swank server on port 4587"
    (swank:create-server :port 4587 :dont-close t))


  (defcommand pause-mpd () ()
    "Pause MPD"
    (mpd-remote:with-mpd-connection (so st)
      (declare (ignorable so))
      (mpd-remote:send-command st :pause)))

  (defcommand next-mpd () ()
    "Next MPD"
    (mpd-remote:with-mpd-connection (so st)
      (declare (ignorable so))
      (mpd-remote:send-command st :next)
      (current-song)))

  (defcommand prev-mpd () ()
    "Prev MPD"
    (mpd-remote:with-mpd-connection (so st)
      (declare (ignorable so))
      (mpd-remote:send-command st :previous)
      (current-song)))

  (defcommand current-song () ()
    "Display information about the current song"
    (mpd-remote:with-mpd-connection (so st)
      (declare (ignorable so))
      (let ((current-song (mpd-remote:send-command st :current-song)))
        (message "~a ~a: ~a - ~a"
                 (mpd-remote.song::track current-song)
                 (mpd-remote.song::artist current-song)
                 (mpd-remote.song::album current-song)
                 (mpd-remote.song::title current-song)))))

  (defparameter *browser-command*
    (namestring
     (merge-pathnames (make-pathname :directory '(:relative "bin")
                                     :name "firefox")
                      (user-homedir-pathname))))

  (defun cat (&rest strings)
    (uiop:escape-sh-command strings))

  (defgeneric get-search-url (provider &rest strings)
    (:method-combination append :most-specific-last)
    (:method :around (provider &rest r)
      (declare (ignore r))
      (apply #'concatenate 'string (call-next-method)))

    (:method append (provider &rest r)
      (declare (ignore r))
      (list "https://duckduckgo.com/?q="))
    (:method append ((provider (eql nil)) &rest strings)
      (list* (car strings) (loop for string in (cdr strings) nconcing (list "+" string))))

    (:method append ((provider (eql :google)) &rest strings)
      (list* "%21google" (loop for string in strings nconcing (list "+" string)))))

  (defmacro add-provider (name ddg-shortcut)
    `(defmethod get-search-url append ((provider (eql ,name)) &rest strings)
       (list* (concatenate 'string "%21" ,ddg-shortcut)
              (loop for string in strings nconcing (list "+" string)))))

  (defmacro add-providers (&body definitions)
    `(progn
       ,@(loop for (name shortcut) in definitions
               collect `(add-provider ,name ,shortcut))))


  (add-providers
    (:amazon "a")
    (:php "php")
    (:python "python")
    (:stack-overflow "sof")
    (:lisp "lisp")
    (:wikipedia "w"))

  (defcommand slack-copy () ()
    (ubiquitous:restore :atomampd-slack)
    (push (get-x-selection) (ubiquitous:value :clip))
    (ubiquitous:offload))

  (defcommand do-search (provider search-string) ((:string "Provider: ") (:string "Search for: "))
    "Run a search against a specified provider"
    (check-type provider (or null string))
    (check-type search-string (or null string))
    (when (and provider search-string)
      (let ((provider (intern (string-upcase provider) :keyword)))
        (run-shell-command (cat *browser-command* (get-search-url provider (substitute #\+ #\space search-string)))))))

  (defcommand google (search-string) ((:string "Search Google for: "))
    "Search google for a given string"
    (check-type search-string (or null string))
    (when search-string
      (run-shell-command (cat *browser-command* (get-search-url :google (substitute #\+ #\space search-string))))))

  (defcommand duckduckgo (search-string) ((:string "Search DDG for: "))
    "Search duckduckgo gor a given string"
    (check-type search-string (or null string))
    (when search-string
      (run-shell-command (cat *browser-command* (get-search-url nil (substitute #\+ #\space search-string))))))

  (defcommand search-for-selection (provider) ((:string "Search Provider?"))
    "Search for the x selection with provider"
    (do-search provider (get-x-selection)))

  (defcommand jira-ticket (number) ((:string "Ticket Number? "))
    "get moi a jira ticket"
    (unless (alpha-char-p (elt number 0))
      (setf number (format nil "ATOMOS-~a" number)))
    (run-shell-command #1=(format nil "'~a'"
                                  (cat *browser-command*
                                       (format nil "https://atomampd.atlassian.net/browse/~a" number)))))

  (defcommand run-yeganesh () ()
    "Run Command given by yeganesh"
    (let ((cmd (run-shell-command "/home/edwlan/bin/yeganesh_run" t)))
      (run-shell-command cmd)))

  (defcommand put-password (identifier) ((:string "Password Identifier? "))
    "Put a password into a text-input"
    (window-send-string (cdr (assoc (alexandria:make-keyword (string-upcase identifier))
                                    *passwords*))))
  (defcommand store-password (identifier password) ((:string "Password Identifier? ") (:string "Password? "))
    "Store a password..."
    (setf *passwords* (acons (alexandria:make-keyword (string-upcase identifier))
                             password
                             *passwords*))
    "stored")

  (defcommand reap-zombies () ()
    (message "~s"
             (loop with reaped-pids = '()
                   for pid = (sb-posix:waitpid -1 sb-posix:WNOHANG)
                   until (= pid 0)
                   do (push pid reaped-pids)
                   finally (return reaped-pids)))))

  ;(defi)
