;;; lifted.el --- Functional reactive programming library for Emacs Lisp

;; Copyright (C) 2015 Ben Yelsey

;; Author: Ben Yelsey <ben.yelsey@gmail.com>
;; Version: 0.0.1
;; Keywords: reactive

;;; Commentary:

;; This is a rough draft of an FRP library for Emacs. I'm working from a number of inspirations, primarily ReactiveCocoa and Elm.
;; Obviously there's a lot left to do, including but not limited to:
;;  - real scheduling (possibly using deferred.el)
;;  - nice UI helpers (keyboard binding, command binding, etc)
;;  - lots and lots of functional operators
;;  - signal merging
;;  - multicasting
;;  - signal disposal
;;  - a real concept of "sequences"
;;  - error handling

;;; Samples (will almost certainly change):

;; ** Making a key signal

;; (defvar key-signal (lifted:signal-for-key (kbd "C-c s")))

;; ** Making a signal

;; (defvar post-command-signal
;;   (lifted:signal
;;    (lambda (subscriber)
;;      (lexical-let ((subscriber subscriber))
;;        (add-hook 'post-command-hook
;;                  (lambda ()
;;                    (funcall subscriber :send-next this-command)))))))

;; ** Subscribing

;; (funcall post-command-signal
;;          :subscribe-next (lambda (command) (message "got command: %s" command)))

;; ** Chaining subscriptions

;; (funcall (funcall post-commandssignal
;;                   :subscribe-next (lambda (command) (message "got command: %s" command)))
;;          :subscribe-next (lambda (command) (message "also got command: %s" command)))

;; ** Functional operators

;; (defvar timestamps-for-key (funcall key-signal :map (lambda (val) (float-time))))
;;  --- equivalent to ---
;; (defvar timestamps-for-key (lifted:map (lambda (val) (float-time)) key-signal))

;; ** Chaining functional operators

;; (funcall key-signal
;;          :map (lambda (val) (float-time))
;;          :subscribe-next (lambda (val) (message "subscriber 1 reporting timestamp: %s" val))
;;          :subscribe-next (lambda (val) (message "subscriber 2 reporting timestamp: %s" val))
;;          :map (lambda (val) (round val))
;;          :filter (lambda (val) (evenp val))
;;          :subscribe-next (lambda (val) (message "subscriber 3 reporting even timestamp: %s" val)))

;;; Code:

(defun lifted:signal-for-key (key &optional key-map)
  "Returns a signal that emits `t' each time is `key' is pressed.
Binds to `key-map' if supplied, defaults to the global map."
  (lexical-let ((subscribers '()))
    (define-key (or key-map (current-global-map)) key
      (lambda ()
        (interactive)
        (dolist (subscriber subscribers)
          (funcall subscriber :send-next t))))
    (lifted:signal (lambda (subscriber)
                     (add-to-list 'subscribers subscriber)))))

(defun lifted:map (callback base-signal)
  "Returns a signal producing the output of the `base-signal', with `callback' applied."
  (lexical-let* ((callback callback)
                 (base-signal base-signal))
    (lifted:signal
     (lambda (subscriber)
       (lexical-let ((subscriber subscriber))
         (funcall base-signal :subscribe-next
                  (lambda (value)
                    (funcall subscriber :send-next
                             (funcall callback value)))))))))

(defun lifted:filter (callback base-signal)
  "Returns a signal producing the output of the `base-signal', if `callback' returns true when applied."
  (lexical-let* ((callback callback)
                 (base-signal base-signal))
    (lifted:signal
     (lambda (subscriber)
       (lexical-let ((subscriber subscriber))
         (funcall base-signal :subscribe-next
                  (lambda (value)
                    (when (funcall callback value)
                      (funcall subscriber :send-next
                               value)))))))))

(defun lifted:signal (body &optional subscribers)
  "Creates a 'signal' closure"
  (lexical-let ((body body)
                (subscribers subscribers))
    (lambda (&rest commands)
      (if (not commands)
          (lifted:signal body subscribers)
        (let ((command (pop commands))
              (callback (pop commands)))
          (unless callback
            (error "Missing callback for %s" command))
          (let ((new-signal (pcase command
                              (:map
                               (lifted:map callback (lifted:signal body subscribers)))
                              (:filter
                               (lifted:filter callback (lifted:signal body subscribers)))
                              (:subscribe-next
                               (let ((subscriber (lifted:subscriber :next callback)))
                                 (funcall body subscriber)
                                 (lifted:signal body (cons subscriber subscribers))))
                              (:subscribe-completed
                               (let ((subscriber (lifted:subscriber :completed callback)))
                                 (funcall body subscriber)
                                 (lifted:signal body (cons subscriber subscribers)))))))
            (apply new-signal commands)))))))

(defun lifted:subscriber (&rest commands)
  "Creates a 'subscriber' closure"
  (lexical-let ((callbacks (make-hash-table)))
    (while commands
      (let ((command (pop commands))
            (callback (pop commands)))
        (if callback
            (puthash command callback callbacks)
          (error "Missing callback for %s" command))))
    (lambda (&rest commands)
      (while commands
        (let ((command (pop commands))
              (value (pop commands)))
          (pcase command
            (:send-next (funcall (gethash :next callbacks) value))
            (:send-completed (funcall (gethash :completed callbacks))))))))) ;; where to dispatch these?

(provide 'lifted)

;;; lifted.el ends here
