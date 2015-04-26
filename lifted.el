;;; lifted.el --- Functional reactive programming library for Emacs Lisp

;; Copyright (C) 2015 Ben Yelsey

;; Author: Ben Yelsey <ben.yelsey@gmail.com>
;; Version: 0.0.1
;; Keywords: reactive

;;; Commentary:

;; This is a rough draft of an FRP library for Emacs. I'm working from a number of inspirations, primarily ReactiveCocoa and Elm.
;; Obviously there's a lot left to do, including but not limited to:
;;  - real scheduling (possibly using deferred.el)
;;  - lots and lots of functional operators
;;  - signal disposal
;;  - a real concept of "sequences"
;;  - error handling

;;; Samples (will almost certainly change):

;; ** Making a signal

;; (defvar user-commands-signal
;;   (lifted:signal
;;    (lambda (subscriber)
;;      (lexical-let ((subscriber subscriber))
;;        (add-hook 'post-command-hook
;;                  (lambda ()
;;                    (funcall subscriber :send-next this-command)))))))

;; ** Subscribing

;; (funcall user-commands-signal
;;          :subscribe-next (lambda (command) (message "got command: %s" command)))

;; ** Chaining subscriptions

;; (funcall (funcall user-commands-signal
;;                   :subscribe-next (lambda (command) (message "got command: %s" command)))
;;          :subscribe-next (lambda (command) (message "also got command: %s" command)))

;;; Code:

(defun lifted:signal (body &optional state)
  "Creates a 'signal' closure"
  (lexical-let ((body body)
                (state (if (hash-table-p state) state (make-hash-table))))
    (lambda (&rest commands)
      (while commands
        (let ((command (pop commands))
              (callback (pop commands)))
          (if callback
              (let ((subscriber (pcase command
                                  (:subscribe-next (lifted:subscriber :next callback))
                                  (:subscribe-completed (lifted:subscriber :completed callback)))))
                (lifted--subscribe body state subscriber))
            (message "Missing callback for %s" command))))
      (lifted:signal body state))))

(defun lifted:subscriber (&rest commands)
  "Creates a 'subscriber' closure"
  (lexical-let ((callbacks (make-hash-table)))
    (while commands 
      (let ((command (pop commands))
            (callback (pop commands)))
        (if callback
            (puthash command callback callbacks)
          (message "Missing callback for %s" command))))
    (lambda (&rest commands)
      (while commands
        (let ((command (pop commands))
              (value (pop commands)))
          (pcase command
            (:send-next (funcall (gethash :next callbacks) value))
            (:send-completed (funcall (gethash :completed callbacks))))))))) ;; where to dispatch these?

(defun lifted--cons-in (key value table)
  "Helper for manipulating lists in hash-tables."
  (let ((existing (gethash key table)))
    (puthash key (cons value existing) table)))

(defun lifted--subscribe (body state subscriber)
  "Wires up subscribers to the signal's executable and state."
  (funcall body subscriber)
  (lifted--cons-in :subscribers subscriber state))

(provide 'lifted)

;;; lifted.el ends here
