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
      (if (not commands)
          (lifted:signal body state)
        (lexical-let ((command (pop commands))
                      (callback (pop commands)))
          (unless callback
            (error "Missing callback for %s" command))
          (let ((new-signal (pcase command
                              (:map
                               (lexical-let* ((copy-signal (lifted:signal body)))
                                 (lifted:signal
                                  (lambda (subscriber)
                                    (lexical-let ((subscriber subscriber))
                                      (funcall copy-signal :subscribe-next
                                               (lambda (value)
                                                 (funcall subscriber :send-next
                                                          (funcall callback value)))))))))
                              (:subscribe-next
                               (progn
                                 (lifted--subscribe body state (lifted:subscriber :next callback))
                                 (lifted:signal body state)))
                              (:subscribe-completed
                               (progn
                                 (lifted--subscribe body state (lifted:subscriber :completed callback))
                                 (lifted:signal body state))))))
            (apply new-signal commands)))))))

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
