;;; lifted.el --- Functional reactive programming library for Emacs Lisp -*- lexical-binding: t -*-

;; Copyright (C) 2015 Ben Yelsey

;; Author: Ben Yelsey <ben.yelsey@gmail.com>
;; Version: 0.0.1
;; Keywords: reactive

;;; Commentary:

;; This is a rough draft of an FRP library for Emacs. I'm working from a number of inspirations, primarily ReactiveCocoa and Elm.
;; There's a lot left to do, including but not limited to:
;;  - many more functional operators
;;  - more diverse deferral options
;;  - multicasting
;;  - signal disposal
;;  - error handling
;;  - throttling/debouncing
;;  - sequences & cold signals
;;  - possibly reducing the number of `funcall`s
;;  - cask build
;;  - getting on MELPA

;;; Samples (will almost certainly change):

;; ** Making a key signal

;; (defvar key-signal (lifted:signal-for-key (kbd "C-c s")))

;; ** Making a hook signal

;; (defvar post-command-signal (lifted:signal-for-hook 'post-command-hook))

;; ** Making a signal

;; (defvar post-command-signal
;;   "Could also do this using (lifted:signal-for-hook 'post-command-hook)"
;;   (lifted:signal
;;    (lambda (subscriber)
;;      (add-hook 'post-command-hook
;;                (lambda ()
;;                  (funcall subscriber :send-next this-command))))))

;; ** Subscribing

;; (funcall post-command-signal
;;          :subscribe-next (lambda (command) (message "got command: %s" command)))

;; ** Chaining subscriptions

;; (funcall (funcall post-command-signal
;;                   :subscribe-next (lambda (command) (message "got command: %s" command)))
;;          :subscribe-next (lambda (command) (message "also got command: %s" command)))

;; ** Functional operators

;; (lifted:map (lambda (value) (float-time)) key-signal)

;; Operators implemented so far:
;; - filter
;; - flatten
;; - flatten-map
;; - map

;; ** Merging

;; (funcall (lifted:merge key-signal post-command-signal)
;;          :subscribe-next (lambda (value) (message "got key or command: %s" value)))

;; ** Deferring

;; (lifted:defer key-signal) ; Any subscribing action will be "deferred"

;; ** Chaining

;; All "lifted:*" functions with the exceptions of `lifted:signal' and `lifted:subscriber' have a chained form.
;; The chained version of the function is used by calling an existing signal with the "keyword form" of the function
;; as an argument:

;; (lifted:map (lambda (value) (float-time)) key-signal)

;; Can be expressed as:

;; (funcall key-signal :map (lambda (value) (float-time)))

;; The "keyword form" is just the name of the function, with the "lifted:" part removed e.g. `lifted:flatten-map' -> `:flatten-map'.

;; A more involved example:

;; (funcall key-signal
;;          :map            (lambda (value) (float-time))
;;          :subscribe-next (lambda (value) (message "subscriber 1 reporting timestamp: %s" value))
;;          :subscribe-next (lambda (value) (message "subscriber 2 reporting timestamp: %s" value))
;;          :defer
;;          :map            (lambda (value) (round value))
;;          :filter         (lambda (value) (= (% value 2) 0))
;;          :subscribe-next (lambda (value) (message "subscriber 3 reporting even timestamp: %s" value)))

;;; Code:

(require 'deferred)

;; Operators

(defun lifted:map (callback base-signal)
  "Returns a signal producing the output of the `base-signal', with `callback' called on the value."
  (lifted:signal
   (lambda (subscriber)
     (funcall base-signal :subscribe-next
              (lambda (value)
                (funcall subscriber :send-next
                         (funcall callback value)))))))

(defun lifted:map-apply (callback base-signal)
  "Returns a signal producing the output of the `base-signal', with `callback' `apply'-ed on the value.
This is especially useful in tandem with `lifted:combine-latest'."
  (lifted:map (lambda (value-args) (apply callback value-args)) base-signal))

(defun lifted:flatten (base-signal)
  "Returns a signal merging the output of a signal-producing `base-signal'."
  (lifted:signal
   (lambda (subscriber)
     (funcall base-signal :subscribe-next
              (lambda (value-signal)
                (funcall value-signal :subscribe-next
                         (lambda (value)
                           (funcall subscriber :send-next value))))))))

(defun lifted:filter (callback base-signal)
  "Returns a signal producing the output of the `base-signal', if `callback' returns true when applied."
  (lifted:signal
   (lambda (subscriber)
     (funcall base-signal :subscribe-next
              (lambda (value)
                (when (funcall callback value)
                  (funcall subscriber :send-next
                           value)))))))

(defun lifted:flatten-map (callback base-signal)
  "Returns a signal merging the output of all signals produced when `callback' is mapped over `base-signal'."
  (lifted:flatten (lifted:map callback base-signal)))

(defun lifted:defer (base-signal)
  "Returns a signal which defers the output of the input signal (i.e. any subscribers will get the output asynchronously)"
  (lifted:signal
   (lambda (subscriber)
     (funcall base-signal :subscribe-next
              (lambda (value)
                (deferred:$
                  (deferred:next
                    (lambda () (funcall subscriber :send-next value)))))))))

(defun lifted:merge (&rest base-signals)
  "Returns a signal merging the output of all given `base-signals'."
  (lifted:signal
   (lambda (subscriber)
     (dolist (base-signal base-signals)
       (funcall base-signal :subscribe-next
                (lambda (value) (funcall subscriber :send-next value)))))))

(defun lifted:combine-latest (&rest base-signals)
  "Returns a signal that watches multiple signals for changes,
and sends the latest values from all of them in a list when a change occurs.
Note: I'm not happy with the implementation here (mutating a vector)."
  (lifted:signal
   (lambda (subscriber)
     (let ((values (make-vector (length base-signals) nil)))
       (dolist (pair (lifted--enumerate base-signals))
         (let ((i (car pair))
               (base-signal (cadr pair)))
           (funcall base-signal :subscribe-next
                    (lambda (value)
                      (aset values i value)
                      (let ((values-list (append values '())))
                        (if (not (member nil values-list))
                            (funcall subscriber :send-next values-list)))))))))))

(defun lifted:subscribe-next (callback base-signal)
  (let ((subscriber (lifted:subscriber :next callback)))
    (funcall base-signal :subscribe subscriber)))

(defun lifted:subscribe-error (callback base-signal)
  (let ((subscriber (lifted:subscriber :error callback)))
    (funcall base-signal :subscribe subscriber)))

(defun lifted:subscribe-completed (callback base-signal)
  (let ((subscriber (lifted:subscriber :completed callback)))
    (funcall base-signal :subscribe subscriber)))

;; Implementation of the "nouns" of the system: signals and subscribers

(defvar lifted--no-arg-commands #s(hash-table size 2 data (:defer lifted:defer :flatten lifted:flatten)))
(defun lifted--no-arg-dispatch (body subscribers command commands)
  (let* ((function (gethash command lifted--no-arg-commands))
         (base-signal (lifted:signal body subscribers))
         (new-signal (funcall function base-signal)))
    (apply new-signal commands)))

(defvar lifted--one-arg-commands
  #s(hash-table
     size 7 data
     (:filter lifted:filter :flatten-map lifted:flatten-map :map lifted:map :map-apply lifted:map-apply :subscribe t :subscribe-completed lifted:subscribe-completed :subscribe-error lifted:subscribe-error :subscribe-next lifted:subscribe-next)))
(defun lifted--one-arg-dispatch (body subscribers command commands)
  (let* ((callback (pop commands))
         (new-signal (if (eq command :subscribe)
                         (let ((subscriber callback))
                           (funcall body subscriber)
                           (lifted:signal body (cons subscriber subscribers)))
                       (let ((function (gethash command lifted--one-arg-commands))
                             (base-signal (lifted:signal body subscribers)))
                         (funcall function callback base-signal)))))
    (apply new-signal commands)))

(defvar lifted--many-arg-commands #s(hash-table size 2 data (:combine-latest lifted:combine-latest :merge lifted:merge)))
(defun lifted--many-arg-dispatch (body subscribers command commands)
  (let ((args '()))
    (while (and commands (not (keywordp (car commands))))
      (push (pop commands) args))
    (let* ((function (gethash command lifted--many-arg-commands))
           (base-signal (lifted:signal body subscribers))
           (new-signal (apply function (cons base-signal args))))
      (apply new-signal commands))))

(defun lifted:signal (body &optional subscribers)
  "Creates a 'signal' closure"
  (lambda (&rest commands)
    (if (not commands)
        (lifted:signal body subscribers)
      (let ((command (pop commands)))
        (cond ((gethash command lifted--no-arg-commands)
               (lifted--no-arg-dispatch body subscribers command commands))
              ((gethash command lifted--one-arg-commands)
               (lifted--one-arg-dispatch body subscribers command commands))
              ((gethash command lifted--many-arg-commands)
               (lifted--many-arg-dispatch body subscribers command commands))
              (t (error "Unrecognized operation: %s" command)))))))

(defun lifted:subscriber (&rest commands)
  "Creates a 'subscriber' closure"
  (let ((callbacks (make-hash-table :size 3)))
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
            (:send-error (funcall (gethash :error callbacks) value))
            (:send-completed (funcall (gethash :completed callbacks)))))))))

;; UI Utilities

(defvar lifted--signal-for-key-subscribers '())

(defun lifted:signal-for-key (key &optional key-map)
  "Returns a signal that emits `t' each time is `key' is pressed.
Binds to `key-map' if supplied, defaults to the global map.
We need to keep global state here so that multiple signals for the same key don't erase eachother."
  (define-key (or key-map (current-global-map)) key
    (lambda ()
      (interactive)
      (dolist (subscriber lifted--signal-for-key-subscribers)
        (funcall subscriber :send-next t))))
  (lifted:signal (lambda (subscriber)
                   (add-to-list 'lifted--signal-for-key-subscribers subscriber))))

(defun lifted:signal-for-hook (hook)
  "Returns a signal that emits `t' each time `hook' is run."
  (lifted:signal
   (lambda (subscriber)
     (add-hook hook
               (lambda ()
                 (funcall subscriber :send-next t))))))

(defun lifted:signal-for-hook-with-args (hook)
  "Returns a signal that emits `t' each time `hook' is run.
Assumes & passes along hook arguments."
  (lifted:signal
   (lambda (subscriber)
     (add-hook hook
               (lambda (&rest args)
                 (funcall subscriber :send-next args))))))

;; Private helpers

(defun lifted--zip (&rest lists)
  (unless (member nil lists)
    (cons (mapcar #'car lists)
          (apply #'lifted--zip (mapcar #'cdr lists)))))

(defun lifted--enumerate (sequence)
  (lifted--zip (number-sequence 0 (1- (length sequence)))
               sequence))

(provide 'lifted)

;;; lifted.el ends here
