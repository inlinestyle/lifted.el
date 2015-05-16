;;; -*- lexical-binding: t -*-

(require 'ert)
(require 'lifted)

;; Some helper fixtures & methods

(defvar lifted:test-log '())

(defun lifted:clear-test-fixtures ()
  (setq lifted:test-hook-0 '())
  (setq lifted:test-hook-1 '())
  (setq lifted:test-log '()))

(defun lifted:log-should-equal (log)
  (should (equal lifted:test-log log)))

(defun lifted:log (format-string &rest args)
  (setq lifted:test-log (cons (apply 'format format-string args)
                              lifted:test-log)))

(defun lifted:trigger-test-hook-0 (value)
  (run-hook-with-args 'lifted:test-hook-0 value))

(defun lifted:trigger-test-hook-1 (value)
  (run-hook-with-args 'lifted:test-hook-1 value))

(defun lifted:make-test-signal-0 ()
  (lifted:signal-for-hook-with-args 'lifted:test-hook-0))

(defun lifted:make-test-signal-1 ()
  (lifted:signal-for-hook-with-args 'lifted:test-hook-1))

;; Actual tests

(ert-deftest lifted-test-signal-subscribe-next ()
  (lifted:clear-test-fixtures)
  (funcall (lifted:make-test-signal-0) :subscribe-next
           (lambda (value) (lifted:log (car value))))
  (lifted:log-should-equal '())
  (lifted:trigger-test-hook-0 "testing")
  (lifted:log-should-equal '("testing")))

(ert-deftest lifted-test-signal-subscribe-next-with-multiple-subscribers ()
  (lifted:clear-test-fixtures)
  (let ((test-signal (lifted:make-test-signal-0)))
    (funcall test-signal :subscribe-next
             (lambda (value) (lifted:log "%s0" (car value))))
    (funcall test-signal :subscribe-next
             (lambda (value) (lifted:log "%s1" (car value))))
    (lifted:log-should-equal '())
    (lifted:trigger-test-hook-0 "testing")
    (lifted:log-should-equal '("testing0" "testing1"))))

(ert-deftest lifted-test-subscriber-send-next ()
  (lifted:clear-test-fixtures)
  (let ((subscriber (lifted:subscriber :next (lambda (value)
                                               (should (equal value "testing"))
                                               (lifted:log "%s0" value)))))
    (lifted:log-should-equal '())
    (funcall subscriber :send-next "testing")
    (lifted:log-should-equal '("testing0"))))

(ert-deftest lifted-test-map ()
  (lifted:clear-test-fixtures)
  (let* ((test-signal (lifted:make-test-signal-0))
         (test-map-signal (lifted:map (lambda (value) (* (car value) 3)) test-signal)))
    (funcall test-signal :subscribe-next
             (lambda (value) (lifted:log "%s" (car value))))
    (funcall test-map-signal :subscribe-next
             (lambda (value) (lifted:log "%s" value)))
    (lifted:log-should-equal '())
    (lifted:trigger-test-hook-0 6)
    (lifted:log-should-equal '("6" "18"))))

(ert-deftest lifted-test-map-with-multiple-subscribers ()
  (lifted:clear-test-fixtures)
  (let* ((test-signal (lifted:make-test-signal-0))
         (test-map-signal (lifted:map (lambda (value)
                                        (lifted:log "in-map")
                                        (format "%s:mapped" (car value)))
                                      test-signal)))
    (funcall test-map-signal :subscribe-next
             (lambda (value) (lifted:log "%s:subscribed0" value)))
    (funcall test-map-signal :subscribe-next
             (lambda (value) (lifted:log "%s:subscribed1" value)))
    (lifted:log-should-equal '())
    (lifted:trigger-test-hook-0 "testing")
    (lifted:log-should-equal '("testing:mapped:subscribed0"
                               "in-map"
                               "testing:mapped:subscribed1"
                               "in-map"))))

(ert-deftest lifted-test-filter ()
  (lifted:clear-test-fixtures)
  (let* ((test-signal (lifted:make-test-signal-0))
         (test-filter-signal (lifted:filter (lambda (value) (= (% (car value) 2) 0)) test-signal)))
    (funcall test-signal :subscribe-next
             (lambda (value) (lifted:log "%s" (car value))))
    (funcall test-filter-signal :subscribe-next
             (lambda (value) (lifted:log "%s" (car value))))
    (lifted:log-should-equal '())
    (lifted:trigger-test-hook-0 6)
    (lifted:trigger-test-hook-0 7)
    (lifted:trigger-test-hook-0 4)
    (lifted:trigger-test-hook-0 2)
    (lifted:trigger-test-hook-0 3)
    (lifted:log-should-equal '("3" "2" "2" "4" "4" "7" "6" "6"))))

(ert-deftest lifted-test-merged ()
  (lifted:clear-test-fixtures)
  (let* ((test-signal-0 (lifted:make-test-signal-0))
         (test-signal-1 (lifted:make-test-signal-1))
         (merged-signal (lifted:merged test-signal-0 test-signal-1)))
    (funcall merged-signal :subscribe-next
             (lambda (value) (lifted:log "merged:%s" (car value))))
    (lifted:log-should-equal '())
    (lifted:trigger-test-hook-0 "test-0-0")
    (lifted:trigger-test-hook-1 "test-1-0")
    (lifted:trigger-test-hook-1 "test-1-1")
    (lifted:trigger-test-hook-0 "test-0-1")
    (lifted:trigger-test-hook-1 "test-1-2")
    (lifted:trigger-test-hook-0 "test-0-2")
    (lifted:log-should-equal '("merged:test-0-2"
                               "merged:test-1-2"
                               "merged:test-0-1"
                               "merged:test-1-1"
                               "merged:test-1-0"
                               "merged:test-0-0"))))

(ert-deftest lifted-test-flatten-map-with-multiple-subscribers ()
  "Tests lifted:flatten-map.
-> Our test mapping function takes a string, and returns a signal wired up
to a dummy external source (`external-hooks').
-> The signals returned by applying the mapping function emits the output of
that source (really just calling callbacks) concatenated with the value
input to the mapping function that spawned the signal.
-> We set up two subscribers to the resulting 'signal of signals' to make
sure that our mapping function gets called for each."
  (lifted:clear-test-fixtures)
  (let* ((test-signal (lifted:make-test-signal-0))
         (test-flatten-map-signal (lifted:flatten-map
                                   (lambda (local-value)
                                     (lifted:make-test-signal-1))
                                   test-signal)))
    (funcall test-signal :subscribe-next
             (lambda (value) (lifted:log "%s:subscribed0" (car value))))
    (funcall test-flatten-map-signal :subscribe-next
             (lambda (value) (lifted:log "%s:subscribed1" (car value))))
    (funcall test-flatten-map-signal :subscribe-next
             (lambda (value) (lifted:log "%s:subscribed2" (car value))))
    (lifted:log-should-equal '())
    (lifted:trigger-test-hook-0 "local0")
    (lifted:trigger-test-hook-1 "external0")
    (lifted:trigger-test-hook-1 "external1")
    (lifted:trigger-test-hook-0 "local1")
    (lifted:trigger-test-hook-1 "external2")
    (lifted:log-should-equal '("external2:subscribed2"
                               "external2:subscribed1"
                               "external2:subscribed2"
                               "external2:subscribed1"
                               "local1:subscribed0"
                               "external1:subscribed2"
                               "external1:subscribed1"
                               "external0:subscribed2"
                               "external0:subscribed1"
                               "local0:subscribed0"))))

(ert-deftest lifted-test-signal-for-hook ()
  (lifted:clear-test-fixtures)
  (let ((hook-signal (lifted:signal-for-hook 'lifted:test-hook-0)))
    (funcall hook-signal :subscribe-next (lambda (value) (lifted:log "subscribed0:%s" value)))
    (funcall hook-signal :subscribe-next (lambda (value) (lifted:log "subscribed1:%s" value)))
    (lifted:log-should-equal '())
    (run-hooks 'lifted:test-hook-0)
    (lifted:log-should-equal '("subscribed0:t"
                               "subscribed1:t"))))

(ert-deftest lifted-test-signal-for-hook-with-args ()
  (lifted:clear-test-fixtures)
  (let ((hook-signal (lifted:signal-for-hook-with-args 'lifted:test-hook-0)))
    (funcall hook-signal :subscribe-next (lambda (&rest args) (lifted:log "subscribed0:%s" (caar args))))
    (funcall hook-signal :subscribe-next (lambda (&rest args) (lifted:log "subscribed1:%s" (caar args))))
    (lifted:log-should-equal '())
    (run-hook-with-args 'lifted:test-hook-0 "foo")
    (lifted:log-should-equal '("subscribed0:foo"
                               "subscribed1:foo"))))
