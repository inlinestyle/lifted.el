;;; -*- lexical-binding: t -*-

(require 'ert)
(require 'lifted)

;; Some helper fixtures & methods

(defvar lifted:test-log '())

(defun lifted:clear-test-fixtures ()
  (setq lifted:test-hook-0 '())
  (setq lifted:test-hook-1 '())
  (setq lifted:test-hook-2 '())
  (setq lifted:test-log '()))

(defun lifted:log-should-equal (log)
  (should (equal lifted:test-log log)))

(defun lifted:log (format-string &rest args)
  (setq lifted:test-log (cons (apply #'format format-string args)
                              lifted:test-log)))

(defun lifted:run-test-hook-0 (value)
  (run-hook-with-args 'lifted:test-hook-0 value))

(defun lifted:run-test-hook-1 (value)
  (run-hook-with-args 'lifted:test-hook-1 value))

(defun lifted:run-test-hook-2 (value)
  (run-hook-with-args 'lifted:test-hook-2 value))

(defun lifted:make-test-signal-0 ()
  (lifted:map #'car (lifted:signal-for-hook-with-args 'lifted:test-hook-0)))

(defun lifted:make-test-signal-1 ()
  (lifted:map #'car (lifted:signal-for-hook-with-args 'lifted:test-hook-1)))

(defun lifted:make-test-signal-2 ()
  (lifted:map #'car (lifted:signal-for-hook-with-args 'lifted:test-hook-2)))

;; Actual tests

(ert-deftest lifted-test-signal-subscribe-next ()
  (lifted:clear-test-fixtures)
  (funcall (lifted:make-test-signal-0) :subscribe-next
           (lambda (value) (lifted:log value)))
  (lifted:log-should-equal '())
  (lifted:run-test-hook-0 "testing")
  (lifted:log-should-equal '("testing")))

(ert-deftest lifted-test-signal-subscribe-next-with-multiple-subscribers ()
  (lifted:clear-test-fixtures)
  (let ((test-signal (lifted:make-test-signal-0)))
    (funcall test-signal :subscribe-next
             (lambda (value) (lifted:log "%s0" value)))
    (funcall test-signal :subscribe-next
             (lambda (value) (lifted:log "%s1" value)))
    (lifted:log-should-equal '())
    (lifted:run-test-hook-0 "testing")
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
         (test-map-signal (lifted:map (lambda (value) (* value 3)) test-signal)))
    (funcall test-signal :subscribe-next
             (lambda (value) (lifted:log "%s" value)))
    (funcall test-map-signal :subscribe-next
             (lambda (value) (lifted:log "%s" value)))
    (lifted:log-should-equal '())
    (lifted:run-test-hook-0 6)
    (lifted:log-should-equal '("6" "18"))))

(ert-deftest lifted-test-map-chaining ()
  (lifted:clear-test-fixtures)
    (let* ((test-signal (lifted:make-test-signal-0))
         (test-map-signal (funcall test-signal :map (lambda (value) (* value 3)))))
    (funcall test-signal :subscribe-next
             (lambda (value) (lifted:log "%s" value)))
    (funcall test-map-signal :subscribe-next
             (lambda (value) (lifted:log "%s" value)))
    (lifted:log-should-equal '())
    (lifted:run-test-hook-0 6)
    (lifted:log-should-equal '("6" "18"))))

(ert-deftest lifted-test-map-with-multiple-subscribers ()
  (lifted:clear-test-fixtures)
  (let* ((test-signal (lifted:make-test-signal-0))
         (test-map-signal (lifted:map (lambda (value)
                                        (lifted:log "in-map")
                                        (format "%s:mapped" value))
                                      test-signal)))
    (funcall test-map-signal :subscribe-next
             (lambda (value) (lifted:log "%s:subscribed0" value)))
    (funcall test-map-signal :subscribe-next
             (lambda (value) (lifted:log "%s:subscribed1" value)))
    (lifted:log-should-equal '())
    (lifted:run-test-hook-0 "testing")
    (lifted:log-should-equal '("testing:mapped:subscribed0"
                               "in-map"
                               "testing:mapped:subscribed1"
                               "in-map"))))

(ert-deftest lifted-test-filter ()
  (lifted:clear-test-fixtures)
  (let* ((test-signal (lifted:make-test-signal-0))
         (test-filter-signal (lifted:filter (lambda (value) (= (% value 2) 0)) test-signal)))
    (funcall test-signal :subscribe-next
             (lambda (value) (lifted:log "%s" value)))
    (funcall test-filter-signal :subscribe-next
             (lambda (value) (lifted:log "%s" value)))
    (lifted:log-should-equal '())
    (lifted:run-test-hook-0 6)
    (lifted:run-test-hook-0 7)
    (lifted:run-test-hook-0 4)
    (lifted:run-test-hook-0 2)
    (lifted:run-test-hook-0 3)
    (lifted:log-should-equal '("3" "2" "2" "4" "4" "7" "6" "6"))))

(ert-deftest lifted-test-merge ()
  (lifted:clear-test-fixtures)
  (let* ((test-signal-0 (lifted:make-test-signal-0))
         (test-signal-1 (lifted:make-test-signal-1))
         (merged-signal (lifted:merge test-signal-0 test-signal-1)))
    (funcall merged-signal :subscribe-next
             (lambda (value) (lifted:log "merged:%s" value)))
    (lifted:log-should-equal '())
    (lifted:run-test-hook-0 "test-0-a")
    (lifted:run-test-hook-1 "test-1-a")
    (lifted:run-test-hook-1 "test-1-b")
    (lifted:run-test-hook-0 "test-0-b")
    (lifted:run-test-hook-1 "test-1-c")
    (lifted:run-test-hook-0 "test-0-c")
    (lifted:log-should-equal '("merged:test-0-c"
                               "merged:test-1-c"
                               "merged:test-0-b"
                               "merged:test-1-b"
                               "merged:test-1-a"
                               "merged:test-0-a"))))


(ert-deftest lifted-test-merge-chaining ()
  (lifted:clear-test-fixtures)
  (let* ((test-signal-0 (lifted:make-test-signal-0))
         (test-signal-1 (lifted:make-test-signal-1))
         (merged-signal (funcall test-signal-0 :merge test-signal-1)))
    (funcall merged-signal :subscribe-next
             (lambda (value) (lifted:log "merged:%s" value)))
    (lifted:log-should-equal '())
    (lifted:run-test-hook-0 "test-0-a")
    (lifted:run-test-hook-1 "test-1-a")
    (lifted:run-test-hook-1 "test-1-b")
    (lifted:run-test-hook-0 "test-0-b")
    (lifted:run-test-hook-1 "test-1-c")
    (lifted:run-test-hook-0 "test-0-c")
    (lifted:log-should-equal '("merged:test-0-c"
                               "merged:test-1-c"
                               "merged:test-0-b"
                               "merged:test-1-b"
                               "merged:test-1-a"
                               "merged:test-0-a"))))

(ert-deftest lifted-test-combine-latest ()
  (lifted:clear-test-fixtures)
  (let* ((test-signal-0 (lifted:make-test-signal-0))
         (test-signal-1 (lifted:make-test-signal-1))
         (test-signal-2 (lifted:make-test-signal-2))
         (combined-signal (lifted:combine-latest test-signal-0 test-signal-1 test-signal-2)))
    (funcall combined-signal :subscribe-next
             (lambda (value) (lifted:log "combined: %s" value)))
    (lifted:log-should-equal '())
    (lifted:run-test-hook-0 "test-0-a")
    (lifted:run-test-hook-2 "test-2-a")
    (lifted:run-test-hook-2 "test-2-b")
    (lifted:run-test-hook-1 "test-1-a")
    (lifted:run-test-hook-0 "test-0-b")
    (lifted:run-test-hook-0 "test-0-c")
    (lifted:run-test-hook-2 "test-2-c")
    (lifted:log-should-equal '("combined: (test-0-c test-1-a test-2-c)"
                               "combined: (test-0-c test-1-a test-2-b)"
                               "combined: (test-0-b test-1-a test-2-b)"
                               "combined: (test-0-a test-1-a test-2-b)"))))

(ert-deftest lifted-test-flatten-map-with-multiple-subscribers ()
  (lifted:clear-test-fixtures)
  (let* ((test-signal (lifted:make-test-signal-0))
         (test-flatten-map-signal (lifted:flatten-map
                                   (lambda (local-value) (lifted:make-test-signal-1))
                                   test-signal)))
    (funcall test-signal :subscribe-next
             (lambda (value) (lifted:log "%s:subscribed0" value)))
    (funcall test-flatten-map-signal :subscribe-next
             (lambda (value) (lifted:log "%s:subscribed1" value)))
    (funcall test-flatten-map-signal :subscribe-next
             (lambda (value) (lifted:log "%s:subscribed2" value)))
    (lifted:log-should-equal '())
    (lifted:run-test-hook-0 "local0")
    (lifted:run-test-hook-1 "external0")
    (lifted:run-test-hook-1 "external1")
    (lifted:run-test-hook-0 "local1")
    (lifted:run-test-hook-1 "external2")
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

(ert-deftest lifted-test-defer ()
  (lifted:clear-test-fixtures)
  (let* ((test-signal (lifted:make-test-signal-0))
         (deferred-signal (lifted:defer test-signal)))
    (funcall deferred-signal :subscribe-next
             (lambda (value) (lifted:log "%s" value)))
    (lifted:log-should-equal '())
    (lifted:run-test-hook-0 "deferred")
    (lifted:log-should-equal '())
    (deferred:flush-queue!)
    (lifted:log-should-equal '("deferred"))))

(ert-deftest lifted-test-defer-chaining ()
  (lifted:clear-test-fixtures)
  (let* ((test-signal (lifted:make-test-signal-0))
         (deferred-signal (funcall test-signal :defer)))
    (funcall deferred-signal :subscribe-next
             (lambda (value) (lifted:log "%s" value)))
    (lifted:log-should-equal '())
    (lifted:run-test-hook-0 "deferred")
    (lifted:log-should-equal '())
    (deferred:flush-queue!)
    (lifted:log-should-equal '("deferred"))))

(ert-deftest lifted-test-defer-map-chaining ()
  (lifted:clear-test-fixtures)
  (let* ((test-signal (lifted:make-test-signal-0)))
    (funcall test-signal
             :defer
             :map (lambda (value) (* value 3))
             :subscribe-next (lambda (value) (lifted:log "%s" value)))
    (lifted:log-should-equal '())
    (lifted:run-test-hook-0 6)
    (lifted:log-should-equal '())
    (deferred:flush-queue!)
    (lifted:log-should-equal '("18"))))

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
