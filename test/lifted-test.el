(require 'ert)
(require 'lifted)


;; Some helper fixtures & methods

(defvar lifted:test-hooks '())
(defvar lifted:test-log '())

(defun lifted:clear-test-fixtures ()
  (setq lifted:test-hooks '())
  (setq lifted:test-log '()))

(defun lifted:log-should-equal (log)
  (should (equal lifted:test-log log)))

(defun lifted:log (format-string &rest args)
  (setq lifted:test-log (cons (apply 'format format-string args)
                              lifted:test-log)))

(defun lifted:trigger-test-hooks (value)
  (dolist (hook lifted:test-hooks)
    (funcall hook value)))

(defun lifted:make-test-signal ()
  (lifted:signal
   (lambda (subscriber)
     (lexical-let ((subscriber subscriber))
       (add-to-list 'lifted:test-hooks
                    (lambda (value) (funcall subscriber :send-next value)))))))

;; Actual tests

(ert-deftest lifted-test-signal-subscribe-next ()
  (lifted:clear-test-fixtures)
  (funcall (lifted:make-test-signal) :subscribe-next
           (lambda (value) (lifted:log value)))
  (lifted:log-should-equal '())
  (lifted:trigger-test-hooks "testing")
  (lifted:log-should-equal '("testing")))

(ert-deftest lifted-test-signal-subscribe-next-with-multiple-subscribers ()
  (lifted:clear-test-fixtures)
  (let ((test-signal (lifted:make-test-signal)))
    (funcall test-signal :subscribe-next
             (lambda (value) (lifted:log "%s0" value)))
    (funcall test-signal :subscribe-next
             (lambda (value) (lifted:log "%s1" value)))
    (lifted:log-should-equal '())
    (lifted:trigger-test-hooks "testing")
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
  (let* ((test-signal (lifted:make-test-signal))
         (test-map-signal (lifted:map (lambda (value) (* value 3)) test-signal)))
    (funcall test-signal :subscribe-next
             (lambda (value) (lifted:log "%s" value)))
    (funcall test-map-signal :subscribe-next
             (lambda (value) (lifted:log "%s" value)))
    (lifted:log-should-equal '())
    (lifted:trigger-test-hooks 6)
    (lifted:log-should-equal '("6" "18"))))

(ert-deftest lifted-test-map-with-multiple-subscribers ()
  (lifted:clear-test-fixtures)
  (let* ((test-signal (lifted:make-test-signal))
         (test-map-signal (lifted:map (lambda (value)
                                        (lifted:log "in-map")
                                        (format "%s:mapped" value))
                                      test-signal)))
    (funcall test-map-signal :subscribe-next
             (lambda (value) (lifted:log "%s:subscribed0" value)))
    (funcall test-map-signal :subscribe-next
             (lambda (value) (lifted:log "%s:subscribed1" value)))
    (lifted:log-should-equal '())
    (lifted:trigger-test-hooks "testing")
    (lifted:log-should-equal '("testing:mapped:subscribed0"
                               "in-map"
                               "testing:mapped:subscribed1"
                               "in-map"))))

(ert-deftest lifted-test-filter ()
  (lifted:clear-test-fixtures)
  (let* ((test-signal (lifted:make-test-signal))
         (test-filter-signal (lifted:filter (lambda (value) (evenp value)) test-signal)))
    (funcall test-signal :subscribe-next
             (lambda (value) (lifted:log "%s" value)))
    (funcall test-filter-signal :subscribe-next
             (lambda (value) (lifted:log "%s" value)))
    (lifted:log-should-equal '())
    (lifted:trigger-test-hooks 6)
    (lifted:trigger-test-hooks 7)
    (lifted:trigger-test-hooks 4)
    (lifted:trigger-test-hooks 2)
    (lifted:trigger-test-hooks 3)
    (lifted:log-should-equal '("3" "2" "2" "4" "4" "7" "6" "6"))))

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
  (lexical-let* ((external-hooks '()))
    (let* ((test-signal (lifted:make-test-signal))
           (test-flatten-map-signal (lifted:flatten-map
                                     (lambda (local-value)
                                       (lexical-let ((local-value local-value))
                                         (lifted:signal (lambda (subscriber)
                                                          (lexical-let ((subscriber subscriber))
                                                            (add-to-list 'external-hooks
                                                                         (lambda (external-value)
                                                                           (lifted:log "in-map-to-external-signal")
                                                                           (funcall subscriber :send-next (format "%s:%s" local-value external-value)))))))))
                                     test-signal)))
      (funcall test-signal :subscribe-next
               (lambda (value) (lifted:log "%s:subscribed0" value)))
      (funcall test-flatten-map-signal :subscribe-next
               (lambda (value) (lifted:log "%s:subscribed1" value)))
      (funcall test-flatten-map-signal :subscribe-next
               (lambda (value) (lifted:log "%s:subscribed2" value)))
      (lifted:log-should-equal '())
      (lifted:trigger-test-hooks "local0")
      (dolist (hook external-hooks) (funcall hook "external0"))
      (dolist (hook external-hooks) (funcall hook "external1"))
      (lifted:trigger-test-hooks "local1")
      (dolist (hook external-hooks) (funcall hook "external2"))
      (lifted:log-should-equal '("local0:external2:subscribed2"
                                 "in-map-to-external-signal"
                                 "local0:external2:subscribed1"
                                 "in-map-to-external-signal"
                                 "local1:external2:subscribed2"
                                 "in-map-to-external-signal"
                                 "local1:external2:subscribed1"
                                 "in-map-to-external-signal"
                                 "local1:subscribed0"
                                 "local0:external1:subscribed2"
                                 "in-map-to-external-signal"
                                 "local0:external1:subscribed1"
                                 "in-map-to-external-signal"
                                 "local0:external0:subscribed2"
                                 "in-map-to-external-signal"
                                 "local0:external0:subscribed1"
                                 "in-map-to-external-signal"
                                 "local0:subscribed0")))))

