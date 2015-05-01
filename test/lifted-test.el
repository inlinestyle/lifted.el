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
  (lexical-let* ((test-signal (lifted:make-test-signal))
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
  (lexical-let* ((test-signal (lifted:make-test-signal))
                 (test-map-signal (lifted:filter (lambda (value) (evenp value)) test-signal)))
    (funcall test-signal :subscribe-next
             (lambda (value) (lifted:log "%s" value)))
    (funcall test-map-signal :subscribe-next
             (lambda (value) (lifted:log "%s" value)))
    (lifted:log-should-equal '())
    (lifted:trigger-test-hooks 6)
    (lifted:trigger-test-hooks 7)
    (lifted:trigger-test-hooks 4)
    (lifted:trigger-test-hooks 2)
    (lifted:trigger-test-hooks 3)
    (lifted:log-should-equal '("3" "2" "2" "4" "4" "7" "6" "6"))))
