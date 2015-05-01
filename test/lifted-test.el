(require 'ert)
(require 'lifted)


;; Some helper fixtures & methods

(defvar lifted:test-hooks '())

(defun lifted:trigger-test-hooks (value)
  (dolist (hook lifted:test-hooks)
    (funcall hook value)))

(defun lifted:clear-test-hooks ()
  (setq lifted:test-hooks '()))

(defun lifted:make-test-signal ()
  (lifted:signal
   (lambda (subscriber)
     (lexical-let ((subscriber subscriber))
       (add-to-list 'lifted:test-hooks
                    (lambda (value) (funcall subscriber :send-next value)))))))

;; Actual tests

(ert-deftest lifted-test-signal-subscribe-next ()
  (lifted:clear-test-hooks)
  (lexical-let* ((sentinel '()))
    (funcall (lifted:make-test-signal) :subscribe-next
             (lambda (value) (add-to-list 'sentinel value)))
    (should (equal sentinel '()))
    (lifted:trigger-test-hooks "testing")
    (should (equal sentinel '("testing")))))

(ert-deftest lifted-test-signal-subscribe-next-with-multiple-subscribers ()
  (lifted:clear-test-hooks)
  (lexical-let* ((sentinel '())
                 (test-signal (lifted:make-test-signal)))
    (funcall test-signal :subscribe-next
             (lambda (value) (add-to-list 'sentinel (format "%s0" value))))
    (funcall test-signal :subscribe-next
             (lambda (value) (add-to-list 'sentinel (format "%s1" value))))
    (should (equal sentinel '()))
    (lifted:trigger-test-hooks "testing")
    (should (equal sentinel '("testing0" "testing1")))))

(ert-deftest lifted-test-subscriber-send-next ()
  (lexical-let* ((sentinel '())
                 (subscriber (lifted:subscriber :next (lambda (value)
                                                        (should (equal value "testing"))
                                                        (add-to-list 'sentinel (format "%s0" value))))))
    (should (equal sentinel '()))
    (funcall subscriber :send-next "testing")
    (should (equal sentinel '("testing0")))))

(ert-deftest lifted-test-map ()
  (lifted:clear-test-hooks)
  (lexical-let* ((sentinel '())
                 (test-signal (lifted:make-test-signal))
                 (test-map-signal (lifted:map (lambda (value) (* value 3)) test-signal)))
    (funcall test-signal :subscribe-next
             (lambda (value) (add-to-list 'sentinel value)))
    (funcall test-map-signal :subscribe-next
             (lambda (value) (add-to-list 'sentinel value)))
    (should (equal sentinel '()))
    (lifted:trigger-test-hooks 6)
    (should (equal sentinel '(6 18)))))

(ert-deftest lifted-test-map-with-multiple-subscribers ()
  (lifted:clear-test-hooks)
  (lexical-let* ((sentinel '())
                 (test-signal (lifted:make-test-signal))
                 (test-map-signal (lifted:map (lambda (value)
                                                (setq sentinel (cons "in-map" sentinel))
                                                (format "%s:mapped" value)) test-signal)))
    (funcall test-map-signal :subscribe-next
             (lambda (value) (add-to-list 'sentinel (format "%s:subscribed0" value))))
    (funcall test-map-signal :subscribe-next
             (lambda (value) (add-to-list 'sentinel (format "%s:subscribed1" value))))
    (should (equal sentinel '()))
    (lifted:trigger-test-hooks "testing")
    (should (equal sentinel '("testing:mapped:subscribed0"
                              "in-map"
                              "testing:mapped:subscribed1"
                              "in-map")))))
