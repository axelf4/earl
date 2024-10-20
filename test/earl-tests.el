;;; earl-tests.el  -*- lexical-binding: t -*-

(require 'ert)
(require 'earl)

(defun earl-binary-to-term (&rest args)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (apply #'insert args)
    (goto-char (point-min))
    (cl-assert (eq (get-byte) earl-ext-version))
    (forward-char)
    (earl-read)))

(defun earl-term-to-binary (term)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert earl-ext-version)
    (earl-write term)
    (buffer-substring-no-properties (point-min) (point-max))))

(ert-deftest earl-gen-digest-test ()
  (should (equal (earl--gen-digest #xb0babeef "kaka")
                 "\327k1\f\326ck'\344\263m\C-F\305P\C-KP")))

(ert-deftest earl-read-test ()
  (should (eq (earl-binary-to-term 131 97 #xff) 255))
  (should (eq (earl-binary-to-term 131 98 #xff #xff #xfc #x18) -1000))
  (should (equal (earl-binary-to-term 131 104 3 97 1 97 2 97 3) [1 2 3]))
  (should (equal (earl-binary-to-term 131 108 0 0 0 1 106 106) '(nil)))
  (should (eq (earl-binary-to-term 131 98 255 255 255 255) -1)))

(ert-deftest earl-write-test ()
  (should (equal (earl-term-to-binary -1) "\203b\377\377\377\377"))
  (should (equal (earl-term-to-binary (- #x80000000)) "\203b\200\0\0\0"))
  (should (equal (earl-term-to-binary '(nil)) "\203l\0\0\0\1jj"))
  (should (equal (earl-term-to-binary [rex "ok"]) "\203h\2w\3rexm\0\0\0\2ok"))

  (should (equal (earl-term-to-binary 1.1337) "\203F?\362#\242\234w\232k")))

(ert-deftest earl-ext-roundtrip-test ()
  (dolist (x `((0 . "x") [atom 0.1] [,earl-tag nil]))
    (should (equal (earl-binary-to-term (earl-term-to-binary x)) x))))

(ert-deftest earl-reference-unique-test ()
  (should-not (equal (earl-make-ref) (earl-make-ref))))

(ert-deftest earl-name-registry-test ()
  (unwind-protect
      (progn (earl-register 'foo (earl-self))
             (should (equal (earl-whereis 'foo) (earl-self))))
    (earl-register 'foo nil))
  (should (null (earl-whereis 'foo))))

(ert-deftest earl-selective-receive-test ()
  (let* (result
         (pid (earl-spawn
               (iter-make (earl-receive (2)) (setq result earl--mailbox)))))
    (dotimes (i 3) (! pid i))
    (while (null result) (earl--run))
    (should (equal result '(0 1)))))

(ert-deftest earl-exit-test ()
  (let ((pid (earl-spawn (iter-make (while t (earl-yield))))))
    (earl-exit pid 'kill)
    (while (gethash pid earl--processes) (earl--run))))

(ert-deftest earl-link-test ()
  (let ((pid (earl-spawn
              (iter-make (earl-spawn-link (iter-make (signal 'error nil)))
                         (while t (earl-yield))))))
    (while (gethash pid earl--processes) (earl--run))))

(ert-deftest earl-timeout-test ()
  (should (eq (earl-receive :after 0 'timeout) 'timeout)))

;; Local Variables:
;; read-symbol-shorthands: (("!" . "earl-send"))
;; End:
