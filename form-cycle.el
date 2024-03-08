;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'ido)

(defvar form-cycle-pattern-file nil)
(defvar form-cycle-current-cycle-state nil)
(defvar form-cycle-position nil)
(defvar form-cycle-initial-position nil)
(defvar form-cycle-new-cycle-p nil)
(defvar form-cycle-undo-previous-function nil)
(defvar form-cycle-function 'form-cycle-default-cycle-function)

(defvar form-cycle-debug nil)

(defmacro form-cycle-debug (thing &optional tag)
  (if form-cycle-debug
      (if tag
          `(princ (format "%s: %s: %s\n" ',tag ',thing ,thing))
        `(princ (format "%s: %s\n" ',thing ,thing)))
    '()))

;; Basic functionality: Inserting strings in a cycle.

(defun form-cycle-make-undo-function (length)
  (lambda ()
    (when form-cycle-initial-position
      (goto-char form-cycle-initial-position))
    (delete-char length)
    (setf form-cycle-undo-previous-function nil)))

(defun form-cycle-default-cycle-function (form)
  (unless form-cycle-undo-previous-function
    (setf form-cycle-undo-previous-function
          (lambda ()
            (when form-cycle-initial-position
              (goto-char form-cycle-initial-position))
            (form-cycle-debug form form-cycle-undo-previous-function)
            (form-cycle-debug (length form) form-cycle-undo-previous-function)
            (delete-char (length form))
            (setf form-cycle-undo-previous-function nil))))
  (insert form)
  form)

(defun form-cycle-next ()
  (form-cycle-debug form-cycle-current-cycle-state)
  (let ((next (pop form-cycle-current-cycle-state)))
    (when next
      (setf form-cycle-current-cycle-state
            (append form-cycle-current-cycle-state (list next)))
      (when form-cycle-undo-previous-function
        (funcall form-cycle-undo-previous-function))
      (funcall form-cycle-function next)
      (setf form-cycle-position (point)))))

;; Do not call this inside save-excursion, it won't work.
(defun form-cycle-skip ()
  (setf form-cycle-position (point))
  (throw 'form-cycle-skip 'form-cycle-skip))

(defvar form-cycle-skip nil)

(defun form-cycle-new-cycle-p ()
  (or (null form-cycle-position)
      (and form-cycle-position
           (not (= form-cycle-position (point)))))) ; Point has moved

(defun form-cycle-initiate (cycle)
  (let ((skip-count 0))
    (cl-tagbody
     again
     (when (eq 'form-cycle-skip
               (catch 'form-cycle-skip
                 (if (form-cycle-new-cycle-p)
                     (let ((form-cycle-new-cycle-p t))
                       (form-cycle-debug "New Cycle.")
                       (setq form-cycle-current-cycle-state cycle
                             form-cycle-undo-previous-function nil
                             form-cycle-initial-position (point))
                       (form-cycle-next))
                   (form-cycle-next))))
       (when (< skip-count (length form-cycle-current-cycle-state))
         (cl-incf skip-count)
         (go again))))))

(defun form-cycle-test ()
  (interactive)
  (form-cycle-initiate '("abc" "foo" "bar")))

;; Adding names and place point.

;; Terminology:

;; - context: The list of cars from each nested list leading up to
;;   the point. I.e. for (foo (bar (xyz <point>))) we have the
;;   context (foo bar xyz)
;;
;; - pattern: A list of symbols that may match some contexts. For
;;   example, the pattern (a b) would match the
;;   context (a (xx (b (yy <point>)))). A pattern matches as long as
;;   its elements are found in the context in the same order.
;;
;; - form: A string that may be inserted into the buffer as a result
;;   of a pattern matching.
;;
;; - form cycle: A pattern paired with a list of forms, where each
;;   form in the list is suggested to the user, one by one, if the
;;   pattern matched.
;;
;;   In function names, the abbreviation "fc" is used to indicate a
;;   form cycle data structure, (e.g. form-cycle-make-fc). A
;;   collection of form cycles may also simply be referred to as
;;   "patterns" (e.g. form-cycle-lisp-patterns).

;; TODO: Refactor the lisp-specific parts away from
;; form-cycle-with-name to make it more general.

(defvar form-cycle-current-name nil)
(defvar form-cycle-name-marker "_")
(defvar form-cycle-point-marker "@")
(defvar form-cycle-up-list-initially-p nil)
(defvar form-cycle-up-list-initially-sexp-string nil)
(defvar form-cycle-raise-list-initially-p nil)

(defun form-cycle-symbol-at-point ()
  (let ((sym (symbol-at-point)))
    (when (and sym
               (string-match "^\\_<" (symbol-name sym)))
      sym)))

(defun form-cycle-beginning-of-symbol-maybe ()
  (when (form-cycle-symbol-at-point)
    (thing-at-point--beginning-of-sexp)))

;; Point should be at the beginning of the form that was previously
;; inserted.
(defun form-cycle-with-name (form)
  ;; Initialize
  (when form-cycle-new-cycle-p
    (when form-cycle-up-list-initially-p
      (save-excursion
        (up-list -1)
        (setf form-cycle-up-list-initially-sexp-string
              (buffer-substring-no-properties
               (point)
               (progn (forward-sexp) (point))))))
    (let ((sym (symbol-at-point)))
      (cond ((and sym
                  (string-match "^\\_<" (symbol-name sym)))
             (unless (looking-at "\\_<")
               (thing-at-point--beginning-of-sexp))
             (kill-sexp)
             (setf form-cycle-current-name (substring-no-properties
                                            (current-kill 0))
                   form-cycle-initial-position (point)))

            ((and (not sym)
                  (looking-at "(\\|\"\\|'"))
             (kill-sexp)
             (setf form-cycle-current-name (substring-no-properties
                                            (current-kill 0))
                   form-cycle-initial-position (point)))
            (t (setf form-cycle-current-name ""))))

    (when form-cycle-up-list-initially-p
      (up-list -1)
      (kill-sexp)
      (setf form-cycle-initial-position (point)))
    (when form-cycle-raise-list-initially-p
      (form-cycle-beginning-of-symbol-maybe)
      (raise-sexp)
      (setf form-cycle-initial-position (point))))

  (let* ((place-point)
         (form-string-designator (form-cycle-fc-form-string form))
         (form-string (if (or (functionp form-string-designator)
                              (symbolp form-string-designator))
                          (funcall form-string-designator)
                        form-string-designator))
         (getopt (lambda (opt)
                   (cl-second (form-cycle-fc-form-assoc form opt))))
         (getargs (lambda (opt)
                    (nthcdr 2 (form-cycle-fc-form-assoc form opt))))
         (string))

    (when (form-cycle-fc-form-assoc form 'map-form)
      (save-excursion
        (setf form-string
              ;; (funcall (funcall getopt 'map-form) form-string)
              (apply (funcall getopt 'map-form)
                     form-string
                     (funcall getargs 'map-form)))))

    ;; Build string
    (with-temp-buffer
      (insert form-string)
      (goto-char (point-min))
      ;; _ -> name
      (if (equal form-cycle-current-name "")
          ;; If no name was given, ignore @ and place point at _
          ;; instead.
          (let ()
            ;; _ -> @
            (save-excursion (replace-string form-cycle-name-marker
                                            form-cycle-point-marker
                                            nil 1 (buffer-end 1)))
            ;; Find first @
            (search-forward form-cycle-point-marker nil t)
            ;; Delete the other @'s
            (save-excursion (replace-string form-cycle-point-marker "")))
        (replace-string form-cycle-name-marker form-cycle-current-name))
      (goto-char (point-min))
      ;; Figure out where to place point
      (when (search-forward form-cycle-point-marker nil t)
        (delete-char (- (length form-cycle-point-marker)))
        (setf place-point (1- (point))))
      (setf string (buffer-substring-no-properties 1 (buffer-end 1))))
    (form-cycle-debug string form-cycle-with-name)
    (when (funcall getopt 'map-string)
      (save-excursion
        (setf string (apply (funcall getopt 'map-string)
                            string
                            (funcall getargs 'map-string)))))

    (form-cycle-default-cycle-function string)

    (when place-point
      (goto-char form-cycle-initial-position)
      (forward-char place-point)
      (when (funcall getopt 'after-place-point)
        (save-excursion
          (funcall (funcall getopt 'after-place-point)))))
    (when (funcall getopt 'place-point)
      (let ((p (funcall getopt 'place-point)))
        (typecase p
                  (integer (goto-char (+ form-cycle-initial-position p)))
                  (function (funcall p)))))
    (when (funcall getopt 'after-cycle)
      (let ((end (save-excursion
                   (let ((len (length string)))
                     (goto-char (+ form-cycle-initial-position len))
                     (push-mark nil t)
                     (point)))))
        (save-excursion
          (funcall (funcall getopt 'after-cycle)))
        (unless (= (mark) end)
          (form-cycle-debug (mark))
          (form-cycle-debug end)
          (setf form-cycle-undo-previous-function
                (form-cycle-make-undo-function
                 (- (mark) form-cycle-initial-position))))
        (pop-mark)))
    string))

(defun form-cycle-test-with-name ()
  (interactive)
  (let ((form-cycle-function 'form-cycle-with-name))
    (form-cycle-initiate '("(defun _ () @)"
                           "(defclass _ () (@))"))))

;; Context aware lisp forms

(defvar form-cycle-lisp-patterns nil)

(defun form-cycle-%%%-to-subseq-of-current-name (form length)
  (if (< (length form-cycle-current-name) length)
      form
    (let ((prefix (cl-subseq form-cycle-current-name 0 length)))
      (replace-regexp-in-string "%%%" prefix form))))

(defun form-cycle-%%%-to-first-char-of-current-name (form)
  (form-cycle-%%%-to-subseq-of-current-name form 1))

(defun form-cycle-%%%-to-first-two-chars-of-current-name (form)
  (form-cycle-%%%-to-subseq-of-current-name form 2))

(defun form-cycle-%%%-to-toplevel-name (form)
  (replace-regexp-in-string "%%%"
                            (form-cycle-toplevel-form-name)
                            form))

(defun form-cycle-require-position (form fn n)
  (let ((count 0))
    (save-excursion
      (ignore-errors
        (cl-loop (print count)
                 (backward-sexp)
                 (cl-incf count))))
    (unless (funcall fn count n)
      (form-cycle-skip))
    form))

(defun form-cycle-indent-defun ()
  (beginning-of-defun)
  (indent-pp-sexp))

(defun form-cycle-toplevel-form-nth (n)
  (condition-case nil
      (save-excursion
        (beginning-of-defun)
        (down-list)
        (forward-sexp 1)
        (forward-sexp n)
        (symbol-name (symbol-at-point)))
    (error nil)))

(defun form-cycle-read-nth-toplevel-form (n)
  (condition-case nil
      (save-excursion
        (beginning-of-defun)
        (down-list)
        (forward-sexp 1)
        (forward-sexp n)
        (backward-sexp)
        (read (current-buffer)))
    (error nil)))

(defun form-cycle-toplevel-form-name ()
  (condition-case nil
      (save-excursion
        (beginning-of-defun)
        (down-list)
        (forward-sexp 2)
        (symbol-name (symbol-at-point)))
    (error nil)))

(defun form-cycle-surrounding-sexp-car ()
  (save-excursion
    (ignore-errors
      (up-list -1)
      (when (looking-at "( *\\_<")      ; list with symbol at car
        (search-forward-regexp "\\_<")
        (symbol-at-point)))))

(defun form-cycle-at-toplevel-p ()
  (save-excursion
    (let ((top t))
      (ignore-errors (up-list -1)
                     (setf top nil))
      top)))

(defun form-cycle-gather-context (&optional include-symbol-at-point)
  (let* ((current-symbol (symbol-at-point))
         (current-symbol-name (when current-symbol
                                (substring-no-properties
                                 (symbol-name current-symbol))))
         (context
          (save-excursion
            (cl-loop until (form-cycle-at-toplevel-p)
                     for car = (form-cycle-surrounding-sexp-car)
                     collect car
                     do (up-list -1)))))
    (if (and include-symbol-at-point
             current-symbol-name)
        (nreverse (cons current-symbol-name context))
      (nreverse context))))

(defun form-cycle-match-context-pattern (pattern current-context
                                                 &optional
                                                 allowed-range
                                                 max-depth
                                                 current-symbol-name)
  (cl-block done
    (when (and max-depth (< max-depth (length current-context)))
      (cl-return-from done nil))

    (let ((last (car (last pattern))))
      (when (stringp last)             ; Match against current symbol.
        (if (equal last current-symbol-name)
            (setf pattern (butlast pattern)) ; ok
          (cl-return-from done nil))))

    (when (and (null pattern)
               (null current-context))
      (cl-return-from done t))

    (unless (listp pattern)
      (error "Bad pattern."))

    (form-cycle-debug allowed-range)

    (cl-loop with pattern-rest = (reverse pattern)
             for range from 0
             for pattern-head = (car pattern-rest)
             for part in (reverse current-context)
             when (and (or (eq part pattern-head)
                           (equal '(*) pattern-head))
                       (or (null allowed-range)
                           (<= range allowed-range)))
             do (setf pattern-rest (cl-rest pattern-rest))
             when (null pattern-rest)
             return t
             finally return nil))) ; if complete pattern was not matched

(defun form-cycle-determine-matching-fcs (known-fcs)
  (save-excursion
    (cl-block done
      (let* ((current-context (form-cycle-gather-context))
             (current-symbol (symbol-at-point))
             (current-symbol-name
              (when current-symbol
                (symbol-name current-symbol))))
        (cl-loop for c in known-fcs
                 for pat = (form-cycle-fc-pattern c)
                 for range = (form-cycle-fc-match-range c)
                 for max-depth = (form-cycle-fc-max-depth c)
                 when (form-cycle-match-context-pattern
                       pat current-context range max-depth current-symbol-name)
                 collect c)))))

(defvar form-cycle-process-includes-list 'nothing)

(defun form-cycle-process-includes (fc known-fcs)
  (let ((form-cycle-process-includes-list nil))
    (form-cycle-process-includes-2 fc known-fcs)))

(defun form-cycle-process-includes-2 (fc known-fcs)
  (let (complete-forms
        patterns-included-p)
    (cl-loop for form in (form-cycle-fc-forms fc)
             if (and (consp form) ; (include foo)
                     (eq (cl-first form) 'include))
             do (let ((pat (cl-rest form)))
                  (unless (cl-find pat form-cycle-process-includes-list
                                   :test #'equal)
                    (cl-loop for form2 in (form-cycle-fc-forms
                                           (form-cycle-find-fc pat))
                             do (push form2 complete-forms))
                    (push pat form-cycle-process-includes-list)
                    (setf patterns-included-p t)))
             else do (push form complete-forms))

    (setf complete-forms (nreverse complete-forms))

    ;; Recurse if an include directive was found.
    (if patterns-included-p
        (form-cycle-process-includes-2
         (form-cycle-make-fc
          (form-cycle-fc-pattern fc)
          complete-forms
          (form-cycle-fc-pattern-options fc))
         known-fcs)
      (form-cycle-make-fc
       (form-cycle-fc-pattern fc)
       complete-forms
       (form-cycle-fc-pattern-options fc)))))

(defun form-cycle-lisp-patterns (&optional lisp-forms initiate-fn)
  (interactive)
  (when (null lisp-forms)
    (setf lisp-forms form-cycle-lisp-patterns))
  (let* ((form-cycle-function 'form-cycle-with-name)
         (matching-fcs (form-cycle-determine-matching-fcs
                        lisp-forms))
         (fc (form-cycle-process-includes
              (cl-first matching-fcs)
              lisp-forms))
         (form-cycle-up-list-initially-p)
         (form-cycle-up-list-initially-sexp-string)
         (form-cycle-raise-list-initially-p))

    (when (< 1 (length matching-fcs))
      (message "Matching patterns: %s"
               (mapcar #'form-cycle-fc-pattern
                       matching-fcs)))

    (form-cycle-debug (mapcar
                       (lambda (x) (form-cycle-fc-pattern x))
                       matching-fcs))
    (form-cycle-debug (form-cycle-gather-context))

    (cl-loop for opt in (form-cycle-fc-pattern-options fc)
             do
             (cl-case opt
               (up-list (setf form-cycle-up-list-initially-p t))))
    (if initiate-fn
        (funcall initiate-fn fc)
      (form-cycle-initiate
       (form-cycle-fc-forms fc)))))

(defun form-cycle-lisp-patterns-ido (&optional lisp-forms)
  (interactive)
  (form-cycle-lisp-patterns
   lisp-forms
   (lambda (fc)
     (let* ((mangled-original-pairs)
            (choice (ido-completing-read
                     ""
                     (mapcar (lambda (string)
                               (let (mangled)
                                 (setf mangled (replace-regexp-in-string "\n" " " string)
                                       mangled (replace-regexp-in-string "  +" " " mangled))
                                 (push (cons mangled string) mangled-original-pairs)
                                 mangled))
                             (mapcar 'form-cycle-fc-form-string
                                     (form-cycle-fc-forms fc)))))
            (form (cl-find (cdr (cl-find choice mangled-original-pairs
                                         :test #'equal
                                         :key #'car))
                           (form-cycle-fc-forms fc)
                           :test #'equal
                           :key #'form-cycle-fc-form-string)))
       (form-cycle-initiate (list form))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun form-cycle-check-plist (thing)
  (cl-check-type thing list)
  (cl-loop for (a b) on thing by #'cddr
           do (cl-check-type a symbol)))

(defun form-cycle-check-alist (thing)
  (cl-check-type thing list)
  (cl-loop for i in thing
           do (unless (symbolp i)
                (cl-check-type i cons))))

(defun form-cycle-check-option (thing)
  (if (consp thing)
      (cl-check-type (car thing) symbol)
    (cl-check-type thing symbol)))

;; Accessors

(defun form-cycle-make-fc (pattern forms &optional options)
  (cl-check-type pattern (or symbol list string))
  (unless (listp pattern)
    (setf pattern (list pattern)))
  (cl-check-type forms list)
  (dolist (opt options)
    (form-cycle-check-option opt))
  (dolist (f forms)
    (if (listp f)
        (unless (eq (car f) 'include)
          (cl-check-type (car f) string)
          (form-cycle-check-alist (cl-rest f)))
      (cl-check-type f string)))
  (list (cl-list* 'pattern pattern)
        (cl-list* 'options options)
        (cl-list* 'forms forms)))

(defun form-cycle-fc-pattern (fc)
  (cdr (assq 'pattern fc)))

(defun form-cycle-fc-pattern-options (fc)
  (cdr (assq 'options fc)))

;; Like assoc but a symbol is considered equal to (foo t)
(defun form-cycle-fc-assoc (fc-alist key)
  (cl-loop for i in fc-alist
           do (cond ((and (consp i)
                          (equal (car i) key))
                     (cl-return i))
                    ((equal key i)
                     (cl-return (list i t))))))

(defun form-cycle-fc-pattern-assoc (fc key)
  (form-cycle-fc-assoc
   (form-cycle-fc-pattern-options fc)
   key))

(defun form-cycle-fc-forms (fc)
  (cdr (assq 'forms fc)))

(defun form-cycle-fc-match-range (fc)
  (let ((r (form-cycle-fc-pattern-assoc fc 'range)))
    (if r
        (cl-second r)
      (if (form-cycle-fc-pattern-assoc fc 'immediate)
          0
        nil))))

(defun form-cycle-fc-max-depth (fc)
  (let ((r (form-cycle-fc-pattern-assoc fc 'max-depth)))
    (if r
        (cl-second r)
      (if (form-cycle-fc-pattern-assoc fc 'toplevel)
          0
        nil))))

(defun form-cycle-fc-form-options (form)
  (if (listp form)
      (cl-rest form)
    nil))

(defun form-cycle-fc-form-string (form)
  (if (listp form)
      (cl-first form)
    form))

(defun form-cycle-fc-form-assoc (form key)
  (form-cycle-fc-assoc
   (form-cycle-fc-form-options form)
   key))

(defun form-cycle-read-fc-from-buffer ()
  (let (forms)
    (cl-loop for form = (condition-case nil
                            (read (current-buffer))
                          (end-of-file (cl-return)))
             do (push form forms))
    (setf forms (nreverse forms))
    (form-cycle-make-fc
     (nth 0 forms)
     (nthcdr 2 forms)
     (nth 1 forms))))

(defmacro form-cycle-with-fc (bindings fc &rest rest)
  "(form-cycle-with-fc (pattern options forms) FC &rest REST)"
  (cl-destructuring-bind (pattern options forms) bindings
    (let ((c (cl-gensym)))
      `(let* ((,c ,fc)
              (,pattern (form-cycle-fc-pattern ,c))
              (,options (form-cycle-fc-pattern-options ,c))
              (,forms (form-cycle-fc-forms ,c)))
         ,@rest))))

(defmacro form-cycle-with-form (bindings form &rest rest)
  (cl-destructuring-bind (string options) bindings
    (let ((f (cl-gensym)))
      `(let* ((,f ,form)
              (,string (form-cycle-fc-form-string ,f))
              (,options (form-cycle-fc-form-options ,f)))
         ,@rest))))

(put 'form-cycle-with-fc 'lisp-indent-function 2)
(put 'form-cycle-with-form 'lisp-indent-function 2)


;; Manipulation

(defmacro form-cycle-define-pattern (pattern options &rest forms)
  `(form-cycle-add-fc ',pattern ',forms ',options))

(put 'form-cycle-define-pattern 'lisp-indent-function 2)

(defun form-cycle-add-fc (pattern forms &optional options)
  (unless (listp pattern)
    (setf pattern (list pattern)))
  (if (form-cycle-find-fc pattern)
      (form-cycle-replace-fc pattern forms options)
    (cl-pushnew (form-cycle-make-fc pattern forms options)
                form-cycle-lisp-patterns
                :test #'equal
                :key #'form-cycle-fc-pattern)))

(defun form-cycle-add-fc-semi-interactively (pattern forms &optional options)
  (if (form-cycle-add-fc pattern forms options)
      (progn
        (message "Added.")
        t)
    (progn
      (message "Error adding.")
      nil)))

;; Adds a form to the fc.
(defun form-cycle-add (b e)
  (interactive "r")
  (let* ((context (form-cycle-gather-context))
         (pattern (read-from-minibuffer "Pattern: "
                                        (with-output-to-string
                                          (prin1 context))
                                        nil
                                        t))
         (exist (form-cycle-find-fc pattern)))
    (if exist
        (progn
          (form-cycle-replace-fc
           pattern
           (append (list (buffer-substring-no-properties b e))
                   (form-cycle-fc-forms exist))
           (form-cycle-fc-pattern-options exist))
          (message "Added."))
      (form-cycle-add-fc-semi-interactively
       pattern
       (list (buffer-substring-no-properties
              b e))))))

(defun form-cycle-edit ()
  (interactive)
  (let* ((context (form-cycle-gather-context t))
         (pattern (read-from-minibuffer "Pattern: "
                                        (with-output-to-string
                                          (prin1 context))
                                        nil
                                        t))
         (exist (form-cycle-find-fc pattern)))
    (form-cycle-open-fc-edit-buffer
     (or exist (form-cycle-make-fc pattern nil nil)))))

(define-minor-mode form-cycle-edit-mode "" nil " Form-Cycle-Edit"
  '(("\C-c\C-c" . form-cycle-edit-save)
    ("\C-c\C-s" . form-cycle-save))
  (when form-cycle-edit-mode
    t))

(defun form-cycle-edit-save ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let* ((fc (form-cycle-read-fc-from-buffer))
           (exist (form-cycle-find-fc
                   (form-cycle-fc-pattern fc))))
      (cond ((and exist
                  (null (form-cycle-fc-forms fc)))
             (form-cycle-delete-fc-semi-interactively
              (form-cycle-fc-pattern fc)))

            (exist
             (form-cycle-replace-fc-semi-interactively
              (form-cycle-fc-pattern fc)
              (form-cycle-fc-forms fc)
              (form-cycle-fc-pattern-options fc)))

            ((not exist)
             (form-cycle-add-fc-semi-interactively
              (form-cycle-fc-pattern fc)
              (form-cycle-fc-forms fc)
              (form-cycle-fc-pattern-options fc)))))))

(defun form-cycle-open-fc-edit-buffer (fc)
  (let ((buf (get-buffer-create "*Form cycle edit*")))
    (switch-to-buffer-other-window buf)
    (emacs-lisp-mode)
    (form-cycle-edit-mode 1)
    (delete-region 1 (buffer-end 1))
    (insert ";; Press C-c C-c to update this form cycle.
;; Press C-c C-s to write all patterns to a file.
;; Valid pattern options: up-list toplevel (depth N) immediate (range N).
;; Valid form options: map-form map-string after-cycle
;; Form may also be (include . PATTERN)
;; If no forms are given, then the form cycle is deleted.
\n")
    (insert ";; Pattern\n\n")
    (insert (prin1-to-string
             (form-cycle-fc-pattern fc)))
    (insert "\n\n;; Options\n\n")
    (insert (prin1-to-string
             (form-cycle-fc-pattern-options fc)))
    (insert "\n\n;; Forms\n\n")
    (cl-loop for form in (form-cycle-fc-forms fc)
             do
             (insert (prin1-to-string form))
             (newline)
             (newline))))

(defun form-cycle-move-to-front ()
  (interactive)
  (let* ((context (form-cycle-gather-context t))
         (pattern (read-from-minibuffer "Pattern: "
                                        (with-output-to-string
                                          (prin1 context))
                                        nil
                                        t))
         (exist (form-cycle-find-fc pattern)))
    (if exist
        (form-cycle-delete-fc pattern)
      (error "No such form cycle."))
    (form-cycle-add-fc
     (form-cycle-fc-pattern exist)
     (form-cycle-fc-forms exist)
     (form-cycle-fc-pattern-options exist))
    (message "Moved to front.")))

(defun form-cycle-find-fc (pattern)
  (unless (listp pattern)
    (setf pattern (list pattern)))
  (cl-find pattern
           form-cycle-lisp-patterns
           :key #'form-cycle-fc-pattern
           :test #'equal))

(defun form-cycle-fc-position (pattern)
  (unless (listp pattern)
    (setf pattern (list pattern)))
  (cl-position pattern form-cycle-lisp-patterns
               :key #'form-cycle-fc-pattern
               :test #'equal))

(defun form-cycle-find-fc-semi-interactively (pattern)
  (message "%s" (prin1-to-string (form-cycle-find-fc pattern))))

(defun form-cycle-delete-fc (pattern)
  (unless (listp pattern)
    (setf pattern (list pattern)))
  (if (form-cycle-find-fc pattern)
      (progn
        (setf form-cycle-lisp-patterns
              (cl-delete pattern form-cycle-lisp-patterns
                         :key #'form-cycle-fc-pattern
                         :test #'equal))
        t)
    nil))

(defun form-cycle-delete-fc-semi-interactively (pattern)
  (if (form-cycle-delete-fc pattern)
      (progn (message "Deleted.")
             t)
    (progn (message "Error deleting.")
           nil)))

(defun form-cycle-delete ()
  (interactive)
  (let* ((context (form-cycle-gather-context t))
         (pattern (read-from-minibuffer "Context: "
                                        (with-output-to-string
                                          (prin1 context))
                                        nil
                                        t)))
    (form-cycle-delete-fc-semi-interactively pattern)))

(defun form-cycle-replace-fc (pattern forms &optional options)
  (let ((pos (form-cycle-fc-position pattern)))
    (if pos
        (setf (nth pos form-cycle-lisp-patterns)
              (form-cycle-make-fc pattern forms options))
      nil)))

(defun form-cycle-replace-fc-semi-interactively (pattern forms &optional options)
  (if (form-cycle-replace-fc pattern forms options)
      (message "Replaced.")
    (error "Error replacing.")))

(defun form-cycle-save ()
  (interactive)
  (let ((file (expand-file-name
               (read-file-name "Save to: "
                               nil nil nil
                               (if form-cycle-pattern-file
                                   form-cycle-pattern-file
                                   "form-cycle-patterns.el")))))
    (with-temp-buffer
      (dolist (fc (reverse form-cycle-lisp-patterns))
        (newline)
        (form-cycle-with-fc (pat opt forms) fc
                            (insert
                             (pp-to-string
                              `(form-cycle-define-pattern ,pat ,opt ,@forms)))))
      (write-file file nil))))

(defun form-cycle-load ()
  (interactive)
  (let ((file (expand-file-name
               (read-file-name "Load patterns from: "
                               nil nil nil "form-cycle-patterns.el"))))
    (load-file file)))

(defun form-cycle-clear ()
  (interactive)
  (setf form-cycle-lisp-patterns nil)
  (message "Cleared patterns."))

(provide 'form-cycle)
