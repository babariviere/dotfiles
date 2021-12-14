(defun amber/scheme-indent-function (indent-point state)
  "Scheme mode function for the value of the variable `lisp-indent-function'.
This behaves like the function `lisp-indent-function', except that:

i) it checks for a non-nil value of the property `scheme-indent-function'
\(or the deprecated `scheme-indent-hook'), rather than `lisp-indent-function'.

ii) if that property specifies a function, it is called with three
arguments (not two), the third argument being the default (i.e., current)
indentation."
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (not (looking-at "\\sw\\|\\s_")))
        ;; car of form doesn't seem to be a symbol
        (progn
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
              (progn (goto-char calculate-lisp-indent-last-sexp)
                     (beginning-of-line)
                     (parse-partial-sexp (point)
										 calculate-lisp-indent-last-sexp 0 t)))
          ;; Indent under the list or under the first sexp on the same
          ;; line as calculate-lisp-indent-last-sexp.  Note that first
          ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (current-column))
      (let ((function (buffer-substring (point)
										(progn (forward-sexp 1) (point))))
			method)
		(setq method (or (get (intern-soft function) 'scheme-indent-function)
						 (get (intern-soft function) 'scheme-indent-hook)))
		(cond ((or (eq method 'defun)
				   (and (null method)
						(> (length function) 3)
						(string-match "\\`def" function)))
			   (lisp-indent-defform state indent-point))
              ;; This next cond clause is the only change -mhw
			  ((and (null method)
                    (> (length function) 1)
										; The '#' in '#:' seems to get lost, not sure why
                    (string-match "\\`:" function))
               (let ((lisp-body-indent 1))
                 (lisp-indent-defform state indent-point)))
			  ((integerp method)
			   (lisp-indent-specform method state
									 indent-point normal-indent))
			  (method
			   (funcall method state indent-point normal-indent)))))))

(defun amber/scheme-set-indent ()
  (setq-local lisp-indent-function #'amber/scheme-indent-function))

(add-hook 'scheme-mode-hook #'amber/scheme-set-indent)

(provide 'amber-scheme)
