
(require 'cl)

(defun git-overlay-basedir (path)
  (apply #'concat (reverse (mapcar (lambda (s)
                                     (concat s "/"))
                                   (cdr (reverse (split-string path "/")))))))

(defun git-overlay-filename (path)
  (car (reverse (split-string path "/"))))

(defmacro git-overlay-with-directory (path &rest body)
  `(let ((currentdir default-directory))
     (cd ,path)
     ,@body
     (cd currentdir)))

(defmacro walk-head (&rest body)
  `(when (looking-at "^diff")
       (progn
         ,@body
         (forward-line) ;; skip over "diff" line
         (while (and (not (eobp)) (not (looking-at "^diff\\|^@@")))
           ,@body
           (forward-line))
         (if (not (eobp))
             (goto-char (match-beginning 0))
           (goto-char (point-max))))))

(defmacro walk-hunk (&rest body)
  `(when (looking-at "^@@\s-\\([0-9]+\\)\,[0-9]+\s\\+\\([0-9]+\\)\,[0-9]+\s@@.*")
       (cond ,@body)
       (forward-line)
       (while (and (not (looking-at "^@@.*")) (not (eobp)))
         (cond ,@body)
         (forward-line))
       (looking-at "^@@.*")))

(defun git-overlay ()
  (interactive)
  (save-excursion
    (let* ((path (git-overlay-basedir (buffer-file-name)))
           (file (git-overlay-filename (buffer-file-name)))
           (buffer (current-buffer))
           (lines-skip '((0 0))))
      (with-current-buffer buffer
        (remove-overlays))
      (git-overlay-with-directory path
                      (progn
                        (with-temp-buffer
                          (call-process "git" nil t t "--no-pager" "diff" file)
                          (goto-char (point-min))
                          (walk-head
                           nil)
                          (while (let* ((hunk-line nil)
                                        (hunk-old-line nil))
                                   (walk-hunk ((looking-at "^@@\s-\\([0-9]+\\)\,\\([0-9]+\\)\s\\+\\([0-9]+\\)\,\\([0-9]+\\)\s@@.*")
                                               (progn
                                                 (message (substring (thing-at-point 'line) 0 -1))
                                                 (setq hunk-line (string-to-number (match-string 3))
                                                       hunk-old-line (string-to-number (match-string 1))
                                                       skip (string-to-number (match-string 4))
                                                       old-skip (string-to-number (match-string 2))
                                                       lines-skip (cons (list hunk-old-line (+ (cadar lines-skip) (- skip old-skip))) lines-skip))
                                                 (print lines-skip)))
                                              ((looking-at "^\\+.*")
                                               (with-current-buffer buffer
                                                 (goto-line hunk-line)
                                                 (overlay-put (make-overlay (line-beginning-position)
                                                                            (save-excursion
                                                                              (progn
                                                                                (forward-line 1)
                                                                                (point)))) 'face '((:background "#354f35")))
                                                 (setq hunk-line (+ hunk-line 1))))
                                              ((looking-at "^-.*")
                                               nil)
                                              (t
                                               (with-current-buffer buffer
                                                 (goto-line hunk-line)
                                                 (overlay-put (make-overlay (line-beginning-position)
                                                                            (save-excursion
                                                                              (progn
                                                                                (forward-line 1)
                                                                                (point)))) 'face '((:background "#36364f")))
                                                 (setq hunk-line (+ hunk-line 1))))))))
                        ;;(print "DONE THIS!!!!!!!!!!!!!!!!!!!!!!!!")
                        (with-temp-buffer
                          (call-process "git" nil t t "--no-pager" "diff" "--cached" file)
                          (goto-char (point-min))
                          (walk-head
                           nil)
                          (while (let* ((hunk-line nil)
                                        (hunk-old-line nil))
                                   (walk-hunk ((looking-at "^@@\s-\\([0-9]+\\)\,\\([0-9]+\\)\s\\+\\([0-9]+\\)\,\\([0-9]+\\)\s@@.*")
                                               (progn
                                                 (message (substring (thing-at-point 'line) 0 -1))
                                                 (print lines-skip)
                                                 (setq hunk-old-line (string-to-number (match-string 3))
                                                       hunk-line (let ((cs '()))
                                                                   (loop for l in lines-skip
                                                                         collect l into cs
                                                                         until (< (car l) hunk-old-line)
                                                                         finally return (+ hunk-old-line (cadar (reverse cs))))))
                                                 ))
                                              ((looking-at "^\\+.*")
                                               (with-current-buffer buffer
                                                 (goto-line hunk-line)
                                                 (overlay-put (make-overlay (line-beginning-position)
                                                                            (save-excursion
                                                                              (progn
                                                                                (forward-line 1)
                                                                                (point)))) 'face '((:background "#4f3535")))
                                                 (setq hunk-line (+ hunk-line 1))))
                                              ((looking-at "^-.*")
                                               nil)
                                              (t
                                               (with-current-buffer buffer
                                                 (print hunk-line)

                                                 (goto-line hunk-line)

                                                 (overlay-put (make-overlay (line-beginning-position)
                                                                            (save-excursion
                                                                              (progn
                                                                                (forward-line 1)
                                                                                (point)))) 'face '((:background "#4f354f")))
                                                 (setq hunk-line (+ hunk-line 1)))))))))))))

(defun git-get-line-commit-info ()
  (with-temp-buffer
    (let ((saved-directory default-directory))
      (cd "/home/lazor/org")
      (= (apply 'call-process "git" nil (current-buffer) nil (list "blame" "--porcelain" "--" "projects.org")) 0)
      (goto-char (point-min))
      (when (re-search-forward "org-mode Integration" nil t)
        (when (re-search-backward "^\\([0-9a-f]\\{40\\}\\) \\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\)$" nil t)
          (let ((p (point-at-bol))
                (s (match-string-no-properties 1))
                (beg nil)
                (end nil))
            (while (and s (re-search-backward s nil t))
              (setq p (point-at-bol)))
            (end-of-line)
            (setq beg p)
            (setq end (if (re-search-forward "^\\([0-9a-f]\\{40\\}\\) \\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\)$" nil t)
                          (point-at-bol)
                        (point-max)))
            (print (parse-commit-info (buffer-substring-no-properties beg end)))
            )))
      (cd saved-directory))))

(defun parse-commit-info (str)
  (let ((r '()))
    (dolist (line (split-string str "\n" t) r)
      (cond ((string-match "author \\(.*\\)" line)
             (add-to-list 'r `("author" . ,(match-string 1 line))))
            ((string-match "author-mail \\(.*\\)" line)
             (add-to-list 'r `("author-mail" . ,(match-string 1 line))))
            ((string-match "commiter \\(.*\\)" line)
             (add-to-list 'r `("commiter" . ,(match-string 1 line))))
            ((string-match "commiter-mail \\(.*\\)" line)
             (add-to-list 'r `("commiter-mail" . ,(match-string 1 line))))
            ))))

(provide 'git-overlay)