
(defmacro walk-head (&rest body)
  (` (when (looking-at "^diff")
       (progn
         (,@ body)
         (forward-line) ;; skip over "diff" line
         (while (and (not (eobp)) (not (looking-at "^diff\\|^@@")))
           (,@ body)
           (forward-line))
         (if (not (eobp))
             (goto-char (match-beginning 0))
           (goto-char (point-max)))))))

(defmacro walk-hunk (&rest body)
  (` (when (looking-at "^@@\s-\\([0-9]+\\)\,[0-9]+\s\\+\\([0-9]+\\)\,[0-9]+\s@@.*")
       (cond (,@ body))
       (forward-line)
       (while (and (not (looking-at "^@@.*")) (not (eobp)))
         (cond (,@ body))
         (forward-line))
       (looking-at "^@@.*"))))

(defun git-overlay ()
  (interactive)
  (save-excursion
    (let* ((path (basename (buffer-file-name)))
           (file (filename (buffer-file-name)))
           (buffer (current-buffer))
           (lines-skip '((0 0))))
      (with-current-buffer buffer
        (remove-overlays))
      (with-directory path
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

(provide 'git-overlay)