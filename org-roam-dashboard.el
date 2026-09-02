(defun org-roam-dashboard ()
  "Dashboard of Org-roam notes sorted by filename timestamp.
Shows Title (fixed width, truncated) and Backlinks. Clicking opens the Org-roam node."
  (interactive)
  (let* ((all-nodes (org-roam-node-list))
         (all-tags (delete-dups (apply #'append (mapcar #'org-roam-node-tags all-nodes))))
         (all-types (delete-dups (mapcar #'org-roam-node-type all-nodes)))
         (tag (completing-read "Filter by tag (RET for all): " all-tags nil t "" nil ""))
         (type (completing-read "Filter by type (RET for all): " all-types nil t "" nil ""))

         (nodes (if (string-empty-p tag)
                    all-nodes
                  (seq-filter (lambda (n) (member tag (org-roam-node-tags n))) all-nodes)))
         (nodes (if (string-empty-p type)
                    nodes
                  (seq-filter (lambda (n) (equal type (org-roam-node-type n))) nodes)))

         (sorted-nodes
          (seq-sort-by
           (lambda (node)
             (let* ((fname (file-name-nondirectory (org-roam-node-file node)))
                    (ts (car (split-string fname "-"))))
               (if (and ts (string-match-p "^[0-9]+$" ts))
                   ts
                 "")))
           #'string>
           nodes)))
    (with-current-buffer (get-buffer-create "*Org-roam Dashboard*")
      (erase-buffer)
      ;; header row
      (insert (format "%-100s %-10s\n" "Title" "Backlinks"))
      (insert (make-string 112 ?-) "\n")
      ;; rows
      (dolist (node sorted-nodes)
        (let* ((title (org-roam-node-title node))
               ;; truncate title to 100 chars max
               (title (if (> (length title) 100)
                          (concat (substring title 0 97) "…")
                        title))
               (backlinks (length (org-roam-backlinks-get node))))
          (insert-text-button
           (format "%-100s %-10d\n" title backlinks)
           'node node
           'action (lambda (btn)
                     (org-roam-node-open (button-get btn 'node)))
           'follow-link t)))
      (goto-char (point-min))
      (org-mode)
      (display-buffer (current-buffer)))))
