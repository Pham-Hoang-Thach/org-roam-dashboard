;; Define a reusable button type once
(define-button-type 'org-roam-node-button
  'follow-link t
  'help-echo "mouse-1, RET: Open this Org-roam note"
  'action (lambda (btn)
            (org-roam-node-open (button-get btn 'node))))

(defun org-roam-dashboard ()
  "Dashboard of Org-roam notes sorted by filename.
Shows Title, Type, Tags, and Backlinks. Clicking Title opens the Org-roam node."
  (interactive)
  (let* ((all-nodes (org-roam-node-list))
         (all-tags (delete-dups (apply #'append (mapcar #'org-roam-node-tags all-nodes))))
         (all-types (delete-dups (mapcar #'org-roam-node-type all-nodes)))
         ;; allow multiple selections
         (tags (completing-read-multiple "Filter by tags (comma separated, RET for all): "
                                         all-tags nil t))
         (types (completing-read-multiple "Filter by types (comma separated, RET for all): "
                                          all-types nil t))

         ;; filter nodes by tags
         (nodes (if (null tags)
                    all-nodes
                  (seq-filter (lambda (n)
                                (seq-intersection tags (org-roam-node-tags n)))
                              all-nodes)))
         ;; filter nodes by types
         (nodes (if (null types)
                    nodes
                  (seq-filter (lambda (n)
                                (member (org-roam-node-type n) types))
                              nodes)))

         ;; sort alphabetically by filename
         (sorted-nodes
          (seq-sort-by
           (lambda (node)
             (file-name-nondirectory (org-roam-node-file node)))
           #'string<
           nodes)))
    (with-current-buffer (get-buffer-create "*Org-roam Dashboard*")
      (erase-buffer)
      ;; header row
      (insert (format "%-60s %-15s %-40s %-10s\n" "Title" "Type" "Tags" "Backlinks"))
      (insert (make-string 130 ?-) "\n")
      ;; rows
      (dolist (node sorted-nodes)
        (let* ((title (org-roam-node-title node))
               (title (if (> (length title) 60)
                          (concat (substring title 0 57) "…")
                        title))
               (tags-str (string-join (org-roam-node-tags node) ", "))
               (type-str (or (org-roam-node-type node) ""))
               (backlinks (length (org-roam-backlinks-get node))))
          ;; Use the predefined button type
          (insert-text-button
           (format "%-60s %-15s %-40s %-10d\n" title type-str tags-str backlinks)
           'type 'org-roam-node-button
           'node node)))
      (goto-char (point-min))
      (org-mode)
      (display-buffer (current-buffer)))))
