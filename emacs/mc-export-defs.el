
(setq-default org-html-postamble nil)
(setq-default org-export-with-section-numbers 1)


(setq-default org-publish-project-alist
      '(

        ("org-roam-notes"
        :base-directory "~/org/roam"
        :auto-sitemap t
        :auto-index t
        :sitemap-filename "index.org"
        :sitemap-title "Notes"
        :sitemap-format-entry mc/roam-sitemap-entry-format
        :sitemap-function mc/roam-sitemap-function
        :base-extension "org"
        :publishing-directory "~/org/roam-html"
        :recursive t
        :publishing-function org-html-publish-to-html
        :headline-levels 4
        :auto-preamble t
        :html-head-extra "<style>@media (prefers-color-scheme: dark) { body { background: black; color: #ddd } }</style>")

        ("org-blog"
        :base-directory "~/org/blog"
        :base-extension "org"
        :auto-sitemap t
        :auto-index t
        :sitemap-filename "index.org"
        :sitemap-title "Milos' pages"
        :sitemap-format-entry mc/org-sitemap-format-entry
        :sitemap-sort-files anti-chronologically
        :base-extension "org"
        :publishing-directory "~/org/blog-html"
        :recursive t
        :publishing-function org-html-publish-to-html
        :headline-levels 4
        :auto-preamble t
        :with-toc nil)

        ("org-blog-static"
        :base-directory "~/org/blog"
        :base-extension "css\\|js\\|png\\|jpg\\|gif"
        :publishing-directory "~/org/blog-html"
        :recursive t
        :publishing-function org-publish-attachment)

        ("org-blog-all" :components ("org-blog" "org-blog-static"))

        )
)

(defun mc/roam-sitemap-function (title files)
  "Sorts file by number of incoming links before calling default sitemap function."
  (let* ((sorted-by-backlinks (mapcar 'cdar (sort (cdr files) #'(lambda (a b) (> (caar a) (caar b))))))
         (lnks (cons (car files) sorted-by-backlinks)))
         (org-publish-sitemap-default title lnks)
    )

)

(defun mc/roam-sitemap-entry-format (entry style project)
  "Allows linking files by ID org property instead of filename and
adds number of backlinks to each entry"
  (cond
      ((not (directory-name-p entry))
         (let ((id (with-temp-buffer (insert-file-contents (org-publish--expand-file-name entry project)) (car (org-property-values "ID")))))
           (if id
               (let* ((backlinks (org-roam-backlinks-get (org-roam-node-from-id id)))
                      (len       (length backlinks))
                      (formatted (format "[[id:%s][%s (%s)]]" id (org-publish-find-title entry project) len)))
                 (list len formatted)
                ) ;; let backlinks
             (list 0 (format "[[file:%s][%s]]" entry (org-publish-find-title entry project)))
             ) ;; if id
           ) ;; let
         ) ;; file entry
       ((eq style 'tree)
         ;; Return only last subdir.
         (list 0 (file-name-nondirectory (directory-file-name entry)))) ;; tree
       (t (list 0 entry))) ;; cond
)


(defun mc/org-sitemap-format-entry (entry style project)
  (format "%s %s"
          (format-time-string
           "%Y-%m-%d\t"
           (org-publish-find-date entry project))
           (org-publish-sitemap-default-entry entry style project)))

;; https://org-roam.discourse.group/t/export-backlinks-on-org-export/1756/21
(defun collect-backlinks-string (backend)
  (when (org-roam-node-at-point)
    (let* ((source-node (org-roam-node-at-point))
           (source-file (org-roam-node-file source-node))
           (nodes-in-file (--filter (s-equals? (org-roam-node-file it) source-file)
                                    (org-roam-node-list)))
           (nodes-start-position (-map 'org-roam-node-point nodes-in-file))
           ;; Nodes don't store the last position, so get the next headline position
           ;; and subtract one character (or, if no next headline, get point-max)
           (nodes-end-position (-map (lambda (nodes-start-position)
                                       (goto-char nodes-start-position)
                                       (if (org-before-first-heading-p) ;; file node
                                           (point-max)
                                         (call-interactively
                                          'org-forward-heading-same-level)
                                         (if (> (point) nodes-start-position)
                                             (- (point) 1) ;; successfully found next
                                           (point-max)))) ;; there was no next
                                     nodes-start-position))
           ;; sort in order of decreasing end position
           (nodes-in-file-sorted (->> (-zip nodes-in-file nodes-end-position)
                                      (--sort (> (cdr it) (cdr other))))))

      (dolist (node-and-end nodes-in-file-sorted)
        (-let (((node . end-position) node-and-end))
          (when (org-roam-backlinks-get node)
            (goto-char end-position)
            ;; Add the references as a subtree of the node
            (setq heading (format "\n\n%s References\n"
                                  (s-repeat (+ (org-roam-node-level node) 1) "*")))
            (insert heading)
            (setq properties-drawer ":PROPERTIES:\n:HTML_CONTAINER_CLASS: references\n:END:\n")
            (insert properties-drawer)
            (dolist (backlink (org-roam-backlinks-get node))
              (let* ((source-node (org-roam-backlink-source-node backlink))
                     (properties (org-roam-backlink-properties backlink))
                     (point (org-roam-backlink-point backlink))
                     (text (s-replace "\n" " " (org-roam-preview-get-contents
                                                (org-roam-node-file source-node)
                                                point)))
                     (reference (format "- [[id:%s][%s]]\n"
                                        (org-roam-node-id source-node)
                                        (org-roam-node-title source-node))))
                (insert reference)))))))))

(add-hook 'org-export-before-processing-hook 'collect-backlinks-string)

(defun mc/publish-notes ()
  "Publishes org notes as html and uploads them to web server."
 (interactive)
 (org-publish "org-roam-notes" t)
 (shell-command "rsync -avh --progress ~/org/roam-html/ -e ssh milos@bytemeer:/volume1/web/"))

(map! :leader
      :prefix "n"
      :desc "Export org notes" "u" #'mc/publish-notes)


(setq-default org-agenda-files (directory-files-recursively "~/org/" "\\.org$"))

(provide 'mc-export-defs)
;;; mc-export-defs.el ends here
