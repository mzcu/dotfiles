;;; mc-export-defs.el -*- lexical-binding: t; -*-

(setq org-html-postamble nil)
(setq org-export-with-section-numbers 1)

(setq org-publish-project-alist
      '(

        ("org-roam-notes"
        :base-directory "~/org/roam"
        :auto-sitemap t
        :auto-index t
        :sitemap-filename "index.org"
        :sitemap-format-entry mc/roam-sitemap-entry-format
        :base-extension "org"
        :publishing-directory "~/org/roam-html"
        :recursive t
        :publishing-function org-html-publish-to-html
        :headline-levels 4
        :auto-preamble t
        )

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


(defun mc/roam-sitemap-entry-format (entry style project)
  (cond ((not (directory-name-p entry))
         (let ((id (with-temp-buffer (insert-file-contents (org-publish--expand-file-name entry project)) (car (org-property-values "ID")))))
           (if id
               (format "[[id:%s][%s]]" id (org-publish-find-title entry project))
               (format "[[file:%s][%s]]" entry (org-publish-find-title entry project)))))
       ((eq style 'tree)
         ;; Return only last subdir.
         (file-name-nondirectory (directory-file-name entry)))
       (t entry))
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
                     (outline (when-let ((outline (plist-get properties :outline)))
                                  (mapconcat #'org-link-display-format outline " > ")))
                     (point (org-roam-backlink-point backlink))
                     (text (s-replace "\n" " " (org-roam-preview-get-contents
                                                (org-roam-node-file source-node)
                                                point)))
                     (reference (format "%s [[id:%s][%s]]\n%s\n%s\n\n"
                                        (s-repeat (+ (org-roam-node-level node) 2) "*")
                                        (org-roam-node-id source-node)
                                        (org-roam-node-title source-node)
                                        (if outline (format "%s (/%s/)"
                                        (s-repeat (+ (org-roam-node-level node) 3) "*") outline) "")
                                        "" ;text
                                        )))
                (insert reference)))))))))

(add-hook 'org-export-before-processing-hook 'collect-backlinks-string)

(defun mc/publish-notes ()
 (interactive)
 (org-publish "org-roam-notes" t)
 (shell-command "rsync -avh --progress ~/org/roam-html/ -e ssh milos@bytemeer:/volume1/web/"))

(map! :leader
      :prefix "n"
      :desc "Export org notes" "u" #'mc/publish-notes)

