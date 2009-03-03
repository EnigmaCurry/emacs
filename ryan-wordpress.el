;; This is a completely NONWORKING and UNIMPLEMENTED wordpress mode.
;; I'll probably never finish this, as I started working
;; on Blogofile instead (http://www.blogofile.com)




(defun wordpress-mode ()
  (kill-all-local-variables)
  (buffer-disable-undo)
  (setq buffer-read-only t)
  (setq major-mode 'wordpress-mode
	mode-name "Wordpress"
	mode-line-process ""
	truncate-lines t
	line-move-visual nil))

(defun wordpress-get-posts ()
  (xml-rpc-method-call wordpress-blog-url 'metaWeblog.getRecentPosts 0 wordpress-blog-user wordpress-blog-pass 0))

(defun wordpress ()
  (interactive)
  (let ((buf (get-buffer-create "*Wordpress-Posts*")))
    (wordpress-login)
    (save-excursion
      (set-buffer buf)
      (setq buffer-read-only nil)
      (erase-buffer)
      (loop for post in (pymacs-eval "wordpress_blog.get_titles_published()")
            do
            (let ((id (nth 0 post))
                  (title (nth 1 post)))
              (put-text-property 0 (length title) 'post-id id title)
              (insert (format "%s\n" title))))
      (wordpress-mode)
      )
    (switch-to-buffer buf)))

(defun wordpress-open-post (post-id)
  "Retrieve a blog post from the server and edit it in a new buffer"
  