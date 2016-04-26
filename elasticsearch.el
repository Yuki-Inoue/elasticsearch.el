
(require 'json)
(require 'tabulated-list)

(defun elasticsearch--raw-snapshots (host port)
  (json-read-from-string
   (shell-command-to-string
    (combine-and-quote-strings
     (list "curl" "-s"
           (concat "http://" host ":" port "/_snapshot"))))))

(defun elasticsearch-convert-snapshots (snapshots)
  (--map
   (list (car it)
         (vector (prin1-to-string (car it))
                 (cdr (assoc 'type (cdr it)))
                 (prin1-to-string (cdr (assoc 'settings (cdr it))))))
   snapshots))

(define-derived-mode elasticsearch-repositories-mode tabulated-list-mode "Containers Menu"
  "Major mode for handling a list of docker containers."

  (setq tabulated-list-format
        '[("Repository" 20 nil)
          ("Type" 10 nil)
          ("Settings" 20 nil)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

(defun elasticsearch-snapshots (host port)
  "List Elasticsearch snapshots."
  (interactive "sHost: \nnPort : ")
  (pop-to-buffer "*elasticsearch-snapshots*")
  (setq tabulated-list-entries
        (elasticsearch-convert-snapshots
         (elasticsearch--raw-snapshots host port)))
  (tabulated-list-init-header)
  (setq tabulated-list-format
        '[("Repository" 20 nil)
          ("Type" 7 nil)
          ("Settings" 20 nil)])
  (elasticsearch-repositories-mode)
  (tabulated-list-revert))

(provide 'elasticsearch)
