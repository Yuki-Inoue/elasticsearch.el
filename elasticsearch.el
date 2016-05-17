;; Copyright (C) 2016 Yuki Inoue <inouetakahiroki _at_ gmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(require 'json)
(require 'tablist)

(defvar-local elasticsearch-host "localhost"
  "Host name (or IP) of Elasticsearch.")

(defvar-local elasticsearch-port 9200
  "Port of Elasticsearch")

(defun elasticsearch-url ()
  (concat "http://" elasticsearch-host ":"
          (number-to-string elasticsearch-port) "/_snapshot"))

(defun elasticsearch--raw-command (&rest args)
  (shell-command-to-string
    (combine-and-quote-strings args)))

(defun elasticsearch--raw-snapshots (host port)
  (json-read-from-string
   (elasticsearch--raw-command
    "curl" "-s" (elasticsearch-url))))

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
  (add-hook 'tabulated-list-revert-hook 'elasticsearch-snapshots-refresh nil t)
  (tabulated-list-init-header)
  (tablist-minor-mode))

(magit-define-popup elasticsearch-put-snapshot-popup "Snapshots Popup"
  :actions '((?P "Put Snapshot" elasticsearch-put-snapshot-selection)))

(define-key elasticsearch-repositories-mode-map (kbd "P") 'elasticsearch-put-snapshot-popup)

(defun elasticsearch-put-snapshot-selection (snapshot-name)
  (interactive "sSnapshot Name : ")
  (dolist (entry (tablist-get-marked-items))
    (let ((repository (car entry)))
      (message
       (elasticsearch--raw-command
        "curl" "-s" "-XPUT"
        (concat (elasticsearch-url)
                "/" (symbol-name repository)
                "/es-" (shell-command-to-string "date +%Y-%m-%d-%H-%M-%S")
                "?wait_for_completion=true&pretty-command"))))))

(defun elasticsearch-snapshots-refresh ()
  "Refresh elasticsearch snapshots."

  (setq tabulated-list-entries
        (elasticsearch-convert-snapshots
         (elasticsearch--raw-snapshots
          elasticsearch-host
          elasticsearch-port))))

(defun elasticsearch-snapshots (host port)
  "List Elasticsearch snapshots."
  (interactive (list
                (read-string "Host: " "localhost")
                (read-number "Port: " 9200)))
  (pop-to-buffer "*elasticsearch-snapshots*")
  (elasticsearch-repositories-mode)
  (setq elasticsearch-host host)
  (setq elasticsearch-port port)
  (tablist-revert))

(defvar elasticsearch-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "s" 'elasticsearch-snapshots)
    map))

(provide 'elasticsearch)
