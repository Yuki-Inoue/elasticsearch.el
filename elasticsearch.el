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
(require 'tabulated-list)

(defvar-local elasticsearch-host "localhost"
  "Host name (or IP) of Elasticsearch.")

(defvar-local Elasticsearch-port 9200
  "Port of Elasticsearch")

(defun elasticsearch--raw-snapshots (host port)
  (json-read-from-string
   (shell-command-to-string
    (combine-and-quote-strings
     (list "curl" "-s"
           (concat "http://" host ":" (number-to-string port) "/_snapshot"))))))

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
  (tabulated-list-init-header))

(defun elasticsearch-snapshots-refresh ()
  "Refresh elasticsearch snapshots."

  (setq tabulated-list-entries
        (elasticsearch-convert-snapshots
         (elasticsearch--raw-snapshots
          elasticsearch-host
          elasticsearch-port))))

(defun elasticsearch-snapshots (host port)
  "List Elasticsearch snapshots."
  (interactive "sHost: \nnPort : ")
  (pop-to-buffer "*elasticsearch-snapshots*")
  (setq elasticsearch-host host)
  (setq elasticsearch-port port)
  (elasticsearch-snapshots-refresh)
  (tabulated-list-init-header)
  (elasticsearch-repositories-mode)
  (tabulated-list-revert))

(defvar elasticsearch-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "s" 'elasticsearch-snapshots)
    map))

(provide 'elasticsearch)
