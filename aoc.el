;;; aoc.el --- Convenience functions for advent of code -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Aleksandr Pokatilov

;; Version: 0.0.1
;; URL: https://github.com/NeuroCorgi/aoc.el

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary

;; This package provides convenience function to submit solutions
;; for advent of code puzzles.
;;
;; Package caches all the downloaded inputs and submission to
;; relieve load on the server.

;;; Code:

(require 'url)

(defgroup aoc.el nil "")

(defcustom aoc-cache-dir
  "~/.adventofcode/"
  "Directory to save puzzle inputs and attempts"
  :group aoc.el
  :type string)

(defcustom aoc-session-cookie-file
  "~/.adventofcode.session"
  "File with advent of code session cookie string"
  :group aoc.el
  :type string)

(defcustom aoc-sesseion-cookie
  nil
  "Autharised session coockie"
  :group aoc.el
  :type string)

(defcustom aoc-run-alist
  `(("\\.py\\'" . ("python3" "%f" "%i"))
	("\\.hs\\'" . ("runhaskell" "%f" "%i"))
	("\\.el\\'" . ("emacs" "--script" "%f" "%i")))
  "Alist to choose command to run submitted file"
  :group 'aoc.el
  :type `(alist :key-type string :value-type (list string)))

(defun aoc-run (input-file file-name)
  (defun format-arg (elem)
	(cond ((string= elem "%f") file-name)
		  ((string= elem "%i") input-file)
		  (t elem)))
  (let ((command (cdr (seq-find (lambda (runner)
								  (let ((regexp (car runner)))
									(string-match-p regexp file-name)))
								aoc-run-alist
								(cons nil nil)))))
	(if command
		(with-temp-buffer
		  (let* ((command (seq-map #'format-arg command))
				 (exit-code (apply #'call-process
								   (car command)
								   input-file
								   t
								   nil
								   (cdr command))))
			(if (= exit-code 0)
				(string-trim (buffer-string))
			  (error "Execution of %s failed with exit code %d"
					 file-name
					 exit-code))))
	  (error "Not found command to run file: %s" file-name))))

(defun aoc--string-last-match (regexp str)
  (let (last-match
		(pos 0))
	(while (string-match regexp str pos)
	  (setq last-match (match-string 0 str))
	  (setq pos (match-end 0)))
	last-match))
  
(defun aoc--file-to-day (file-name)
  (let* ((regexp "d\\(ay\\)?\\([0-9]\\{1,2\\}\\)")
		 (last-match (aoc--string-last-match regexp file-name)))
	(string-match regexp last-match)
	(string-to-number (match-string 2 last-match))))

(defun aoc--submit (file-path day part)
  (message (format "Program result: %s" (aoc-run "input.txt" file-path))))

(defun aoc-submit ()
  (interactive)
  (let* ((file-name (read-file-name "File to submit: " nil nil t))
		 (full-file-name (expand-file-name file-name))
		 (day  (read-number    "Day: " (aoc--file-to-day full-file-name)))
		 (part (read-number    "Part: " 1)))
	(cond
	 ((or (< day 1) (> day 25))
	  (error "Day must be from 1 to 25, given: %d" day))
	 ((not (or (= part 1) (= part 2)))
	  (error "Part must either 1 or 2, given: %d" part))
	 (t (aoc--submit full-file-name day part)))))

;;; End of aoc.el
