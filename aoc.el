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

(defgroup aoc.el nil "Convenience tool for Advent of Code")

(defcustom aoc-cache-dir
  "~/.adventofcode/"
  "Directory to save puzzle inputs and attempts"
  :group 'aoc.el
  :type 'string)

(defcustom aoc-session-cookie-file
  "~/.adventofcode.session"
  "File with advent of code session cookie string"
  :group 'aoc.el
  :type 'string)

(defcustom aoc-session-cookie
  nil
  "Authorised session cookie"
  :group 'aoc.el
  :type 'string)

;; TODO: add possibility for two step commands
;;       when compilation is required
(defcustom aoc-run-alist
  `(("\\.py\\'" . ("pypy3" "%f" "%i"))
	("\\.hs\\'" . ("runhaskell" "%f" "%i"))
	("\\.el\\'" . ("emacs" "--script" "%f" "%i"))
	("\\.bqn\\'" . ("BQN" "%f" "%i")))
  "Alist to choose command to run submitted file"
  :group 'aoc.el
  :type '(alist :key-type string :value-type (list string)))

(defconst aoc--download-input-url-format
  "https://adventofcode.com/%d/day/%d/input")

(defconst aoc--submit-answer-url-format
  "https://adventofcode.com/%d/day/%d/answer")

(defconst aoc--user-agent-header
  "https://github.com/NeuroCorgi/aoc.el by pokatilov0802@gmail.com")

(defconst aoc--right-answer-regexp
  "That's the right answer")

(defconst aoc--incorrect-answer-regexp
  "That's not the right answer")

(defconst aoc--recent-answer-regexp
  "You gave an answer too recently")

(defconst aoc--wrong-level-regexp
  "You don't seem to be solving the right level")

(defun aoc--session-cookie ()
  (string-trim
   (cond
	((null aoc-session-cookie)
	 (if (null aoc-session-cookie-file)
		 (error "No session cookie specified")
	   (with-temp-buffer
		 (insert-file-contents aoc-session-cookie-file)
		 (buffer-string))))
	((functionp aoc-session-cookie) (aoc-session-cookie))
	(t aoc-session-cookie))))

(defun aoc--session-cookie-string ()
  (concat "session=" (aoc--session-cookie)))

(defun aoc--download-input--callback (status file year day)
  (if (eq (car status) :error)
	  (error "Failed to fetch input data for day %d, %d"
			 day year)
	(with-current-buffer (current-buffer)
	  (goto-char (point-min))
	  (search-forward "\n\n")
	  (write-region (point) (point-max) file)
	  (message (format "Downloaded input data for day %d, %d to %s"
					   day year file)))))

(defun aoc--download-input (file year day)
  (unless (file-exists-p file)
	(make-directory (file-name-directory file) t)
	(write-region "" nil file))
  (let ((url (url-encode-url
			  (format aoc--download-input-url-format
					  year day)))
		(url-request-extra-headers
		 `(("User-Agent" . ,aoc--user-agent-header)
		   ("Cookie"     . ,(aoc--session-cookie-string)))))
	(print url-request-extra-headers)
	(message file)
	(url-retrieve url #'aoc--download-input--callback (list file year day))))

(defun aoc--input-file (year day)
  (let* ((base-dir (expand-file-name aoc-cache-dir))
		 (year-s (concat "y" (number-to-string year)))
		 (year-dir (expand-file-name year-s base-dir))
		 (day-s (concat "d" (number-to-string day)))
		 (day-file (expand-file-name day-s year-dir)))
	day-file))

(defun aoc--run (input-file file-name)
  (defun format-arg (elem)
	(cond ((string= elem "%f") file-name)
		  ((string= elem "%i") input-file)
		  (t elem)))
  (let ((command
		 (cdr (seq-find
			   (lambda (runner)
				 (let ((regexp (car runner)))
				   (string-match-p regexp file-name)))
			   aoc-run-alist
			   (cons nil nil)))))
	(unless command
	  (error "Not found command to run file: %s" file-name))
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
				 exit-code))))))

(defun aoc--submit--callback (status answer year day part)
  (if (eq (car status) :error)
	  (error "Failed to submit solution for part %d, day %d, %d"
			 part day year)
	(let ((response 
		   (with-current-buffer (current-buffer)
			 (buffer-substring-no-properties (point-min) (point-max)))))
	  (cond
	   ((string-match-p aoc--right-answer-regexp response)
		;; TODO: add answer caching
		(message (format "That's the right answer %s for part %d, day %d, %d"
						 answer part day year)))
	   ((string-match-p aoc--incorrect-answer-regexp response)
		;; TODO: Check for "too low"/"too high" and cache it
		(message (format "That's not the right answer %s for part %d, day %d, %d"
						 answer part day year)))
	   ((string-match-p aoc--recent-answer-regexp response)
		(message "You gave an answer too recently"))
	   ((string-match-p aoc--wrong-level-regexp response)
		(message "You don't seem to be solving the right level"))
	   (t (message "Got an unknown response")
		  (write-region (point-min) (point-max) "debug-output"))))))

(defun aoc--submit (file-path year day part)
  (let ((input-file (aoc--input-file year day)))
	(unless (file-exists-p input-file)
	  (aoc--download-input input-file year day))
	(let* ((answer (aoc--run input-file file-path))
		   (url (url-encode-url
				 (format aoc--submit-answer-url-format
						 year day)))
		   (url-request-method "POST")
		   (url-request-extra-headers
			`(("User-Agent" . ,aoc--user-agent-header)
			  ("Content-Type" . "application/x-www-form-urlencoded")
			  ("Cookie" . ,(aoc--session-cookie-string))))
		   (url-request-data (format "level=%d&answer=%s"
									 part answer)))
	  (print url-request-extra-headers)
	  (print url-request-data)
	  (message (format "Submitted solution %s for part %d, day %d, %d"
					   answer part day year))
	  (url-retrieve url #'aoc--submit--callback
					(list answer year day part)))))

(defun aoc--string-last-match (regexp str)
  (let (last-match
		(pos 0))
	(while (string-match regexp str pos)
	  (setq last-match (match-string 0 str))
	  (setq pos (match-end 0)))
	last-match))

(defun aoc--last-december-year ()
  (let ((year (string-to-number (format-time-string "%Y"))))
	(if (string= "12" (format-time-string "%M"))
		year
	  (1- year))))

(defun aoc--from-file (file-name default regexp)
  (if (null file-name)
	  default
	(let* ((last-match (aoc--string-last-match regexp file-name)))
	  (if last-match
		  (progn
			(string-match regexp last-match)
			(string-to-number (match-string 2 last-match)))
		default))))

(defun aoc--file-to-year (file-name)
  (aoc--from-file file-name
				   (aoc--last-december-year)
				   "y\\(ear\\)?\s*?\\([0-9]\\{4\\}\\)"))

(defun aoc--file-to-day (file-name)
  (aoc--from-file file-name
				  1
				  "d\\(ay\\)?\s*?\\([0-9]\\{1,2\\}\\)"))

(defun aoc--file-to-part (file-name)
  (aoc--from-file file-name
				  1
				  "p\\(art\\)?\s*\\([0-9]\\)"))

(defun aoc--get-file-name ()
  (interactive)
  (let* ((file-name (read-file-name "Solution file: " nil nil t))
		 (full-file-name (expand-file-name file-name)))
	(cond
	 ((not (file-exists-p full-file-name))
	  (error "File doesn't exist: %s" file-name))
	 ((file-directory-p full-file-name)
	  (error "File is a directory: %s" file-name))
	 ((not (file-readable-p full-file-name))
	  (error "Cannot read file: %s"))
	 (t full-file-name))))

(defun aoc--get-year (&optional file-name)
  (interactive)
  (let ((year (read-number "Year: " (aoc--file-to-year file-name))))
	(cond 
	 ((< year 2015)
	  (error "There was no advent of code event that year: %d" year))
	 ((> year (aoc--last-december-year))
	  (error "It's too early for advent of code of that year: %d" year))
	 (t year))))

(defun aoc--get-day (&optional file-name)
  (interactive)
  (let ((day (read-number "Day: " (aoc--file-to-day file-name))))
	(if (and (>= day 1) (<= day 25))
		day
	  (error "Day must be from 1 to 25: %d" day))))

(defun aoc--get-part (&optional file-name)
  (interactive)
  (let ((part (read-number "Part: " (aoc--file-to-part file-name))))
	(if (or (= part 1) (= part 2))
		part
	  (error "Part must either 1 or 2: %d" part))))

;;;###autoload
(defun aoc-submit ()
  (interactive)
  (let* ((file-name (aoc--get-file-name))
		 (year      (aoc--get-year file-name))
		 (day       (aoc--get-day  file-name))
		 (part      (aoc--get-part file-name)))
	 (aoc--submit file-name year day part)))

;;;###autoload
(defun aoc-test ()
  (interactive)
  (let* ((file-name (aoc--get-file-name))
		 (test-name (read-file-name "Test file: " nil nil t)))
	(message (format "Program result with test data: %s"
					 (aoc--run (expand-file-name test-name)
							   (expand-file-name file-name))))))

;;;###autoload
(defun aoc-download ()
  (interactive)
  (let* ((year      (aoc--get-year))
		 (day       (aoc--get-day))
		 (file-name (read-file-name "File: " nil nil nil))
		 (file-name (expand-file-name file-name)))
	(aoc--download-input file-name year day)))

;;; End of aoc.el
