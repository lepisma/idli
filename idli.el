;;; idli.el --- Interactive Debate via LLMs -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav@lepisma.xyz>
;; Version: 0.0.1
;; Package-Requires: ((emacs "29"))
;; Keywords: llm, learning
;; URL: https://github.com/lepisma/idli

;;; Commentary:

;; Interactive Debate via LLMs
;; This file is not a part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'cl-seq)
(require 'llm)
(require 'llm-openai)
(require 'org)

(defvar idli-debators nil
  "Variable holding prompts for debators.")

(defvar idli-llm-provider nil
  "Variable holding the LLM provider for use in chat.")

(setq llm-warn-on-nonfree nil)

(defun idli-generate-debators-prompts (topic)
  "Generate system prompts for debators for the TOPIC using llm PROVIDER."
  (let ((prompt (llm-make-chat-prompt (format "You have to generate system prompts for LLMs to take different stances in a debate on the topic: '%s'. Prefer to have two stances (pro and anti) only. The prompts should tell the debator to use logic, well framed short arguments, and data points in a debate with others. Each reply by a debator would be an argument or introduction of an stance, nothing else. Other than other stances, the debate moderator might intervene and their point should be respected. Separate each prompt with 5 dashes like this ----- and only give the prompt, no headings." topic))))
    (cl-remove-if #'string-empty-p
     (mapcar #'string-trim (string-split (llm-chat idli-llm-provider prompt) "-----")))))

(defun idli-start (topic)
  (interactive "s")
  (setq idli-llm-provider (make-llm-openai :key (auth-info-password (car (auth-source-search :host "api.openai.com"))) :chat-model "gpt-4o"))
  (setq idli-debators (idli-generate-debators-prompts topic))
  (switch-to-buffer (get-buffer-create "*idli*"))
  (delete-region (point-min) (point-max))
  (org-mode)
  (visual-line-mode 1)
  (insert "#+TITLE: " topic "\n\n")
  (insert "This is a debate between " (number-to-string (length idli-debators)) " debators on the above topic. To start with, each member will put their opening arguments one by one.")
  (idli-open-all))

(defun idli-open (debator-name debator-prompt)
  "Write opening argument for the debator."
  (let ((prompt (llm-make-chat-prompt (format "%s\n\nReturn your opening argument on the topic. Don't write anything other than that, no prefix with your name." (buffer-substring-no-properties (point-min) (point-max))) :context debator-prompt)))
    (insert "** Debator " debator-name ">\n" (string-trim (llm-chat idli-llm-provider prompt)) "\n\n")))

(defun idli-open-all ()
  "Write openings for all debators."
  (goto-char (point-max))
  (insert "\n\n")
  (let ((labels '("A" "B" "C" "D" "E" "F" "G")))
    (dolist (prompt idli-debators)
      (idli-open (car labels) prompt)
      (setq labels (cdr labels)))))

(defun idli-continue (debator-name debator-prompt)
  (let ((prompt (llm-make-chat-prompt (format "%s\n\nReturn your argument based on the above discussion till now. Don't write anything other than that, no prefix with your name." (buffer-substring-no-properties (point-min) (point-max))) :context debator-prompt)))
    (insert "** Debator " debator-name ">\n" (string-trim (llm-chat idli-llm-provider prompt)) "\n\n")))

(defun idli-continue-all ()
  "Write continuations for all debators."
  (interactive)
  (goto-char (point-max))
  (insert "\n\n")
  (let ((labels '("A" "B" "C" "D" "E" "F" "G")))
    (dolist (prompt idli-debators)
      (idli-continue (car labels) prompt)
      (setq labels (cdr labels)))))

(provide 'idli)

;;; idli.el ends here
