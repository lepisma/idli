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

(defcustom idli-buffer-name "*idli*"
  "Buffer name to keep idli conversations.")

(defvar idli-debators nil
  "Variable holding prompts for debators.")

(defvar idli-llm-provider nil
  "Variable holding the LLM provider for use in chat.")

(setq llm-warn-on-nonfree nil)

(defun idli-generate-debators-prompts (topic callback)
  "Generate system prompts for debators for the TOPIC using llm PROVIDER."
  (let ((prompt (llm-make-chat-prompt (format "You have to generate system prompts for LLMs to take different stances in a debate on the topic: '%s'. Prefer to have two stances (pro and anti) only. The prompts should tell the debator to use logic, well framed short arguments, and data points in a debate with others. Each reply by a debator would be an argument or introduction of an stance, nothing else. Other than other stances, the debate moderator might intervene and their point should be respected. Separate each prompt with 5 dashes like this ----- and only give the prompt, no headings." topic))))
    (llm-chat-async idli-llm-provider prompt
                    (lambda (response)
                      (with-current-buffer idli-buffer-name
                        (setq idli-debators (cl-remove-if #'string-empty-p (mapcar #'string-trim (string-split response "-----"))))
                        (funcall callback)))
                    (lambda (err) (error "%s" err)))))

(defun idli-start (topic)
  (interactive "sWrite topic: ")
  (switch-to-buffer idli-buffer-name)
  (setq idli-llm-provider (make-llm-openai :key (auth-info-password (car (auth-source-search :host "api.openai.com"))) :chat-model "gpt-4o"))
  (delete-region (point-min) (point-max))
  (org-mode)
  (visual-line-mode 1)
  (insert "#+TITLE: " topic "\n\n")
  (idli-generate-debators-prompts topic
                                  (lambda ()
                                    (insert "This is a debate between " (number-to-string (length idli-debators)) " debators on the above topic. To start with, each member will put their opening arguments one by one."))))

(defun idli-step (debator-name debator-prompt instruction callback)
  "Step ahead and insert response for one debator."
  (let ((prompt (llm-make-chat-prompt (format "%s\n\n%s" (buffer-substring-no-properties (point-min) (point-max)) instruction) :context debator-prompt)))
    (llm-chat-async idli-llm-provider prompt
                    (lambda (response)
                      (with-current-buffer idli-buffer-name
                        (insert "** Debator " debator-name ">\n" (string-trim response) "\n\n")
                        (funcall callback)))
                    (lambda (err) (error "%s" err)))))

(defun idli--step-recursive (labels debators instruction)
  "Recursively write opening arguments for DEBATORS."
  (when (and labels debators)
    (let ((debator-name (car labels))
          (debator-prompt (car debators)))
      (idli-step debator-name debator-prompt instruction
                 (lambda () (idli--step-recursive (cdr labels) (cdr debators) instruction))))))

(defun idli-open-all ()
  "Initiate opening arguments for all debators."
  (interactive)
  (goto-char (point-max))
  (insert "\n\n")
  (idli--step-recursive '("A" "B" "C" "D" "E" "F" "G") idli-debators "Return your opening argument on the topic. Don't write anything other than that, no prefix with your name."))

(defun idli-continue-all ()
  "Continue arguments for all debators."
  (interactive)
  (goto-char (point-max))
  (insert "\n\n")
  (idli--step-recursive '("A" "B" "C" "D" "E" "F" "G") idli-debators "Return your argument based on the above discussion till now. Don't write anything other than that, no prefix with your name."))

(provide 'idli)

;;; idli.el ends here
