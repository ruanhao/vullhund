;; -*- coding: utf-8 -*-
;; author: Hao Ruan
;; date: 2014/1/26

;; Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.org/>
;; Everyone is permitted to copy and distribute verbatim copies
;; of this license document, but changing it is not allowed.

;; vullhund, a plugin used for Erlang programming, paring keywords like *begin*,
;; *fun*, *case*, *if*, *receive*, and *end*. vullhund helps to jump to the
;; corresponding keyword if TWO KEYWORDS are far from each other. This will make
;; code review much easier.

(defun vullhund-pick-word-at-point ()
  "pick current word under cursor
this function would move cursor to the beginning of the word"
  (let (tail-point)
    (skip-chars-forward "-_A-Za-z0-9")
    (setq tail-point (point))
    (skip-chars-backward "-_A-Za-z0-9")
    (buffer-substring-no-properties (point) tail-point)))

(defun vullhund-erlang-pair-keyword-valid-p ()
  (let ((line-head-point (line-beginning-position)))
    (if (and
         (= 0 (% (count-matches "\"" line-head-point (point)) 2))
         (= 0 (% (count-matches "'" line-head-point (point)) 2))
         (= 0 (count-matches "%" line-head-point (point))))
        t
      nil)))

(defun vullhund-erlang-pair-construct-stack (value old-stack)
  (if (= 0 (+ value (car old-stack)))
      (cdr old-stack)
    (cons value old-stack)))

(defun vullhund-erlang-pair-find (direction stack origin-point)
  (catch 'ok
    (while t
      (condition-case nil
          (progn
            (funcall direction "\\(^\\|[\s\t\\[(=>]\\)\\(case\\|if\\|begin\\|receive\\|fun[\s\t\n]*(\.*\\|end\\)\\($\\|[\s\t,;.]\\)")
            (goto-char (match-beginning 2))
            (setq stack
                  (if (not (vullhund-erlang-pair-keyword-valid-p))
                      stack
                    (if (looking-at "end")
                        (vullhund-erlang-pair-construct-stack -1 stack)
                      (vullhund-erlang-pair-construct-stack 1 stack))))
            (if stack
                (forward-char)          ; a trick here, there is no need to use
                                        ; (backward-char) here when do backward-search,
                                        ; but you have to use (forward-char) when do forward-search
              (throw 'ok t)))
        (error (progn
                 (message "Wrong format")
                 (goto-char origin-point)
                 (throw 'ok t)))))))

(defun vullhund-erlang-pair ()
  "find pair for if, case, begin for Erlang mode"
  (interactive)
  (when (eq major-mode 'erlang-mode)
    (let ((keywords '("case" "if" "begin" "receive")))
      (when (vullhund-erlang-pair-keyword-valid-p)
        (if (or (member (vullhund-pick-word-at-point) keywords)
                (looking-at "fun[\s\t\n]*(")) ; 'fun' is an except
            (progn
              (forward-char)
              (vullhund-erlang-pair-find 'search-forward-regexp '(1) (point)))
          (if (equal (vullhund-pick-word-at-point) "end")
              (progn
                ;; (backward-char)
                (vullhund-erlang-pair-find 'search-backward-regexp '(-1) (point)))))))))
