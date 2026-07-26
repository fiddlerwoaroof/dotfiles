(setq fwoar-git-mode :ssh)

(use-package sendmail
  :ensure t
  :custom
  ( message-sendmail-envelope-from 'header)
  ( mail-envelope-from 'header)
  ( mail-specify-envelope-from t)
  ( send-mail-function 'sendmail-send-it)
  ( sendmail-program "/Users/edwlan/.nix-profile/bin/msmtp"))

(use-package notmuch
  :pin "melpa-stable"
  :ensure t
  :custom
  ( notmuch-search-oldest-first nil)
  ( notmuch-saved-searches '((:name "important" :query "tag:attend")
                             (:name "inbox" :query
	                                  "tag:inbox and ( date:-4weeks.. or tag:flagged )" :key
	                                  [105] :sort-order newest-first)
                             (:name "unread" :query "tag:unread and tag:inbox" :key [117])
                             (:name "flagged" :query "tag:flagged and tag:inbox" :key [102])
                             (:name "receipts" :query "tag:receipt")
                             (:name "sent" :query "tag:sent" :key [116])
                             (:name "drafts" :query "tag:draft" :key [100])
                             (:name "all mail" :query "not tag:archive" :key [97])
                             (:name "last-day" :query "date:-1days..now" :key [108])))
  ( notmuch-command "/Users/edwlan/bin/notmuch-remote")
  ( notmuch-fcc-dirs nil)
  :config
  (with-eval-after-load 'evil-collection
    (fwoar:setup-evil-collection-for-mode 'notmuch)))
