(use-package zig-mode
  :after lsp-mode
  :ensure t
  :hook
  (zig-mode . lsp)
  :custom
  (lsp-zig-zls-executable (expand-file-name "~/zls/zls")))
