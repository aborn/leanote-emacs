(add-to-list 'load-path default-directory)
(require 'leanote)
(require 'request)
(require 'pcache)
(mapc #'byte-compile-file '("leanote.el"))
