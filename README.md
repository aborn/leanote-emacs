# leanote-mode
Writing markdown blog in emacs with elegant [leanote](https://leanote.com/[leanote])
and [its open source platform](http://leanote.org/).

## Install
Install it from elpa package source (i.e. [melpa](https://melpa.org/) or [popkit elpa](https://elpa.popkit.org/)).  
```elisp
M-x package-install RET leanote-mode RET
```

## Usage
Add following code to your init(.emacs or init.el anyway) file.
```elisp
(add-hook 'markdown-mode-hook 'leanote)
```

## Hotkey
* C-c u leanote-push-current-file-to-remote


