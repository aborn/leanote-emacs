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
**Note: ** Config your own server api if you use youself host(vps) as following  
```elisp
(setq leanote-api-root "youryomain/api")
```

## Hotkey
* **C-c u** update or add new note content to remote server
* **C-c d** delete current note
* **C-c r** leanote-rename

## Leanote log
All logs are recorded in \*Leanote-Log* buffer.

-----------------------------------------------------------------------

# leanote-mode
leanote-mode是emacs下的一个minor-mode,
在emacs中采用[leanote](https://leanote.com/)提供的服务写markdown笔记。

## 安装
从elpa的源中进行安装（如[melpa](https://melpa.org/) 或者 [popkit elpa](https://elpa.popkit.org/)).）  
```elisp
M-x package-install RET leanote-mode RET
```

## 使用
将下面代码添加到你emacs的启动文件(.emacs 或者 init.el)
```elisp
(add-hook 'markdown-mode-hook 'leanote)
```
如果你是自己部署了leanote的服务，配置自己服务的api
```elisp
(setq leanote-api-root "youryomain/api")
```

## 快捷键
* **C-c u** 更新或者添加当前笔记到服务器
* **C-c d** 删除当前笔记
* **C-c r** 修改当前笔记名

## 操作日志
所有的操作日志被记录在\*Leanote-Log* 这个buffer里。
