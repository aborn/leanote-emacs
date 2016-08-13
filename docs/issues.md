# 中文名的问题
有时候可能会出现**"工作笔记"**这样的中文名，原因是在见面版编辑时产生的。

## recipe
1. 去掉spaceline
(require 'spaceline)

2. 去掉swiper和helm

## 可能的安装问题
1. 如果运行环境没有安装spaceline的话，直接安装leanote后会引起leanote-spaceline-status
的问题，因为使用了*spaceline-define-segment*这个宏，编译的时候没有展开。因此会被当作函数
来看待。运行leanote-spaceline-status会报以下错
```elisp
(invalid-function spaceline-define-segment)
```

