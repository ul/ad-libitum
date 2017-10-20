#!/bin/sh

DIR=`pwd`

emacs -Q --batch -L ~/.emacs.d/elpa/htmlize-*/ \
    --eval \
    "(progn
     (require 'htmlize)
     (setq org-confirm-babel-evaluate nil)
     (mapc (lambda (file)
            (find-file (expand-file-name file \"$DIR\"))
            (org-html-export-to-html)
            (kill-buffer)) '(\"README.org\" \"TTEM.org\")))" \
#2>&1
