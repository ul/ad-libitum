#!/bin/sh

DIR=`pwd`

# TODO fix htmlize version dep
emacs -Q --batch -L ~/.emacs.d/elpa/htmlize-20171017.141/ \
    --eval \
    "(progn
     (require 'htmlize)
     (setq org-confirm-babel-evaluate nil)
     (mapc (lambda (file)
            (find-file (expand-file-name file \"$DIR\"))
            (org-html-export-to-html)
            (kill-buffer)) '(\"README.org\" \"TTEM.org\")))" \
#2>&1
