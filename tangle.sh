#!/bin/sh

DIR=`pwd`

emacs -Q --batch \
    --eval \
    "(progn
     (require 'org)(require 'ob)(require 'ob-tangle)
     (setq org-confirm-babel-evaluate nil)
     (mapc (lambda (file)
            (find-file (expand-file-name file \"$DIR\"))
            (org-babel-tangle)
            (kill-buffer)) '(\"README.org\")))" \
#2>&1 | grep Tangled
