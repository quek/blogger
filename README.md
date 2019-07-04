# rebuild

Markdown を grip で HTML に変更する

grip のインストール

```
pip3 install grip
~/.local/bin/grip --version
```

Emacs の設定

``` emacs-lisp
(setq markdown-command "~/.local/bin/grip --export - -")
```
