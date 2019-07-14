# flymake-go-staticcheck

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![MELPA](https://melpa.org/packages/flymake-go-staticcheck-badge.svg)](https://melpa.org/#/flymake-go-staticcheck)

Flymake backend for Go using [staticcheck](https://github.com/dominikh/go-tools/tree/master/cmd/staticcheck).

## Installation

You can now install package `flymake-go-staticcheck` from
[MELPA](https://melpa.org/#/getting-started). Just `M-x`
`package-install`<kbd>Enter</kbd> `flymake-go-staticcheck` <kbd>Enter</kbd>.

Also you need install [staticcheck](https://github.com/dominikh/go-tools/tree/master/cmd/staticcheck).

``` shell
go get -u honnef.co/go/tools/cmd/staticcheck
```

## Configuration

``` emacs-lisp
(add-hook 'go-mode-hook #'flymake-go-staticcheck-enable)
(add-hook 'go-mode-hook #'flymake-mode)
```

### Customization

`M-x` `customize` <kbd>Enter</kbd> `flymake-go-staticcheck` 

## Contributing

Patches are welcome :) You also can send your bugreports and feature
requests [here](https://github.com/s-kostyaev/flymake-go-staticcheck/issues/new).
