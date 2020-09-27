[![Build Status](https://travis-ci.com/jcs-elpa/goto-line-preview.svg?branch=master)](https://travis-ci.com/jcs-elpa/goto-line-preview)
[![MELPA](https://melpa.org/packages/goto-line-preview-badge.svg)](https://melpa.org/#/goto-line-preview)
[![MELPA Stable](https://stable.melpa.org/packages/goto-line-preview-badge.svg)](https://stable.melpa.org/#/goto-line-preview)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

# goto-line-preview
> Preview line when executing `goto-line` command.

<p align="center">
  <img src="./etc/goto-line-preview-demo.gif" width="450" height="513"/>
</p>

Normally `goto-line` will just ask for input of the line number then once you hit 
`RET`; it will just go to that line of code. This package makes this better by 
navigating the line while you are inputting in minibuffer.

*P.S. Inspired by [Visual Studio Code](https://code.visualstudio.com/) goto line preset behavior.*

## Usage

Call it from `minibuffer` directly, 

```
M-x goto-line-preview
```

Or you can bind it globally to replace `goto-line`:

```el
(global-set-key [remap goto-line] 'goto-line-preview)
```

## Contribution

If you would like to contribute to this project, you may either 
clone and make pull requests to this repository. Or you can 
clone the project and establish your own branch of this tool. 
Any methods are welcome!
