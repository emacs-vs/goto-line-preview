[![Build Status](https://travis-ci.com/jcs090218/goto-line-preview.svg?branch=master)](https://travis-ci.com/jcs090218/goto-line-preview)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)


# goto-line-preview #
> Preview line when executing `goto-line` command.

Normally `goto-line` will just ask for input of the line number 
then once you hit `RET`; it will just go to that line of code. 
This package makes this better by navigating the line while you 
are inputting in minibuffer.


## Usage ##
```el
(define-key global-map (kbd "M-g") #'goto-line-preview-goto-line)
```


## Screenshot ##
<img src="./screenshot/goto-line-preview-demo.gif" width="300" height="341"/>


## Contribution ##
If you would like to contribute to this project. You may either
clone and make pull request to this repository. Or you can
clone the project and make your own branch of this tool. Any
methods are welcome!
