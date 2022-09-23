[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![JCS-ELPA](https://raw.githubusercontent.com/jcs-emacs/badges/master/elpa/v/ts-docstr.svg)](https://jcs-emacs.github.io/jcs-elpa/#/ts-docstr)

# ts-docstr
> A document string minor mode using tree-sitter

[![CI](https://github.com/emacs-vs/ts-docstr/actions/workflows/test.yml/badge.svg)](https://github.com/emacs-vs/ts-docstr/actions/workflows/test.yml)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [ts-docstr](#ts-docstr)
    - [💾 Installation](#💾-installation)
        - [🔍 Method 1. with `straight.el` and `use-package`:](#🔍-method-1-with-straightel-and-use-package)
        - [🔍 Method 2. Manual](#🔍-method-2-manual)
    - [📇 Commands](#📇-commands)
    - [🔧 Customization](#🔧-customization)
        - [🎣 Hooks](#🎣-hooks)
    - [🔨 Supported languages](#🔨-supported-languages)
    - [Contribute](#contribute)
        - [❓ How to create a docstring parser?](#❓-how-to-create-a-docstring-parser)

<!-- markdown-toc end -->

## 💾 Installation

### 🔍 Method 1. with `straight.el` and `use-package`:

```elisp
(use-package ts-docstr 
  :straight (ts-docstr :repo "emacs-vs/ts-docstr" :fetcher github
                       :files (:defaults "langs/*.el")))
```

### 🔍 Method 2. Manual

```sh
git clone https://github.com/emacs-vs/ts-docstr /path/to/lib
```

then in Emacs:

```elisp
(add-to-list 'load-path "/path/to/lib")
(require ts-docstr)
```

or

```elisp
(use-package ts-fold
  :load-path "/path/to/lib")
```

## 📇 Commands

| Commands             | Description                           |
|:---------------------|:--------------------------------------|
| `ts-docstr-at-point` | Add document string at current point. |

## 🔧 Customization

### 🎣 Hooks

Hook patterns are:

* `ts-docstr-[module]-before-[activate/parse/insert]-hook`
* `ts-docstr-[module]-after-[activate/parse/insert]-hook`

For example,

* `ts-docstr-c++-before-activate-hook`

or just a general one, (without the language name)

* `ts-docstr-before-activate-hook`

## 🔨 Supported languages
> ⚠️ Please sort these two lists alphabetically!

These languages are fairly complete:

* C / C++ / C#
* Go
* Java / JavaScript
* PHP

These languages are in development:

* Python
* Ruby
* Rust
* Scala
* Swift
* TypeScript

## Contribute

[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](http://makeapullrequest.com)
[![Elisp styleguide](https://img.shields.io/badge/elisp-style%20guide-purple)](https://github.com/bbatsov/emacs-lisp-style-guide)
[![Donate on paypal](https://img.shields.io/badge/paypal-donate-1?logo=paypal&color=blue)](https://www.paypal.me/jcs090218)
[![Become a patron](https://img.shields.io/badge/patreon-become%20a%20patron-orange.svg?logo=patreon)](https://www.patreon.com/jcs090218)

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!

### ❓ How to create a docstring parser?

WIP
