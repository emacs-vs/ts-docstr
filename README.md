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
        - [⌨️ Keys](#️-keys)
        - [🎣 Hooks](#🎣-hooks)
    - [🔨 Supported languages](#🔨-supported-languages)
    - [Contribute](#contribute)
        - [❓ How to create a docstring parser?](#❓-how-to-create-a-docstring-parser)
        - [❓ How to trigger by a key?](#❓-how-to-trigger-by-a-key)

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
| `ts-docstr-mode`     | Use nature key bindings.              |

## 🔧 Customization

### ⌨️ Keys

Better editing experience,

```elisp
(setq ts-docstr-key-support t)
```

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
* Rust
* TypeScript

These languages are in development:

* Python
* Ruby
* Scala
* Swift

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

> ⚠ The best way to learn how a parser works is to look into other files in 
> `/langs` folder from the project root.

All parsers are defined in the `/langs` folder from the project root. The file
is named with the prefix `ts-docstr-` followed by the `language name`. For
example, if you want to create a parser for the `C` programming languge; the
file should be named `ts-docstr-c.el`.

The parser file is consist in three part:

* ts-docstr-[lang]-activate `()`
* ts-docstr-[lang]-parse `(node)`
* ts-docstr-[lang]-insert `(node data)`

The `activate` function is used to search for a node and confirm weather it
should insert a document string.

Use `ts-docstr-activatable-p` function to check to see if you are able to insert
a document string at point, this function returns a node.

```elisp
(defun print-activate-node ()
  (interactive)  ; make interactive, so you could M-x
  (message "node: %s" (ts-docstr-activatable-p)))
```

Evaluate, then `M-x print-activate-node` to see if it return something or `nil`.

### ❓ How to trigger by a key?

WIP
