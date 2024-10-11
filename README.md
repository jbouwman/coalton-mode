This is an Emacs mode that supports working with Coalton.

## Requirements

`lsp-mode`

## Installation

In your emacs init file (probably `~/.emacs.d/init.el` or `~/.emacs`), 
add this directory to your load-path, and require the mode:

    ;; Coalton
    
    (add-to-list 'load-path "~/git/coalton-mode")
    (require 'coalton-mode)

## Usage

Open a .coal file (e.g., fibonacci.coal in the root directory) and
enable lsp-mode With `M-x lsp-mode`.
