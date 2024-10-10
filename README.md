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

## Developing

Start the server in slime with:

    SLIME 2.30.git
    CL-USER> (asdf:load-system "coalton-mode")
    ;; COALTON starting in development mode
    ;; COALTON starting with specializations enabled
    ;; COALTON starting with heuristic inlining disabled
    ;; COALTON will emit type annotations
    T
    CL-USER> (coalton-mode::restart-server)
    ;; :INFO coalton-mode: start #<SERVER 127.0.0.1:10001 {700D0A4333}>
    #<COALTON-MODE::SERVER 127.0.0.1:10001 {700D0A4333}>

Open resources/fibonacci.coal, and `M-x eglot`

When prompted, enter 127.0.0.1:10001

