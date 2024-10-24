This is an Emacs mode that supports working with Coalton.

## Requirements

This mode has been developed using `eglot`, which is included with
recent versions of Emacs.

## Usage

In the Emacs init file (probably `~/.emacs.d/init.el`), add this
directory to the load-path, and require the mode:

    ;; Coalton
    
    (add-to-list 'load-path "~/git/coalton-mode")
    (require 'coalton-mode)

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

In Emacs, open a .caol file (e.g. resources/fibonacci.coal), and
enable eglot mode:

    M-x eglot
