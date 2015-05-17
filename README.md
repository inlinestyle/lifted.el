# lifted.el [![Build Status](https://travis-ci.org/inlinestyle/lifted.el.svg?branch=master)](https://travis-ci.org/inlinestyle/lifted.el)
Functional reactive programming library for Emacs Lisp

## Quick Look
This is a rough draft of an FRP library for Emacs Lisp. I'm working from a number of inspirations, primarily [ReactiveCocoa](http://reactivecocoa.io) and [Elm](http://elm-lang.org).
```elisp
(defvar key-signal (lifted:signal-for-key (kbd "C-c s")))

(funcall key-signal
         :map            (lambda (value) (float-time))
         :defer
         :map            (lambda (value) (round value))
         :filter         (lambda (value) (evenp value))
         :subscribe-next (lambda (value)
                           (message "subscriber reporting even rounded timestamp: %s" value)))
```

## Chainable Operators
 - `lifted:defer`
 - `lifted:filter`
 - `lifted:flatten`
 - `lifted:flatten-map`
 - `lifted:map`
 - `lifted:merge`

## TODO
There's a ton left to do!
 - many more functional operators
 - various signal combination/reduce helpers
 - more diverse deferral options
 - multicasting
 - signal disposal
 - error handling
 - sequences & cold signals
 - possibly reducing the number of `funcall`s
 - cask build
 - getting on MELPA

## Acknowledgements
 - [emacs-deferred](https://github.com/kiwanami/emacs-deferred)
 - [emacs-travis](https://github.com/rolandwalker/emacs-travis)
