(in-package :maxclaims)

(define-symbol-macro $window 
    (context.window-component *context*))

(define-symbol-macro $body 
    (current-component $window))


