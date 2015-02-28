(import (srfi 89))

(define* (f a (b #f)) (list a b))
(display (f 1)) (newline)

(define* (g a (b a) (key: k (* a b)))
  (list a b k))
(display (g 4 3 'key: 5)) (newline)


