;; Copyright (c) 2015 by Gregor Klinke
;; All rights reserved.

;; register a default text mapping
(text
    (literal (node-property 'data (current-node) default: "")))

(default
    (process-children))
