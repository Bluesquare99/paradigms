;; We said that our emycin looks at all possible rules for each
;; parameter, because there is no telling how a later rule may affect
;; the certainty factor. Actually, that is not quite true. If there is
;; a rule that leads to a conclusion with certainty 1, then no other
;; rules need be considered. This was called a unity path. Modify the
;; program to look for unity paths first.
