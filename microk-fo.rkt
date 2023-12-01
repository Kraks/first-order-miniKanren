#lang racket
(provide
  (all-from-out "common.rkt")
  (struct-out disj)
  (struct-out conj)
  (struct-out relate)
  (struct-out ==)
  (struct-out =/=)
  (struct-out symbolo)
  (struct-out stringo)
  (struct-out numbero)
  (struct-out not-symbolo)
  (struct-out not-stringo)
  (struct-out not-numbero)
  (struct-out mplus)
  (struct-out bind)
  (struct-out pause)
  step
  mature
  mature?)

(require "common.rkt")

;; first-order microKanren
(struct disj     (g1 g2)                  #:prefab)
(struct conj     (g1 g2)                  #:prefab)
(struct relate   (thunk description)      #:prefab)
(struct ==       (t1 t2)                  #:prefab)
(struct =/=      (t1 t2)                  #:prefab)
(struct symbolo  (t)                      #:prefab)
(struct stringo  (t)                      #:prefab)
(struct numbero  (t)                      #:prefab)
(struct not-symbolo (t)                      #:prefab)
(struct not-stringo (t)                      #:prefab)
(struct not-numbero (t)                      #:prefab)
(struct bind     (bind-s bind-g)          #:prefab)
(struct mplus    (mplus-s1 mplus-s2)      #:prefab)
(struct pause    (pause-state pause-goal) #:prefab)

(define (mature? s) (or (not s) (pair? s)))
(define (mature s)
  (if (mature? s) s (mature (step s))))

(define (start st g)
  (match g
    ((disj g1 g2)
     (step (mplus (pause st g1)
                  (pause st g2))))
    ((conj g1 g2)
     (step (bind (pause st g1) g2)))
    ((relate thunk _)
     (pause st (thunk)))
    ((== t1 t2) (state->stream (unify t1 t2 st)))
    ((=/= t1 t2) (state->stream (disunify t1 t2 st)))
    ((symbolo t) (state->stream (typify t symbol? st)))
    ((stringo t) (state->stream (typify t string? st)))
    ((numbero t) (state->stream (typify t number? st)))
    ((not-symbolo t) (state->stream (distypify t symbol? st)))
    ((not-stringo t) (state->stream (distypify t string? st)))
    ((not-numbero t) (state->stream (distypify t number? st)))))

(define (loop r)
  (if (procedure? r)
      (begin
        (set! r (r))
        (loop r))
      r))

(define (step s)
  (loop (stepk s (lambda (x) x))))

(define (stepk s k)
  (lambda ()
    (match s
      [(mplus s1 s2)
       (let [(k^ (lambda (s1)
                   (cond [(not s1) (k s2)]
                         [(pair? s1)
                          (k (cons (car s1) (mplus s2 (cdr s1))))]
                         [else (k (mplus s2 s1))])))]
         (if (mature? s1) (k^ s1) (stepk s1 k^)))]
      [(bind s g)
       (let [(k^ (lambda (s)
                   (cond [(not s) (k #f)]
                         [(pair? s)
                          (stepk (mplus (pause (car s) g)
                                        (bind (cdr s) g))
                                 k)]
                         [else (k (bind s g))])))]
         (if (mature? s) (k^ s) (stepk s k^)))]
      [(pause st g) (k (start st g))]
      [_            (k s)])))

