#lang racket/base
(require stretchable-snip
         (prefix-in g: racket/generator)
         racket/function
         racket/gui/base
         racket/contract
         racket/match
         racket/class
         racket/list)
         
(provide multicolumn%)

(define calculated-columns (make-parameter null))

(define (percent->number n)
  (cond
    [(string? n)
     (define but1 (sub1 (string-length n)))
     (if (eqv? (string-ref n but1) #\%)
         (string->number (substring n 0 but1))
         #f)]
    [(symbol? n) (percent->number (symbol->string n))]
    [else #f]))

(define ((calc-column w norm) column)
  (cond
    [((and/c real? (not/c negative?)) column) column]
    [(percent->number column) => (λ (x) (round (/ (* x w) norm)))]
    [else (raise-argument-error 'calc-column
                                "(or/c (and/c real? (not/c negative?)) symbol? string?)"
                                column)]))

(define (add-delta widths delta)
  (match widths
    [(list-rest s rest)
     (if (> s delta)
         (list* (- s delta) rest)
         (list* s (add-delta rest delta)))]
    [else widths]))

(define (calculate-columns sizes width)
  (define-values (to-distribute norm)
    (let loop ([sizes sizes] [sum 0] [norm 0])
      (match sizes
        [(list-rest s rest)
         (if (number? s)
             (loop rest (+ sum s) norm)
             (loop rest sum (+ norm (percent->number s))))]
        [_ (values (- width sum) norm)])))
  (define stage1 (map (if (> to-distribute 0)
                          (calc-column to-distribute norm)
                          (λ (s) (if (number? s) s 1)))
                      sizes))
  (define delta (- (apply + stage1) width))
  (if (= delta 0)
      stage1
      (add-delta stage1 delta)))

(define cell% (class editor-snip%
                (init [width #f])
                (inherit use-style-background get-max-width set-max-width
                         set-min-width get-margin set-max-height set-min-height)
                (super-new
                 [left-inset 0]
                 [right-inset 0]
                 [top-inset 0]
                 [bottom-inset 0]
                 [with-border? #t])
                (use-style-background #t)
                (define/public (set-width! w)
                  (define-values (lm rm) (values (box 0) (box 0)))
                  (get-margin lm (box 0) rm (box 0))
                  (define col (- w (unbox lm) (unbox rm)))
                  (unless (or (< col 0) (eqv? (get-max-width) col))
                    (set-max-width col)
                    (set-min-width col)))
                (define/public (set-height! h)
                  (define-values (tm bm) (values (box 0) (box 0)))
                  (get-margin (box 0) tm (box 0) bm)
                  (define new-h (- h (unbox tm) (unbox bm)))
                  (set-max-height new-h)
                  (set-min-height new-h))
                (when width (set-width! width))))

(define cell-text%
  (class text%
    (super-new)
    (define/override (on-char event)
      (super on-char event)                        
      (define row-editor (send+ this
                                (get-admin) (get-snip) ; cell
                                (get-admin) (get-editor))) ; row text
      (send row-editor begin-edit-sequence #f)
      (send+ row-editor
             (get-admin) (get-snip) ; row
             (update-row-height!))
      (send row-editor end-edit-sequence))))

(define (as-editor v width delta)
  (cond
    [(is-a? v editor-snip%) v]
    [else
     (define t (new cell-text%))
     (send t insert v)
     (new cell% [editor t] [width width])]))

(define editor-snip? (is-a?/c editor-snip%))

(define (add-last l d)
  (match l
    [(list a) (list (+ a d))]
    [(list-rest h r) (list* h (add-last r d))]))

(define (get-minimum-height row-editor)
  (for ([sn (in-snips row-editor editor-snip?)])                       
    (send* sn (set-min-height 'none)
              (set-max-height 'none)))
  (define max-h (box 0))
  (send row-editor get-extent #f max-h)
  (unbox max-h))

(define (row-on-size this w h)
  (define calculated (calculated-columns))
  (unless (null? calculated)
    (define t (send this get-editor))
    (define sn (send+ t (get-admin) (get-snip)))
    (define canvas (send+ sn (get-admin) (get-editor) (get-canvas)))
    (define-values (cl-w cl-h) (canvas-client-size canvas))
    (define widths (add-last calculated (- cl-w w -1)))
    (define height (get-minimum-height t))
    (for ([sn (in-snips t editor-snip?)]
          [width (in-list widths)])
      (send sn resize width height))))

(define (generate-snips editor [filter (λ (snip) #t)])
  (g:generator ()
    (let loop ([snip (send editor find-first-snip)])
      (when snip
        (when (filter snip) (g:yield snip))
        (loop (send snip next))))
    #f))

(define (generate-snips-between editor start end [filter (λ (snip) #t)])
  (g:generator ()
    (let loop ([snip (send editor find-snip start 'before)])
      (when (and snip (<= (send editor get-snip-position snip) end))
        (when (filter snip) (g:yield snip))
        (loop (send snip next))))
    #f))

(define (in-snips editor [filter (λ (snip) #t)])
  (in-producer (generate-snips editor filter) #f))

(define (in-snips-between editor start end [filter (λ (snip) #t)])
  (in-producer (generate-snips-between editor start end filter) #f))

(define base-row% (class editor-snip%
                    (inherit get-editor)
                    (init [data #f] [delta #f] [columns #f])
                    (super-new
                     [left-inset 0]
                     [right-inset 0]
                     [top-inset 0]
                     [bottom-inset 0]
                     [left-margin 0]
                     [right-margin 0]
                     [top-margin 0]
                     [with-border? #f]
                     [bottom-margin 0])
                    (when data
                      (set-data! data columns delta))
                    (define/public (set-data! d columns [delta #f])
                      (define t (get-editor))
                      (send t erase)
                      (send t set-line-spacing 0)
                      (if columns
                          (for ([r (in-list d)]
                                [col (in-list columns)])
                            (send t insert (as-editor r col delta)))
                          (for ([r (in-list d)])
                            (send t insert (as-editor r #f delta))))
                      (send t change-style delta 0))
                    (define/public (update-row-height!)
                      (define t (get-editor))
                      (define new-h (get-minimum-height t))
                      (for ([sn (in-snips t editor-snip?)])
                        (send sn set-height! new-h)))))

(define avoid-focus-text%
  (class text%
    (inherit set-caret-owner get-position find-snip get-focus-snip)
    (super-new)
    (define/public (move-focus)
      (define sn (get-focus-snip))
      (unless (editor-snip? sn)
        (define pos (box 0))
        (get-position pos)
        (define cell-snip
          (let loop ()
            (define snip (find-snip (unbox pos) 'before pos))
            (if (or (= 0 (unbox pos)) (editor-snip? snip)) snip (loop))))
        (when (is-a? cell-snip editor-snip%)
          (set-caret-owner cell-snip))))))

(define row% (class base-row%
               (super-new
                [editor (new (class avoid-focus-text%
                               (inherit move-focus)
                               (super-new)
                               (define/override (on-focus on)                                 
                                 (when on
                                   (move-focus)))))])
               (define/public (set-width! new-sizes)
                 (define t (send this get-editor))
                 (for ([current (in-list current-sizes)]
                       [new (in-list new-sizes)]
                       [cell (in-snips t editor-snip?)])
                   (unless (= current new)
                     (send cell set-width! new)))
                 (set! current-sizes new-sizes))
               (init sizes)
               (field [current-sizes (map (const 0) sizes)])
               (set-width! sizes)))

(define title-row% ((stretchable-snip-static-mixin row-on-size) base-row%))

(define (text-height t)
  (define h (box 0))
  (send t get-extent #f h)
  (- (unbox h) 2))

(define btext%
  (class avoid-focus-text%
    (init-field body column-sizes title-row)
    (super-new)
    (inherit find-snip get-position set-caret-owner find-first-snip move-focus
             refresh get-extent invalidate-bitmap-cache
             in-edit-sequence? get-admin)
    (define update-thread #f)
    (define/augment (on-display-size)
      (define-values (cl-w cl-h) (canvas-client-size body))
      (define new-sizes (calculate-columns column-sizes (+ 3 cl-w)))
      (parameterize ([calculated-columns new-sizes])
        (send title-row on-size cl-w 0))
      (when update-thread
        (send update-thread stop))
      (set! update-thread
            (let ([gen-rows (generate-snips this editor-snip?)])
              (new timer%
                   [interval 1]
                   [notify-callback 
                    (λ ()
                      (send body suspend-flush)
                      (define-values (s e) (values (box 0) (box 0)))
                      (send this get-visible-position-range s e)
                      (for ([screen-row (in-snips-between this (unbox s) (unbox e) editor-snip?)])
                        (send screen-row set-width! new-sizes))
                      (unless (for/and ([n (in-range 10)])
                                (define row (gen-rows))
                                (when row (send row set-width! new-sizes))
                                row)
                        (send update-thread stop)
                        (set! update-thread #f))
                      (send body resume-flush)
                      (yield))]))))
    (define/override (on-event event)
      (super on-event event)
      (when (send event button-down?)
        (move-focus)))))

(define multicolumn%
  (class vertical-pane%
    (init columns [title-bgcolor "gainsboro"])
    (define column-sizes (map second columns))
    (define min-width (for/sum ([size column-sizes])
                            (cond
                              [(percent->number size) => (λ (x) x)]
                              [((and/c real? (not/c negative?)) size) size]
                              [else (raise-argument-error
                                     'multicolumn%
                                     "(or/c (and/c real? (not/c negative?)) symbol? string?)"
                                     size)])))
    (super-new [min-width min-width])
    (define-values (head body)
      (let ([htext (new text%)]
            [delta (make-object style-delta%)])
        (send delta set-delta-background title-bgcolor)
        (define title-row (new title-row% [data (map car columns)] [columns #f] [delta delta]))
        (send htext insert title-row)
        (define head (new editor-canvas%
                          [editor htext]
                          [parent this]
                          [style '(no-focus control-border no-hscroll no-vscroll)]
                          [stretchable-height #f]
                          [horizontal-inset 0]
                          [vertical-inset 0]))
        (send head set-canvas-background (send the-color-database find-color "gainsboro"))
        (send head min-height (inexact->exact (text-height htext)))
        (define body (new editor-canvas%
                          [parent this]
                          [vertical-inset 0]
                          [horizontal-inset 0]
                          [style '(control-border no-hscroll auto-vscroll)]))
        (define btext (new btext%
                         [body body]
                         [column-sizes column-sizes]
                         [title-row title-row]))
        (send btext set-line-spacing 0)
        (send body set-editor btext)
        (values head body)))
    (define/public (add-row data)
      (define btext (send body get-editor))
      (when (send btext find-first-snip)
          (send btext insert "\n"))
      (send btext insert (new row%
                              [data data]
                              [sizes (map (const 0) (calculate-columns column-sizes min-width))])))
    (define/public (scroll-to localx localy w h refresh? [bias 'none])
      (send body scroll-to localx localy w h refresh? bias))))

(module+ test
  (define (demo)
    (define f (new frame% [label "frame"] [width #f] [height 400]))
    (send f create-status-line)
    (define t (new multicolumn%
                   [parent f] [horiz-margin 20]
                   [columns '(("Title 1\nNext" 70%) ("Title 2\nNext" 30%) ("Title 3" 400))]))

    (for ([i 15000])
      (send t add-row (list (format "Str ~a 1" i) (format "Str ~a 2" i) (format "Str ~a 3" i))))
  
    (send t scroll-to 0 0 1 1 #t)
    (send f show #t)
    (void)))
