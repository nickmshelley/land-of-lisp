#lang racket
(require tests/eli-tester
         racket/system)

(provide graph->png ugraph->png)

(define (dot-name exp)
  (regexp-replace* #px"[^[:word:]]" exp "_"))
(test
 (dot-name "") => ""
 (dot-name "hi_there") => "hi_there"
 (dot-name "hi there") => "hi_there"
 (dot-name "any one-here? I=don't_think)so:")
 => "any_one_here__I_don_t_think_so_")

; dot-label : list -> string
(define (dot-label exp)
  (define max-label-length 30)
  (if (empty? exp)
      ""
      (let ([s (let ([string-out (open-output-string)])
                 (display exp string-out)
                 (get-output-string string-out))])
        (if (> (string-length s) 30)
            (string-append (substring s 0 (- max-label-length 3)) "...")
            s))))
(test
 (dot-label '()) => ""
 (dot-label '(short one)) => "(short one)"
 (dot-label '(30 chars aaaaabbbbbcccccdddd)) => "(30 chars aaaaabbbbbcccccdddd)"
 (dot-label '(31 chars aaaaabbbbbcccccddddd)) => "(31 chars aaaaabbbbbcccccdd...")

(define (nodes->dot nodes)
  (for-each (lambda (node)
              (newline)
              (display (dot-name (symbol->string (first node))))
              (display "[label=\"")
              (display (dot-label node))
              (display "\"];"))
            nodes))

(define (edges->dot edges)
  (for-each (lambda (node)
              (for-each (lambda (edge)
                          (newline)
                          (display (dot-name (symbol->string (first node))))
                          (display "->")
                          (display (dot-name (symbol->string (first edge))))
                          (display "[label=\"")
                          (display (dot-label (rest edge)))
                          (display "\"];"))
                        (rest node)))
            edges))

(define (graph->dot nodes edges)
  (display "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (display "}"))

(define (dot->png filename thunk)
  (with-output-to-file filename thunk #:exists 'replace)
  (putenv "PATH" (string-append (getenv "PATH") ":/usr/local/bin"))
  (system (string-append "dot -Tpng -O " filename)))

(define (graph->png filename nodes edges)
  (dot->png filename
            (lambda ()
              (graph->dot nodes edges))))

(define (uedges->dot edges)
  (let loop ([remaining edges])
    (unless (empty? remaining)
      (define node (first remaining))
      (for-each (lambda (edge)
                  (unless (assoc (first edge) remaining)
                    (newline)
                    (display (dot-name (symbol->string (first node))))
                    (display "--")
                    (display (dot-name (symbol->string (first edge))))
                    (display "[label=\"")
                    (display (dot-label (rest edge)))
                    (display "\"];")))
                (rest node))
      (loop (rest remaining)))))

(define (ugraph->dot nodes edges)
  (display "graph{")
  (nodes->dot nodes)
  (uedges->dot edges)
  (display "}"))

(define (ugraph->png filename nodes edges)
  (dot->png filename
            (lambda ()
              (ugraph->dot nodes edges))))
