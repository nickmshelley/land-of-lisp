#lang Racket

(require tests/eli-tester)

(define (http-char c1 c2)
  (define code 
    (string->number
     (string-append "#x" (list->string (list c1 c2)))))
  (if code
      (integer->char code)
      #\space))

(define (decode-param s)
  (list->string
   (let loop ([lst (string->list s)])
     (if (empty? lst)
         empty
         (case (first lst)
           [(#\%) (cons (http-char (second lst) (third lst))
                      (loop (list-tail lst 3)))]
           [(#\+) (cons #\space (loop (rest lst)))]
           [else (cons (first lst) (loop (rest lst)))])))))
(test
 (decode-param "foo") => "foo"
 (decode-param "foo%3F") => "foo?"
 (decode-param "foo+bar") => "foo bar")

(define (parse-params s)
  (map (lambda (p)
         (define result (regexp-split #rx"=" p))
         (cons (first result) (decode-param (second result))))
       (regexp-split #rx"&" s)))
(test
 (parse-params "name=bob&age=25&gender=male")
 => '(("name" . "bob") ("age" . "25") ("gender" . "male"))
 (parse-params "name=bob%3F&age=25&gender=male+guy")
 => '(("name" . "bob?") ("age" . "25") ("gender" . "male guy")))

(define (parse-url s)
  (define path-list
    (regexp-split #rx"\\?" (substring (second (regexp-split #rx" " s)) 1)))
  (if (> (length path-list) 1)
      (cons (first path-list)
            (parse-params (second path-list)))
      path-list))
(test
 (parse-url "GET /lolcats.html HTTP/1.1") => '("lolcats.html")
 (parse-url "GET /lolcats.html?extra-funny=yes HTTP/1.1") => 
 '("lolcats.html" ("extra-funny" . "yes")))

(define (get-header port)
  (define s (read-line port))
  (define splitted (regexp-split #rx": " s))
  (if (> (length splitted) 1)
    (cons (cons (first splitted) (second splitted))
          (get-header port))
    empty))
(test
 (get-header (open-input-string "foo: 1
bar: abc, 123

")) => '(("foo" . "1") ("bar" . "abc, 123")))

(define (get-content-params in header)
  (define l (assoc "content-length" header))
  (if l
      (parse-params (read-string (string->number (cdr l)) in))
      empty))
(test
 (get-content-params (open-input-string "") '()) => empty
 (get-content-params (open-input-string "userid=foo&password=secret")
                     '(("content-length" . "26")))
 => '(("userid" . "foo") ("password" . "secret")))

(define (serve request-handler)
  (define l (tcp-listen 8080))
  (with-handlers ([identity (lambda (exn)
                              (print "exception")
                              (display exn)
                              (tcp-close l))])
    (let loop ()
      (define-values (in out) (tcp-accept l))
      (define url (parse-url (read-line in)))
      (define path (first url))
      (define header (get-header in))
      (define params (append (rest url)
                             (get-content-params in header)))
      (parameterize ([current-output-port out])
        (request-handler path header params))
      (close-input-port in)
      (close-output-port out)
      (loop))))

(define (hello-request-handler path header params)
  (cond
    [(string=? path "greeting")
     (define name (assoc "name" params))
     (if name
         (printf "Nice to meet you, ~a!" (cdr name))
         (printf "<html><form>What is your name?<input name='name' /></form></html>"))]
    [else (display "Sorry, I don't know that page.")]))
(test
 (with-output-to-string 
  (lambda ()
    (hello-request-handler "lolcats" empty empty))) => "Sorry, I don't know that page."
 (with-output-to-string 
  (lambda ()
    (hello-request-handler "greeting" empty empty))) => 
 "<html><form>What is your name?<input name='name' /></form></html>"
 (with-output-to-string 
  (lambda ()
    (hello-request-handler "greeting" empty '(("name" . "Bob"))))) => "Nice to meet you, Bob!")