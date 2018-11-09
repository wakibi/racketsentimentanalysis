#lang racket

(require data-science)
(require plot)
(require math)
(require json)

;;; I want date->string from srfi/19, not racket/date. But I want the
;;; other stuff
(require (except-in racket/date date->string))
(require (except-in srfi/19 current-date))
;;(require pict)

(define (json-tweets->json-array #:head [head #f])
  (let loop ([num 0]
             [json-array '()]
             [record (read-json(current-input-port))])
    (if (or (eof-object? record)
           (and head (>= num head)))
        (jsexpr->string json-array)
        (loop (add1 num)(cons record json-array)
              (read-json (current-input-port))))))

(define (clean-tweets extracted-tweets)
  (map (lambda (t)
         (string-normalize-spaces
          (remove-punctuation
           (remove-urls
            (string-downcase t)))))
       extracted-tweets))

(define read-tweets (string->jsexpr
                     (with-input-from-file "2018_oct.json" (lambda () (json-tweets->json-array)))))

(define extracted-text-and-timestamp
  (let([tmp (map (lambda (t)(list (hash-ref t 'text) (hash-ref t 'created_at))) read-tweets)])
    (filter (lambda (t) (not (string-prefix? (first t) "RT"))) tmp)))

(define (convert-tweet-time str)
  (string->date str "~a ~b ~d ~H:~M:~S ~z ~Y"))

(define tweets-by-month
  (map(lambda (t)(list (first t)
                       (cond [(string-contains? (second t) "Jan") "Jan"]
                             [(string-contains? (second t) "Feb") "Feb"]
                             [(string-contains? (second t) "Mar") "Mar"]
                             [(string-contains? (second t) "Apr") "Apr"]
                             [(string-contains? (second t) "May") "May"]
                             [(string-contains? (second t) "Jun") "Jun"]
                             [(string-contains? (second t) "Jul") "Jul"]
                             [(string-contains? (second t) "Aug") "Aug"]
                             [(string-contains? (second t) "Sept") "Sept"]
                             [(string-contains? (second t) "Oct") "Oct"]
                             [(string-contains? (second t) "Nov") "Nov"]
                             [(string-contains? (second t) "Dec") "Dec"])))
      extracted-text-and-timestamp))

;;clean and separate tweets by month
(define jan (filter (lambda (t) (string=? (second t) "Jan")) tweets-by-month))
(define feb (filter (lambda (t) (string=? (second t) "Feb")) tweets-by-month))
(define mar (filter (lambda (t) (string=? (second t) "Mar")) tweets-by-month))
(define apr (filter (lambda (t) (string=? (second t) "Apr")) tweets-by-month))
(define may (filter (lambda (t) (string=? (second t) "May")) tweets-by-month))
(define jun (filter (lambda (t) (string=? (second t) "Jun")) tweets-by-month))
(define jul (filter (lambda (t) (string=? (second t) "Jul")) tweets-by-month))
(define aug (filter (lambda (t) (string=? (second t) "Aug")) tweets-by-month))
(define sept (filter (lambda (t) (string=? (second t) "Sept")) tweets-by-month))
(define oct (filter (lambda (t) (string=? (second t) "Oct")) tweets-by-month))
(define nov (filter (lambda (t) (string=? (second t) "Nov")) tweets-by-month))
(define dec (filter (lambda (t) (string=? (second t) "Dec")) tweets-by-month))

(define jan-a (map (lambda (x)
                 (remove-stopwords x))
               (cond [(empty? jan) jan]
                     [else (map (lambda (y)
                      (string-split (clean-tweets y)))
                    ($ jan 0))])))

(define feb-a (map (lambda (x)
                 (remove-stopwords x))
               (cond [(empty? feb) feb]
                     [else (map (lambda (y)
                      (string-split (clean-tweets y)))
                    ($ feb 0))])))

(define mar-a (map (lambda (x)
                 (remove-stopwords x))
               (cond [(empty? mar) mar]
                     [else (map (lambda (y)
                      (string-split (clean-tweets y)))
                    ($ mar 0))])))

(define apr-a (map (lambda (x)
                 (remove-stopwords x))
               (cond [(empty? apr) apr]
                     [else (map (lambda (y)
                      (string-split (clean-tweets y)))
                    ($ apr 0))])))

(define may-a (map (lambda (x)
                 (remove-stopwords x))
               (cond [(empty? may) may]
                     [else (map (lambda (y)
                      (string-split (clean-tweets y)))
                    ($ may 0))])))

(define jun-a (map (lambda (x)
                 (remove-stopwords x))
               (cond [(empty? jun) jun]
                     [else (map (lambda (y)
                      (string-split (clean-tweets y)))
                    ($ jun 0))])))

(define jul-a (map (lambda (x)
                 (remove-stopwords x))
               (cond [(empty? jul) jul]
                     [else (map (lambda (y)
                      (string-split (clean-tweets y)))
                    ($ jul 0))])))

(define aug-a (map (lambda (x)
                 (remove-stopwords x))
               (cond [(empty? aug) aug]
                     [else (map (lambda (y)
                      (string-split (clean-tweets y)))
                    ($ aug 0))])))

(define sept-a (map (lambda (x)
                 (remove-stopwords x))
               (cond [(empty? sept) sept]
                     [else (map (lambda (y)
                      (string-split (clean-tweets y)))
                    ($ sept 0))])))

(define oct-a (map (lambda (x)
                 (remove-stopwords x))
               (cond [(empty? oct) oct]
                     [else (map (lambda (y)
                      (string-split (clean-tweets y)))
                    ($ oct 0))])))

(define nov-a (map (lambda (x)
                 (remove-stopwords x))
               (cond [(empty? nov) nov]
                     [else (map (lambda (y)
                      (string-split (clean-tweets y)))
                    ($ nov 0))])))

(define dec-a (map (lambda (x)
                 (remove-stopwords x))
               (cond [(empty? dec) dec]
                     [else (map (lambda (y)
                      (string-split (clean-tweets y)))
                    ($ dec 0))])))

(define jan-p (filter (lambda (x) (not (equal? x ""))) (flatten jan-a)))
(define feb-p (filter (lambda (x) (not (equal? x ""))) (flatten feb-a)))
(define mar-p (filter (lambda (x) (not (equal? x ""))) (flatten mar-a)))
(define apr-p (filter (lambda (x) (not (equal? x ""))) (flatten apr-a)))
(define may-p (filter (lambda (x) (not (equal? x ""))) (flatten may-a)))
(define jun-p (filter (lambda (x) (not (equal? x ""))) (flatten jun-a)))
(define jul-p (filter (lambda (x) (not (equal? x ""))) (flatten jul-a)))
(define aug-p (filter (lambda (x) (not (equal? x ""))) (flatten aug-a)))
(define sept-p (filter (lambda (x) (not (equal? x ""))) (flatten sept-a)))
(define oct-p (filter (lambda (x) (not (equal? x ""))) (flatten oct-a)))
(define nov-p (filter (lambda (x) (not (equal? x ""))) (flatten nov-a)))
(define dec-p (filter (lambda (x) (not (equal? x ""))) (flatten dec-a)))

(define all (append jan-p feb-p mar-p apr-p may-p jun-p jul-p aug-p sept-p oct-p nov-p dec-p))

(define jan-words (sort (sorted-counts jan-p)
                     (lambda (x y) (> (second x) (second y)))))
(define feb-words (sort (sorted-counts feb-p)
                     (lambda (x y) (> (second x) (second y)))))
(define mar-words (sort (sorted-counts mar-p)
                     (lambda (x y) (> (second x) (second y)))))
(define apr-words (sort (sorted-counts apr-p)
                     (lambda (x y) (> (second x) (second y)))))
(define may-words (sort (sorted-counts may-p)
                     (lambda (x y) (> (second x) (second y)))))
(define jun-words (sort (sorted-counts jun-p)
                     (lambda (x y) (> (second x) (second y)))))
(define jul-words (sort (sorted-counts jul-p)
                     (lambda (x y) (> (second x) (second y)))))
(define aug-words (sort (sorted-counts aug-p)
                     (lambda (x y) (> (second x) (second y)))))
(define sept-words (sort (sorted-counts sept-p)
                     (lambda (x y) (> (second x) (second y)))))
(define oct-words (sort (sorted-counts oct-p)
                     (lambda (x y) (> (second x) (second y)))))
(define nov-words (sort (sorted-counts nov-p)
                     (lambda (x y) (> (second x) (second y)))))
(define dec-words (sort (sorted-counts dec-p)
                     (lambda (x y) (> (second x) (second y)))))
(define all-words (sort (sorted-counts all)
                     (lambda (x y) (> (second x) (second y)))))


;;; Plot the top 20 words from both devices combined
(parameterize ([plot-width 600]
               [plot-height 600])
    (plot (list
        (tick-grid)
        (discrete-histogram (reverse (take all-words 20))
                            #:invert? #t
                            #:color "DimGray"
                            #:line-color "DimGray"
                            #:y-max 450))
       #:x-label "Occurrences"
       #:y-label "word"))

;(define (get-moods-of-tweets-for-year json-file keywords)
;  )

;;(print read-tweets)

;;(convert-tweet-time "Tue Oct 30 23:59:39 +0000 2018")