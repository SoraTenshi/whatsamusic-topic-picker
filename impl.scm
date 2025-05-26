(load "helper.scm")

(define generate (document.getElementById "generate"))
(define clear (document.getElementById "clear"))
(define nsfw (document.getElementById "nsfw"))
(define spotify (document.getElementById "spotify"))
(define output (document.getElementById "output"))

(define is-nsfw #f)
(define is-spotify #f)

(set! nsfw.onchange (lambda ()
                      (set! is-nsfw nsfw.checked)
                      (init-remaining-counts)))
(set! spotify.onchange (lambda ()
                         (set! is-spotify spotify.checked)))

(define words-in-title
  '("irgendein Land" "'Liebe / Love'" "'Hass / Hate'" "irgendeine Zahl (als gesamter titel)"
    "irgendeine Jahreszeit" "'Junge / Boy'" "'Maedchen / Girl'" "irgendein Biom"
    "'Feuer / Fire'" "'Herz / Heart'" "'Regen / Rain'" "'Traum / Dream'" "irgendeine Stadt" "'Nacht / Night'" "'allein / alone'"
    "Vor- / Nachname" "5 oder mehr Woerter im Titel" "Punktuierung / Sonderzeichen (nicht teil eines Scriptes)" "'Ich / I'" "'Du / You'"
    "5 Buchstaben oder weniger" "irgendein Tier" "eine Wortwiederholung"))

(define topics
  '("Lied zur Beerdigung" "Gaming" "Kulturell bereichernde Musik"
    "70s (1970-1979)" "80s (1980-1989)" "90s (1990-1999)" "00s (2000-2009)" "scuffed songtitel"
    "Meme songs" "toter Kuenstler" "SAUFLIEDER!!!!" "Lieder die man in der Dusche hoeren kann"
    "Baustelle" "Jahreszeiten / Monate" "Lieder aus Medien" "Irgendwas mit Essen"
    "Lieder auf Deutsch"
    "Cover / Remix" "Unbekannte Songs von bekannten Kuenstler (1 Song >100m)" "Song mit Rechtschreibfehler"
    ;; TODO: Create own filter for Genre
    "Rock / Metal" "Elektronische Musik" "Pop"))

(define words-in-title-r18-obf
  '("vetraqjrypur Qebtra" "vetraqjnf zvg Frk" "vetraqrva Nyubxby"))

(define topics-r18-obf
  '("Frk" "Qebtraresnuehat" "Rebgvfpur Zhfvx" "Fgevcpyho ivorf"))

(define words-in-title-r18 (map rot13 words-in-title-r18-obf))
(define topics-r18 (map rot13 topics-r18-obf))

(define artist-used (make-vector 26 #f))
(define title-used (make-vector 36 #f))
(define words-used '())
(define topics-used '())
(define daily-mix-used '())

(define artist-remaining 26)
(define title-remaining 36)
(define words-remaining 0)
(define topics-remaining 0)
(define daily-mix-remaining 300)

(define artist-chars
  (let ((chars (make-vector 26)))
    (do ((i 0 (+ i 1)))
        ((= i 26) chars)
      (vector-set! chars i (integer->char (+ (char->integer #\a) i))))))

(define title-chars
  (let ((chars (make-vector 36)))
    (do ((i 0 (+ i 1)))
        ((= i 26) chars)
      (vector-set! chars i (integer->char (+ (char->integer #\a) i))))
    (do ((i 0 (+ i 1)))
        ((= i 10) chars)
      (vector-set! chars (+ i 26) (integer->char (+ (char->integer #\0) i))))
    chars))

(define (fast-rand-int max)
  (floor (* (Math.random) max)))

(random (Date.now))

(define (init-remaining-counts)
  (set! words-remaining (length (if is-nsfw 
                                   (append words-in-title words-in-title-r18)
                                   words-in-title)))
  (set! topics-remaining (length (if is-nsfw
                                    (append topics topics-r18)
                                    topics))))

(define (random-unique-artist-char)
  (if (<= artist-remaining 0)
      #f
      (let loop ()
        (let ((idx (fast-rand-int 26)))
          (if (vector-ref artist-used idx)
              (loop)
              (begin
                (vector-set! artist-used idx #t)
                (set! artist-remaining (- artist-remaining 1))
                (vector-ref artist-chars idx)))))))

(define (random-unique-title-char)
  (if (<= title-remaining 0)
      #f
      (let loop ()
        (let ((idx (fast-rand-int 36)))
          (if (vector-ref title-used idx)
              (loop)
              (begin
                (vector-set! title-used idx #t)
                (set! title-remaining (- title-remaining 1))
                (vector-ref title-chars idx)))))))

(define (random-list-item lst)
  (if (null? lst)
      #f
      (let ((idx (fast-rand-int (length lst))))
        (list-ref lst idx))))

(define (member-assoc key alist)
  (assoc key alist))

(define (add-to-used used-list item)
  (cons (cons item #t) used-list))

(define (random-title is-nsfw)
  (if (<= words-remaining 0)
      #f
      (let ((available-list (if is-nsfw
                               (append words-in-title words-in-title-r18)
                               words-in-title)))
        (let loop ()
          (let* ((idx (fast-rand-int (length available-list)))
                 (item (list-ref available-list idx)))
            (if (member-assoc item words-used)
                (loop)
                (begin
                  (set! words-used (add-to-used words-used item))
                  (set! words-remaining (- words-remaining 1))
                  item)))))))

(define (random-topic is-nsfw)
  (if (<= topics-remaining 0)
      #f
      (let ((available-list (if is-nsfw
                               (append topics topics-r18)
                               topics)))
        (let loop ()
          (let* ((idx (fast-rand-int (length available-list)))
                 (item (list-ref available-list idx)))
            (if (member-assoc item topics-used)
                (loop)
                (begin
                  (set! topics-used (add-to-used topics-used item))
                  (set! topics-remaining (- topics-remaining 1))
                  item)))))))

(define (random-daily-mix)
  (if (<= daily-mix-remaining 0)
      #f
      (let loop ()
        (let* ((mix (+ 1 (fast-rand-int 6)))
               (song (+ 1 (fast-rand-int 50)))
               (key (string-append (number->string mix) "-" (number->string song))))
          (if (member-assoc key daily-mix-used)
              (loop)
              (begin
                (set! daily-mix-used (add-to-used daily-mix-used key))
                (set! daily-mix-remaining (- daily-mix-remaining 1))
                (list mix song)))))))

(define (filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
        (else (filter pred (cdr lst)))))

(define (get-available-categories max-roll)
  (let ((available '()))
    (if (> title-remaining 0)
        (set! available (cons 0 available)))
    (if (> artist-remaining 0)
        (set! available (cons 1 available)))
    (if (> topics-remaining 0)
        (set! available (cons 2 available)))
    (if (> words-remaining 0)
        (set! available (cons 3 available)))
    (if (and (= max-roll 5) (> daily-mix-remaining 0))
        (set! available (cons 4 available)))
    available))

(define (preprocess-category max-roll version)
  (let ((available (get-available-categories max-roll)))
    (if (null? available)
        #f
        (let* ((chosen-idx (fast-rand-int (length available)))
               (chosen-version (list-ref available chosen-idx))
               (result (case chosen-version
                        ((0) (random-unique-title-char))
                        ((1) (random-unique-artist-char))
                        ((2) (random-topic is-nsfw))
                        ((3) (random-title is-nsfw))
                        ((4) (random-daily-mix))
                        (else #f))))
          (if result
              (list chosen-version result)
              #f)))))

(define (generate-random-category)
  (let* ((max-roll (if is-spotify 5 4))
         (current-roll (fast-rand-int max-roll))
         (cat (preprocess-category max-roll current-roll)))
    (cond ((not cat) "All categories have been exhausted. Please clear the cache.")
          (else (let ((version (car cat))
                      (value (cadr cat)))
                (cond
                  ((= version 0) (string-append "Song beginnt mit: " (string value)))
                  ((= version 1) (string-append "Artist beginnt mit: " (string value)))
                  ((= version 2) (string-append "Topic: " value))
                  ((= version 3) (string-append "Songtitel muss " value " im titel haben."))
                  ((= version 4)
                   (let ((mix (car value))
                         (song (cadr value)))
                     (string-append "Daily Mix " (number->string mix)
                                    " Song Nr. " (number->string song))))
                  (else "Unknown category")))))))

(define (create-new-output content)
  (let ((out (document.getElementById "output"))
        (copy-btn (document.createElement "button")))
    (set! copy-btn.className "copy-btn")
    (set! copy-btn.textContent "Copy")
    (set! copy-btn.onclick copy-to-clipboard)
    (out.replaceChildren content)
    (out.appendChild copy-btn)))

(set! generate.onclick (lambda ()
                         (let ((content (generate-random-category)))
                           (create-new-output content))))


(define (clear-all-caches)
  (vector-fill! artist-used #f)
  (vector-fill! title-used #f)
  (set! words-used '())
  (set! topics-used '())
  (set! daily-mix-used '())
  (set! artist-remaining 26)
  (set! title-remaining 36)
  (set! daily-mix-remaining 300)
  (init-remaining-counts))

(define (select-output)
  (let* ((range (document.createRange))
        (selection (window.getSelection)))
    (range.selectNodeContents output.firstChild)
    (selection.removeAllRanges)
    (selection.addRange range)))

(set! output.onclick select-output)

(define (copy-to-clipboard)
  (let* ((text (--> output.textContent (replace "Copy" "") (trim))))
    (--> '>(navigator.clipboard.writeText text) (then
         (lambda ()
                 (let ((btn (document.querySelector ".copy-btn")))
                   (btn.replaceChildren "Copied!")
                   (window.setTimeout (lambda ()
                                              (let ((btn (document.querySelector ".copy-btn")))
                                                (btn.replaceChildren "Copy")))
                                      1000)))))))


(set! clear.onclick (lambda ()
                      (let ((is-ok (confirm "Are you sure? This will cause possible repeats.")))
                        (if is-ok (begin
                            (clear-all-caches)
                            (create-new-output "Output will be here."))))))

(init-remaining-counts)
