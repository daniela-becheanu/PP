#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)


; TODO
; Aveți libertatea să vă structurați programul cum doriți (dar cu restricțiile
; de mai jos), astfel încât funcția serve să funcționeze conform specificației.
(define-struct counter (index tt et queue) #:transparent)
; 
; Restricții (impuse de checker):
; - trebuie să existe în continuare funcția (empty-counter index)
; - cozile de la case trebuie implementate folosind noul TDA queue

(define (empty-counter index)
  (make-counter index 0 0 empty-queue))
  
  

(define (update f counters index)
  (map (lambda (C)
         (if (= (counter-index C) index)
             (f C)
             C))
       counters))

(define (tt+ minutes)
  (λ (C) (match C
           [(counter index tt et queue)
            (struct-copy counter C [tt (+ tt minutes)])])))

(define (et+ minutes)
  (λ (C) (match C
           [(counter index tt et queue)
            (struct-copy counter C [et (+ et minutes)])])))

(define (add-to-counter name items)     ; testată de checker
  (λ (C)                                ; nu modificați felul în care funcția își primește argumentele
    (if (queue-empty? (counter-queue C))
        (match C
          [(counter index tt et queue)
           (struct-copy counter C [tt (+ tt items)]
                        [et (+ et items)]
                        [queue (enqueue (cons name items) queue)])])
        (match C
          [(counter index tt et queue)
           (struct-copy counter C [tt (+ tt items)]
                        [queue (enqueue (cons name items) queue)])])
        )
    ))


(define (f-counter-tt C)
  (match C [(counter index tt et queue) tt]))

(define (f-counter-et C)
  (match C [(counter index tt et queue) et]))

(define (min-elem counters C f)
  (cond ((null? counters) (cons (counter-index C) (f C)))
        ((> (f C) (f (car counters))) (min-elem (cdr counters) (car counters) f))
        (else (min-elem (cdr counters) C f))))

(define (min-tt counters) (if (null? counters)
                              null
                              (min-elem counters (car counters) f-counter-tt)))

(define (min-et counters) (if (null? counters)
                              null
                              (min-elem counters (car counters) f-counter-et)))

(define (remove-first-from-counter C)   ; testată de checker
  (if (or (and (= 1 (stream-length (queue-left (counter-queue C)))) (null? (queue-right (counter-queue C))))
          (and (= 1 (length (queue-right (counter-queue C)))) (null? (queue-left (counter-queue C)))))
      (match C
        [(counter index tt et queue)
         (struct-copy counter C [tt (- tt et)]
                      [et 0]
                      [queue empty-queue])])
      (match C
        [(counter index tt et queue)
         (struct-copy counter C [tt (- tt et)]
                      [et (cdr (top (dequeue queue)))]
                      [queue (dequeue queue)])])))


; TODO
; Implementați o funcție care calculează starea unei case după un număr dat de minute.
; Funcția presupune, fără să verifice, că în acest timp nu a ieșit nimeni din coadă, 
; deci va avea efect doar asupra câmpurilor tt și et.
; (cu alte cuvinte, este responsabilitatea utilizatorului să nu apeleze această funcție
; cu minutes > timpul până la ieșirea primului client din coadă)
; Atenție: casele fără clienți nu trebuie să ajungă la timpi negativi!


(define (pass-time-through-counter minutes)
  (λ (C)
    (if (< (counter-tt C) minutes)
        (struct-copy counter C [tt 0]
                     [et 0])
        (struct-copy counter C [tt (- (counter-tt C) minutes)]
                     [et (- (counter-et C) minutes)]))))

(define (mergesort L f)
  (sort-helper L (length L) f))

(define (sort-helper L len f)
  (if (<= len 1) L
      (merge (sort-helper (take L (quotient len 2))
                          (quotient len 2) f)
             (sort-helper (drop L (quotient len 2))
                          (- len (quotient len 2)) f)
             f)))


(define (merge L R f)
  (cond ((null? L) R)
        ((null? R) L)
        ((>= (f (car L)) (f (car R))) (cons (car L) (merge (cdr L) R f)))
        (else (cons (car R) (merge L (cdr R) f)))))

(define (comp-minutes L)
  (car L))

(define (comp-index C)
  (- 0 (counter-index C)))
  

(define (remove-clients x result)
  (λ(C)
    (cond ((< x 0) (cons C result))
          ((queue-empty? (counter-queue C)) (cons ((pass-time-through-counter x) C) result))
          ((<= (counter-et C) x) ((remove-clients (- x (counter-et C))
                                                  (cons (cons (- x (counter-et C)) (cons (counter-index C) (car (top (counter-queue C))))) result))
                                  (remove-first-from-counter C)))
          (else ((remove-clients -1 result) ((pass-time-through-counter x) C))))))

; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 3, apare un nou tip de cerere, așadar
; requests conține 5 tipuri de cereri (cele moștenite din etapa 3 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă              (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute         (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)           (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                       (ca înainte)
;   - (close <index>) - casa index este închisă                                            (   NOU!   )
; Sistemul trebuie să proceseze cele 5 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele DESCHISE cu tt minim; nu se va întâmpla
;   niciodată ca o persoană să nu poată fi distribuită la nicio casă                       (mică modificare)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți);
;   nu aplicați vreun tratament special caselor închise                                    (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele DESCHISE, 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>         (mică modificare)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică. (ca înainte)
; - când o casă se închide, ea nu mai primește clienți noi; clienții care erau deja acolo
;   avansează normal, până la ieșirea din supermarket                                    
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul:
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista cozilor (de la case DESCHISE sau ÎNCHISE) care încă au clienți:
;   - fiecare element va avea forma (index_casă . coadă) (coada este de tip queue)
;   - lista este sortată după indexul casei

(define (serve requests fast-counters slow-counters)
  (define (serve-helper requests fast-counters slow-counters fast-open-counters slow-open-counters result)

    (define (compute-sum-tt counters)
      (foldl (λ(C acc)
               (+ (counter-tt C) acc))
             0
             counters))

    (define (compute-average-tt sum length)
      (/ sum length))

    (define (ensure-average fast-open-counters slow-open-counters average index)
      (if (>= average (compute-average-tt (compute-sum-tt (append fast-open-counters slow-open-counters))
                                          (length (append fast-open-counters slow-open-counters))))
          slow-open-counters
          (ensure-average fast-open-counters
                          (append slow-open-counters (list (empty-counter (add1 index))))
                          average
                          (add1 index))))

    (define (add-counters counters index k) ; k = nr de case ce trebuie adaugate, index = indexul de inceput
      (if (<= k 0)
          counters
          (add-counters (append counters (list (empty-counter index))) (add1 index) (sub1 k))))
    
    (if (null? requests)
        (cons result (map (λ(C) (cons (counter-index C) (counter-queue C))) (filter (λ(C) (not (queue-empty? (counter-queue C))))
                                                                                    (append fast-counters slow-counters))))
        (match (car requests)
          [(list 'close index)
           (serve-helper (cdr requests)
                         fast-counters
                         slow-counters
                         (filter (λ(C)
                                   (not (equal? (counter-index C) index)))
                                 fast-open-counters)
                         (filter (λ(C)
                                   (not (equal? (counter-index C) index)))
                                 slow-open-counters)
                         result)]
          [(list 'ensure average)
           (serve-helper (cdr requests)
                         fast-counters
                         (add-counters slow-counters (add1 (counter-index (car (reverse slow-counters))))
                                       (- (ceiling (/ (compute-sum-tt (append fast-open-counters slow-open-counters)) average))
                                          (length (append fast-open-counters slow-open-counters))))
                         fast-open-counters
                         (add-counters slow-open-counters (add1 (counter-index (car (reverse slow-counters))))
                                       (- (ceiling (/ (compute-sum-tt (append fast-open-counters slow-open-counters)) average))
                                          (length (append fast-open-counters slow-open-counters))))
                         result)]
      
          [(list name n-items)
           (if (<= n-items ITEMS)
               (serve-helper (cdr requests)
                             (update (add-to-counter name n-items)
                                     fast-counters
                                     (car (min-tt (mergesort (append fast-open-counters slow-open-counters) comp-index))))
                             (update (add-to-counter name n-items)
                                     slow-counters
                                     (car (min-tt (mergesort (append fast-open-counters slow-open-counters) comp-index))))
                             (mergesort (update (add-to-counter name n-items)
                                                fast-open-counters
                                                (car (min-tt (mergesort (append fast-open-counters slow-open-counters) comp-index)))) comp-index)
                             (mergesort (update (add-to-counter name n-items)
                                                slow-open-counters
                                                (car (min-tt (mergesort (append fast-open-counters slow-open-counters) comp-index)))) comp-index)
                             result)
               (serve-helper (cdr requests)
                             fast-counters
                             (update (add-to-counter name n-items)
                                     slow-counters
                                     (car (min-tt (mergesort slow-open-counters comp-index))))
                             fast-open-counters
                             (update (add-to-counter name n-items)
                                     slow-open-counters
                                     (car (min-tt (mergesort slow-open-counters comp-index))))
                             result))]
      
          [(list 'delay index minutes)
           (serve-helper (cdr requests)
                         (update (tt+ minutes) (update (et+ minutes) fast-counters index) index)
                         (update (tt+ minutes) (update (et+ minutes) slow-counters index) index)
                         (mergesort (update (tt+ minutes) (update (et+ minutes) fast-open-counters index) index) comp-index)
                         (mergesort (update (tt+ minutes) (update (et+ minutes) slow-open-counters index) index) comp-index)
                         result)]
          
          [x (serve-helper (cdr requests)
                           (map car (map (remove-clients x null) fast-counters))
                           (map car (map (remove-clients x null) slow-counters))
                           (mergesort (map car (map (remove-clients x null) fast-open-counters)) comp-index)
                           (mergesort (map car (map (remove-clients x null) slow-open-counters)) comp-index)
                           (append result (map cdr (mergesort (apply append (filter (λ(L) (not (null? L))) (map cdr (map (remove-clients x null)
                                                                                                                         (append fast-counters slow-counters))))) comp-minutes))))])))
  (serve-helper requests fast-counters slow-counters fast-counters slow-counters null))



