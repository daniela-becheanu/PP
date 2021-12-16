#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)

;; ATENȚIE: Pentru această etapă a temei este necesar să implementați
;;          întâi TDA-ul queue în fișierul queue.rkt.
;; Reveniți la sarcinile din acest fișier după ce ați implementat tipul 
;; queue și ați verificat implementarea folosind checker-ul.


; Structura counter nu se modifică.
; Ceea ce se modifică este implementarea câmpului queue:
; - în loc de listă, acesta va fi o coadă (o structură de tip queue)
; - acest lucru nu este vizibil în definiția structurii counter,
;   ci în implementarea operațiilor acestui tip de date (tipul counter)
(define-struct counter (index tt et queue) #:transparent)


; TODO
; Actualizați funcțiile de mai jos astfel încât ele să folosească
; o structură de tip queue pentru reprezentarea cozii de persoane.
; Elementele cozii continuă să fie perechi (nume . nr_produse).
; Este esențial să respectați "bariera de abstractizare", adică să
; operați cu coada doar folosind interfața acestui TDA:
; - empty-queue
; - queue-empty?
; - enqueue
; - dequeue
; - top
; Obs: Doar câteva funcții vor necesita actualizări.
(define (empty-counter index)           ; testată de checker
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
  (if (or (and (= 1 (length (queue-left (counter-queue C)))) (null? (queue-right (counter-queue C))))
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
(define (modif x)
  (if (<= x 0)
      0
      x))

(define (pass-time-through-counter minutes)
  (λ (C)
    (if (queue-empty? (counter-queue C))
        (match C
          [(counter index tt et queue)
           (struct-copy counter C [tt (modif(- tt minutes))]
                        [et (modif (- et minutes))])])
        (match C
          [(counter index tt et queue)
           (struct-copy counter C [tt (- tt minutes)]
                        [et (- et minutes)])]))))

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
  

; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 2, apar modificări în:
; - formatul listei de cereri (parametrul requests)
; - formatul rezultatului funcției (explicat mai jos)
; requests conține 4 tipuri de cereri (3 moștenite din etapa 2 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă            (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute       (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al tuturor caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)         (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                     (   NOU!   )
; Obs: Au dispărut cererile de tip remove-first, pentru a lăsa loc unui mecanism mai 
; sofisticat de a scoate clienții din coadă (pe măsură ce trece timpul).
; Sistemul trebuie să proceseze cele 4 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)  (ca înainte)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți) (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele (și cele fast, și cele slow), 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>       (ca înainte)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică.
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista caselor în starea finală (ca rezultatul din etapele 1 și 2)
; Obs: Pentru a contoriza ieșirile din cozi, puteți să lucrați într-o funcție ajutătoare
; (cu un parametru în plus față de funcția serve), pe care serve doar o apelează.

(define (serve requests fast-counters slow-counters)
  (define (serve-helper requests fast-counters slow-counters result)
    (define (remove-clients x result)
      (λ(C)
        (cond ((< x 0) (cons C result))
              ((queue-empty? (counter-queue C)) (cons ((pass-time-through-counter x) C) result))
              ((<= (counter-et C) x) ((remove-clients (- x (counter-et C))
                                                      (cons (cons (- x (counter-et C))
                                                                  (cons (counter-index C)
                                                                        (car (top (counter-queue C)))))
                                                            result))
                                      (remove-first-from-counter C)))
              (else ((remove-clients -1 result) ((pass-time-through-counter x) C))))))

    (define (compute-sum-tt counters)
      (foldl (λ(C acc)
               (+ (counter-tt C) acc))
             0
             counters))

    (define (compute-average-tt sum length)
      (/ sum length))

    (define (ensure-average fast-counters slow-counters average)
      (if (>= average (compute-average-tt (compute-sum-tt (append fast-counters slow-counters))
                                          (length (append fast-counters slow-counters))))
          slow-counters
          (ensure-average fast-counters
                          (append slow-counters
                                  (list (empty-counter (+ 1 (length (append fast-counters slow-counters))))))
                          average)))
    (if (null? requests)
        (cons result (append fast-counters slow-counters))
        (match (car requests)
          [(list 'ensure average)
           (serve-helper (cdr requests)
                         fast-counters
                         (ensure-average fast-counters slow-counters average)
                         result)]
          [(list name n-items)
           (if (<= n-items ITEMS)
               (serve-helper (cdr requests)
                             (update (add-to-counter name n-items)
                                     fast-counters
                                     (car (min-tt (append fast-counters slow-counters))))
                             (update (add-to-counter name n-items)
                                     slow-counters
                                     (car (min-tt (append fast-counters slow-counters))))
                             result)
               (serve-helper (cdr requests)
                             fast-counters
                             (update (add-to-counter name n-items)
                                     slow-counters
                                     (car (min-tt slow-counters)))
                             result))]
          [(list 'delay index minutes)
           (serve-helper (cdr requests)
                         (update (tt+ minutes) (update (et+ minutes) fast-counters index) index)
                         (update (tt+ minutes) (update (et+ minutes) slow-counters index) index)
                         result)]
          [x (serve-helper (cdr requests)
                           (map car (map (remove-clients x null) fast-counters))
                           (map car (map (remove-clients x null) slow-counters))
                           (append result
                                   (map cdr (mergesort (apply append (filter (λ(L)
                                                                               (not (null? L)))
                                                                             (map cdr (map (remove-clients x null)
                                                                                           (append fast-counters slow-counters)))))
                                                       comp-minutes))))])))
  (serve-helper requests fast-counters slow-counters null))

