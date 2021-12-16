#lang racket
(require racket/match)

(provide (all-defined-out))

(define ITEMS 5)

;; Actualizăm structura counter cu informația et:
;; Exit time (et) al unei case reprezintă timpul până la ieșirea primului client de la casa respectivă,
;; adică numărul de produse de procesat pentru acest client + întârzierile suferite de casă (dacă există).
;; Ex:
;; la C3 s-au așezat întâi Ana cu 3 produse și apoi Geo cu 7 produse,
;; și C3 a fost întârziată cu 5 minute => et pentru C3 este 3 + 5 = 8 (timpul până când va ieși Ana).


; Redefinim structura counter.
(define-struct counter (index tt et queue) #:transparent)


; TODO
; Actualizați implementarea empty-counter astfel încât să conțină și câmpul et.
(define (empty-counter index)
  (make-counter index 0 0 null))


; TODO
; Implementați o funcție care aplică o transformare casei cu un anumit index.
; f = funcție unară care se aplică pe un argument de tip casă, counters = listă de case,
; index = indexul casei din counters căreia îi veți aplica transformarea f
; Veți întoarce lista actualizată de case.
; Dacă nu există în counters o casă cu acest index, veți întoarce lista nemodificată.
(define (update f counters index)
  (map (lambda (C)
         (if (= (counter-index C) index)
             (f C)
             C))
       counters))


; TODO
; Memento: tt+ crește tt-ul unei case cu un număr dat de minute.
; Actualizați implementarea tt+ pentru:
; - a ține cont de noua reprezentare a unei case
; - a permite ca operații de tip tt+ să fie pasate ca argument funcției update în cel mai facil mod
; Obs: Pentru compatibilitatea cu primul argument al lui update, trebuie să obținem ușor din tt+ 
; o funcție unară care se aplică pe un argument de tip casă (v. funcții curry).
; Am eliminat parametrii lui tt+ din define-ul de mai jos astfel încât voi să determinați
; cum este cel mai bine ca tt+ să își primească parametrii.
; Din acest motiv checker-ul nu testează separat această funcție, dar asistentul va verifica
; faptul că ați implementat-o conform cerințelor.
(define (tt+ minutes)
  (λ (C) (match C
    [(counter index tt et queue)
     (struct-copy counter C [tt (+ tt minutes)])])))


; TODO
; Implementați o funcție care crește et-ul unei case cu un număr dat de minute.
; Păstrați formatul folosit pentru tt+.
; Checker-ul nu testează separat această funcție.
(define (et+ minutes)
  (λ (C) (match C
    [(counter index tt et queue)
     (struct-copy counter C [et (+ et minutes)])])))


; TODO
; Memento: add-to-counter adaugă o persoană (reprezentată prin nume și număr de produse) la o casă. 
; Actualizați implementarea add-to-counter pentru aceleași rațiuni pentru care am modificat tt+.
; Atenție la cum se modifică tt și et!
; Checker-ul nu testează separat această funcție.
(define (add-to-counter name n-items) 
  (λ (C) (if (null? (counter-queue C))
          (match C
            [(counter index tt et queue)
             (struct-copy counter C [tt (+ tt n-items)]
                          [et (+ et n-items)]
                          [queue (append queue (list (cons name n-items)))])])
          (match C
            [(counter index tt et queue)
             (struct-copy counter C [tt (+ tt n-items)]
                          [queue (append queue (list (cons name n-items)))])]))))


; TODO
; Întrucât vom folosi atât min-tt (implementat în etapa 1) cât și min-et (funcție nouă),
; definiți-vă o funcție mai abstractă din care să derive ușor atât min-tt cât și min-et.
; Prin analogie cu min-tt, definim min-et astfel:
; min-et = funcție care primește o listă de case și intoarce o pereche dintre:
; - indexul casei (din listă) care are cel mai mic et
; - et-ul acesteia
; (când mai multe case au același et, este preferată casa cu indexul cel mai mic)
(define (f-counter-tt C)
  (match C [(counter index tt et queue) tt]))

(define (f-counter-et C)
  (match C [(counter index tt et queue) et]))

; functie care calculeaza elementul minim dintr-o lista de case, criteriul de comparatie
; fiind functia data ca parametru
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


; TODO
; Implementați o funcție care scoate prima persoană din coada unei case.
; Funcția presupune, fără să verifice, că există minim o persoană la coada casei C.
; Veți întoarce o nouă structură obținută prin modificarea cozii de așteptare.
; Atenție la cum se modifică tt și et!
; Dacă o casă tocmai a fost părăsită de cineva, înseamnă că ea nu mai are întârzieri.
(define (remove-first-from-counter C)
  (if (= 1 (length (counter-queue C)))
      (match C
        [(counter index tt et queue)
         (struct-copy counter C [tt (- tt et)]
                  [et 0]
                  [queue null])])
      (match C
        [(counter index tt et queue)
         (struct-copy counter C [tt (- tt et)]
                  [et (cdr (cadr queue))]
                  [queue (cdr queue)])])))

; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 1, funcția serve operează cu următoarele modificări:
; - nu mai avem doar 4 case, ci:
;   - fast-counters (o listă de case pentru maxim ITEMS produse)
;   - slow-counters (o listă de case fără restricții)
;   (Sugestie: folosiți funcția update pentru a obține comportamentul pe liste de case)
; - requests conține 4 tipuri de cereri (cele 2 din etapa anterioară plus 2 noi):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute
;   - (remove-first) - cea mai avansată persoană părăsește casa la care se află
;   - (ensure <average>) - cât timp tt-ul mediu al tuturor caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)
; Sistemul trebuie să proceseze cele 4 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)
;   (ca înainte, dar acum folosiți fast-counters și slow-counters)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți)
;   (puțin diferit față de etapa 1, și acum folosiți fast-counters și slow-counters)
; - persoana care părăsește supermarketul este persoana fruntașă la casa cu et-ul minim
;   (dintre casele care au clienți); dacă nicio casă nu are clienți, cererea e ignorată
; - tt-ul mediu (ttmed) se calculează pentru toate casele (și cele fast, și cele slow), 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>
; Considerați că indecșii caselor încep de la 1 și sunt mereu în ordine.
; Ex:
; fast-counters conține casele 1-2, slow-counters conține casele 3-15
; => la nevoie veți adăuga întâi casa 16, apoi casa 17, etc.
; (puteți determina matematic de câte case noi este nevoie sau
;  puteți adăuga recursiv una câte una până când condiția este îndeplinită)


(define (serve requests fast-counters slow-counters)
  ; functie care primeste o lista de case si intoarce o lista cele care nu au coada goala
  (define (not-null-queue C)
      (not (null? (counter-queue C))))
  
  ; functie care face suma tt-urilor caselor din lista data ca parametru
  (define (compute-sum-tt counters)
    (if (null? counters)
        0
        (+ (counter-tt (car counters)) (compute-sum-tt (cdr counters)))))

  ; functie care face media tt-urilor
  (define (compute-average-tt sum length)
    (/ sum length))
  
 ; functie prin care se adauga case cat timp tt-ul mediu este mai mare decat media data ca parametru
  (define (ensure-average fast-counters slow-counters average)
    (if (>= average (compute-average-tt (compute-sum-tt (append fast-counters slow-counters))
                                        (length (append fast-counters slow-counters))))
        slow-counters
        (ensure-average fast-counters
                        (append slow-counters (list (empty-counter (+ 1 (length (append fast-counters slow-counters))))))
                        average)
        ))

  (define (remove-first-helper counters)
      (car (min-et (filter not-null-queue counters))))
  
  (if (null? requests)
      (append fast-counters slow-counters)
      (match (car requests)
        [(list 'ensure average)
         (serve (cdr requests)
                fast-counters
                (ensure-average fast-counters slow-counters average))]
        
        [(list name n-items)
         (if (<= n-items ITEMS)
             (serve (cdr requests)
                    (update (add-to-counter name n-items)
                            fast-counters
                            (car (min-tt (append fast-counters slow-counters))))
                    (update (add-to-counter name n-items)
                            slow-counters
                            (car (min-tt (append fast-counters slow-counters)))))
             (serve (cdr requests)
                    fast-counters
                    (update (add-to-counter name n-items)
                            slow-counters
                            (car (min-tt slow-counters)))))]
        
        [(list 'delay index minutes)
         (serve (cdr requests)
                (update (tt+ minutes) (update (et+ minutes) fast-counters index) index)
                (update (tt+ minutes) (update (et+ minutes) slow-counters index) index))]
        
        [(list 'remove-first)
         (if (null? (filter not-null-queue (append fast-counters slow-counters)))
             (serve (cdr requests) fast-counters slow-counters)
             (serve (cdr requests)
                (update remove-first-from-counter
                        fast-counters
                        (remove-first-helper (append fast-counters slow-counters)))
                (update remove-first-from-counter
                        slow-counters
                        (remove-first-helper (append fast-counters slow-counters)))))])))

