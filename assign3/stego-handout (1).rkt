#lang racket

;(require rsound)
(define frame-rate 44100)
(define volume 0.1)

; A table of notes and their corresponding frequency;
; Note C1 is the C in the 1st octave.
(define notes 
'(
   (C0 16.35)
   (C#0 17.32)
   (Db0 17.32)
   (D0 18.35)
   (D#0 19.45)
   (Eb0 19.45)
   (E0 20.60)
   (F0 21.83)
   (F#0 23.12)
   (Gb0 23.12)
   (G0 24.50)
   (G#0 25.96)
   (Ab0 25.96)
   (A0 27.50)
   (A#0 29.14)
   (Bb0 29.14)
   (B0 30.87)
   (C1 32.70)
   (C#1 34.65)
   (Db1 34.65)
   (D1 36.71)
   (D#1 38.89)
   (Eb1 38.89)
   (E1 41.20)
   (F1 43.65)
   (F#1 46.25)
   (Gb1 46.25)
   (G1 49.00)
   (G#1 51.91)
   (Ab1 51.91)
   (A1 55.00)
   (A#1 58.27)
   (Bb1 58.27)
   (B1 61.74)
   (C2 65.41)
   (C#2 69.30)
   (Db2 69.30)
   (D2 73.42)
   (D#2 77.78)
   (Eb2 77.78)
   (E2 82.41)
   (F2 87.31)
   (F#2 92.50)
   (Gb2 92.50)
   (G2 98.00)
   (G#2 103.83)
   (Ab2 103.83)
   (A2 110.00)
   (A#2 116.54)
   (Bb2 116.54)
   (B2 123.47)
   (C3 130.81)
   (C#3 138.59)
   (Db3 138.59)
   (D3 146.83)
   (D#3 155.56)
   (Eb3 155.56)
   (E3 164.81)
   (F3 174.61)
   (F#3 185.00)
   (Gb3 185.00)
   (G3 196.00)
   (G#3 207.65)
   (Ab3 207.65)
   (A3 220.00)
   (A#3 233.08)
   (Bb3 233.08)
   (B3 246.94)
   (C4 261.63)
   (C#4 277.18)
   (Db4 277.18)
   (D4 293.66)
   (D#4 311.13)
   (Eb4 311.13)
   (E4 329.63)
   (F4 349.23)
   (F#4 369.99)
   (Gb4 369.99)
   (G4 392.00)
   (G#4 415.30)
   (Ab4 415.30)
   (A4 440.00)
   (A#4 466.16)
   (Bb4 466.16)
   (B4 493.88)
   (C5 523.25)
   (C#5 554.37)
   (Db5 554.37)
   (D5 587.33)
   (D#5 622.25)
   (Eb5 622.25)
   (E5 659.25)
   (F5 698.46)
   (F#5 739.99)
   (Gb5 739.99)
   (G5 783.99)
   (G#5 830.61)
   (Ab5 830.61)
   (A5 880.00)
   (A#5 932.33)
   (Bb5 932.33)
   (B5 987.77)
   (C6 1046.50)
   (C#6 1108.73)
   (Db6 1108.73)
   (D6 1174.66)
   (D#6 1244.51)
   (Eb6 1244.51)
   (E6 1318.51)
   (F6 1396.91)
   (F#6 1479.98)
   (Gb6 1479.98)
   (G6 1567.98)
   (G#6 1661.22)
   (Ab6 1661.22)
   (A6 1760.00)
   (A#6 1864.66)
   (Bb6 1864.66)
   (B6 1975.53)
   (C7 2093.00)
   (C#7 2217.46)
   (Db7 2217.46)
   (D7 2349.32)
   (D#7 2489.02)
   (Eb7 2489.02)
   (E7 2637.02)
   (F7 2793.83)
   (F#7 2959.96)
   (Gb7 2959.96)
   (G7 3135.96)
   (G#7 3322.44)
   (Ab7 3322.44)
   (A7 3520.00)
   (A#7 3729.31)
   (Bb7 3729.31)
   (B7 3951.07)
   (C8 4186.01)
   (C#8 4434.92)
   (Db8 4434.92)
   (D8 4698.63)
   (D#8 4978.03)
   (Eb8 4978.03)
   (E8 5274.04)
   (F8 5587.65)
   (F#8 5919.91)
   (Gb8 5919.91)
   (G8 6271.93)
   (G#8 6644.88)
   (Ab8 6644.88)
   (A8 7040.00)
   (A#8 7458.62)
   (Bb8 7458.62)
   (B8 7902.13)
   (pause 20000)
))

; (note->freq n) returns the frequency (from the
; notes table above for the note n. (note->freq A#8)
; should return 7458.62, for example. If the lookup
; fails, generate the error message "no such note".
(define (note->freq n)
   (note->freqH n notes)    
    ; ((equal? 
  )

(define (note->freqH n notesL)
  (cond
    ((null? notesL) (error "no such note"))
    ((equal? (caar notesL) n) (display (cadar notesL)))
    (else (note->freqH n (cdr notesL)))
    )
  )

; (freq->note n) returns the note that corresponds
; approximately, to frequency n, by doing a reverse
; lookup in the notes table able. n is a floating
; point number. Equality should be within 0.1 (exclusive) of
; the value in the notes table. If the lookup fails,
; return the symbol 'NO-NOTE. For example,
; (freq->note 7040.00), (freq->note 7040.09), and
; (freq->note 7039.91) should all return A8, but
; (freq->note 7040.1) should return 'NO-NOTE.
(define (freq->note n)
   (freq->noteH n notes)
  )

(define (freq->noteH n notesL)
  (cond
    ((null? notesL) (quote NO-NOTE))
    ((and (< (- n .1) (cadar notesL)) (>= (+ n .1) (cadar notesL)) (caar notesL))) 
    (else (freq->noteH n (cdr notesL)))
    )
  )

; Symbolic representation of the riff from funky town.
(define funky-town
  '(
   (C5 1/8) (C5 1/8) (Bb4 1/8) (C5 1/8)
   (pause 1/8) (G4 1/8) (pause 1/8) (G4 1/8)
   (C5 1/8) (F5 1/8) (E5 1/8) (C5 1/8)
   (pause 1/2)
   ))

; Turn the symbolic representation of a song into a
; list of (frequency duration) pairs. For example,
; eval-song funky-town) returns '((523.25 5513)
; (523.25 5513) (466.16 5513) (523.25 5513) ... (1 22050))
(define (eval-song song)
  (define (eval-note f duration)
     (list (note->freq f) (ceiling (* duration frame-rate)))
    )
  (cond
       ((null? song) '())
       (else (cons
              (eval-note (caar song) (cadar song))
              (eval-song (cdr song))))))

; Use the rsound package to turn a list of
; (frequency duration) pairs into rsound tones.
; For example, > (song->rsound (eval-song funky-town))
; returns '((rsound #<s16vector> 0 5513 44100.0)...)
;(define (song->rsound song)
 ; (cond
  ;     ((null? song) '())
   ;    (else (cons
    ;          (make-tone (caar song) volume (cadar song))
     ;         (song->rsound (cdr song)))
      ;       )))

; Play a song, for example, (play-song funky-town).
;(define (play-song song)
 ;  (play (rs-append* (song->rsound (eval-song song)))))

; (string->bits s), where s is a character string, returns
; a list of the ASCII bits of s, where bits are represented
; by the numbers 0 and 1. Since each ASCII character is 8 bits,
; a string of length n will return a list of length 8*n of
; the numbers 0 and 1. For example, (string->bits "ab")
; returns '(0 1 1 0 0 0 0 1 0 1 1 0 0 0 1 0).
(define (string->bits s)
  (define (pad-to-8 x)
    (append (take '(0 0 0 0 0 0 0 0) (- 8 (length x))) x)
    )
(define (char->bits c)
    (map
       (lambda (x) (- (char->integer x) 48))
          (string->list (number->string (char->integer c) 2))
    )
  )
  (foldl
      (lambda (c result)
         (append result (pad-to-8 (char->bits c))))
       '()
       (string->list s))
  )

; (bits->string s), where s is a list of 0/1 bits of
; length n*8, returns a character string t, where
; every 8 bits of s is interpreted as an ASCII
; character in t. For example,
; (bits->string (string->bits "ab")) returns "ab".
(define (bits->string s)
  (define (split-by-8 s)
  (cond
    ((null? s) '())
    (else (cons (take s 8) (split-by-8 (drop s 8)))))
  )
  (define (bits->char b)
    (integer->char
       (string->number
           (list->string
               (map (lambda (c)
                  (integer->char (+ 48 c))) b)
            )
           2)
       )
    )
    (list->string (map bits->char (split-by-8 s)))
  )

; (embed-bits all-notes bits offset), where all-notes is a
; list of (frequency duration) pairs, bits a
; list of 0/1 bits to be embeded, and offset is an integer,
; returns a new list of (frequency duration) pairs, such that
;   * if the current bit=0, and the current
;     (frequency duration) pair is (f d), replace
;     (f d) with ((- f offset) d).
;   * if the current bit=1, and the current
;     (frequency duration) pair is (f d), replace
;     (f d) with ((+ f offset) d).
;   * otherwise make no changes to (f d).
; If all-notes (the song into which the bits are embedded)
; runs out before all the bits have been embedded, then
; all-notes is repeated.
; If bits runs out before all notes in all-notes has been
; consumed, no further changes are made to the remaining
; notes (i.e. they are not discarded, and not changed).
;(define (embed-bits all-notes bits offset)
   ; fill this in
;)

; (extract-bits remaining-notes offet), where
; remaining-notes is a list of (frequency duration)
; paris, and offset an integer, returns a list of
; 0/1 bits, extracted from the notes. If 
; (- frequency offset) is a valid note from the notes
; table above (within a 0.1 bound) then a 1 is generated,
; If (+ frequency offset) is a valid note from the notes
; table above (within a 0.1 bound) then a 0 is generated,
; otherwise no bit is generated.
;(define (extract-bits remaining-notes offset)
   ; fill this in
;)

; (send song msg offset) where msg is a string and offset
; an integer, embeds the bits from msg into the song.
;(define (send song msg offset)
 ;  (embed-bits
  ;     (eval-song song)
   ;    (string->bits msg)
    ;   offset))

; (receive msg offset) where msg is a list of (frequency
; duration) paris, and offset an integer, extracts the
; embedded bits from msg and converts to the original
; string message.
;(define (receive msg offset)
 ;  (bits->string (extract-bits msg offset)))

; Test if embedding then extracting a message returns
; the same value.
;(define (test song msg offset)
 ;  (equal? (receive (send song msg offset) offset) msg)
  ;)

; Embed a message in a song and play it.
;(define (play-msg song msg offset)
 ;  (play (rs-append* (song->rsound (send song msg offset)))))

(require racket/trace)
;(trace freq->note)
;(trace freq->noteH)
