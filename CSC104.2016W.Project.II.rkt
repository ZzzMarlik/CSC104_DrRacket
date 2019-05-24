;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname CSC104.2016W.Project.II) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#| A Program to Model and Visualize the Spread of an Infection. |#

; Do all the tasks below marked by a “★”.


(require picturing-programs)

; These are some parameters that do not change while the program is running. 
;   Experiment with them once you have the program working.
;
; VIRULENCE: how likely disease spreads from person to person.
; DURATION : how many days an infection lasts.
; IMMUNITY : how many days of immunity after infection ends.
(define VIRULENCE 35)
(define DURATION 30)
(define IMMUNITY 20)

#| ★ AFTER YOU HAVE IMPLEMENTED THE REST OF THE PROGRAM, run it with various values
    of VIRULENCE, DURATION and IMMUNITY.

   Try to find values of those parameters that produce a delicate balance of the spread:
    values where running it a few times leads to to visibly different end results due
    to the randomness.
   Also try large and small values and view the results.
   Write a couple of paragraphs here summarizing your findings:

; First of all, let's talk about the variable VIRULENCE that approximately,
describe the speed of the disease spreads around world besides the
probability(how likely) of spreading the disease. We find that if
the value is very big, the disease will spread only one time with high
speed and left with full green. It means the disease did spread in
the world but finally disappear due to the immunity that human have. 
And if it is very small, the disease would be slowly spread or even
can't spread.

Talking about DURATION, this variable tells us how strong the disease is,
and maximum days it could survive. As we increase the number of DURATION,
the area of the red dots will be larger, and if it is big enough (much
greater than IMMUNITY) we will find that everyone in the world would be
living under disease for a very long time. It's interesting to see that if
the value of DURATION is half of the IMMUNITY or lower, the disease will be
dead in the end.

As for IMMUNITY, it works kind of like DURATION but in the opposite
way. Therefore, when we increase the IMMUNITY or it is big enough, which is
much larger than DURATION, the disease will spread one time but as the
people got immunity, the disease will not spread any more. We can regard it
as the walls that isolate the disease from people. And when it becomes
tiny, the disease would attack people without any obstructions.

In conclusion, we find the set {VIRULENCE=35, DURATION=30, IMMUNITY=20}
could be produce a delicate balance of the spread, which makes a circulation
of different infection status, and it will give us different result by
the randomness. And this project mainly describe how those three variables
change the ability of the disease and gives us a idea of how will disease
spread in out real life. (I love this project so much because it's looks
like a part of the game "Plague")


|#


; WIDTH : number of dots wide the simulation image is.
; HEIGHT: number of dots high the simulation image is.
(define WIDTH 100)
(define HEIGHT 100)


; take : list number -> list
(check-expect (take (list 1 2 3 4 5) 3) (list 1 2 3))
; Produce prefix of length n of L.
(define (take L n)
  (cond [(zero? n) (list)]
        [else (list* (first L) (take (rest L) (- n 1)))]))

; drop : list number -> list
(check-expect (drop (list 1 2 3 4 5) 3) (list 4 5))
(check-expect (drop (list 1 2 3) 3) (list))
; Produce suffix of L after removing n elements.
(define (drop L n)
  (cond [(<= (length L) n) (list)]
        [(zero? n) L]
        [else (drop (rest L) (- n 1))]))

; left-cycle : list number -> list
(check-expect (left-cycle (list 1 2 3 4) 1) (list 2 3 4 1))
(check-expect (left-cycle (list 1 2 3 4) 2) (list 3 4 1 2))
;
; ★ Write a full design check-expect for left-cycle:
(check-expect (left-cycle (list 4 5 6 7) 3)
              (cond [(zero? 3) (list 4 5 6 7)]
                    [else (append (drop (list 4 5 6 7) 3)
                                  (take (list 4 5 6 7) 3))]))


; ★ Fix the body of left-cycle based on your full design check-expect.
; HINT: use take and drop.
; Produce a list with the first n elements of L after the others.
(define (left-cycle L n)
  (cond [(zero? n) L]
        [else (append (drop L n) (take L n))]))

; right-cycle : list number -> list
(check-expect (right-cycle (list 1 2 3 4) 1) (list 4 1 2 3))
(check-expect (right-cycle (list 1 2 3 4) 2) (list 3 4 1 2))
; Produce a list with the last n elements of L before the others.
(define (right-cycle L n)
  (reverse (left-cycle (reverse L) n)))


#| Infection Status.

 A person is either:

   Infected, with a certain number of days of infection left.
     This is represented by a number which is the *negative* of the number
      of days of infection left.

   Immune, with a certain number of days of immunity left.
     This is represented by a *positive* number which is the number of days
      of immunity left.

   Infectable.
     This is represented by zero. |#

; INITIAL-STATUSES is the list of initial infection statuses of everyone.
(define INITIAL-STATUSES
  (local [(define DOTS (* WIDTH HEIGHT))
          (define DOTS/2 (quotient DOTS 2))]
    (append (make-list DOTS/2 0)
            (list (- DURATION)) ; One infected person.
            (make-list (- DOTS (+ 1 DOTS/2)) 0))))


; infect : integer integer integer integer integer -> integer
(check-expect (infect -1  0 -2  3 -4) -1)
(check-expect (infect  2  0  3 -4  5)  2)
(check-expect (infect  2  0  3 -4  5)  2)
(check-expect (infect  0  1  0  2  0)  0)
; Due to the randomness, no check-expect is available for the case where
;  infect should produce a number representing DURATION days of infection.

; ★ Fix the body of infect.
; Produce the number representing an infection of DURATION days if:
;   the subject number represents infectable, and
;   neighbour-0 or neighbour-1 or neighbour-2 or neighbour-3 represents infected, and
;   VIRULENCE is more than the randomly chosen number (random 100).
; Otherwise: produce the number representing the subject's current status.
(define (infect subject neighbour-0 neighbour-1 neighbour-2 neighbour-3)
  (cond [(and (zero? subject)
              (or (< neighbour-0 0) (< neighbour-1 0) (< neighbour-2 0)
                  (< neighbour-3 0))
              (< (random 100) VIRULENCE)) (- DURATION)]
        [else subject]))

; update-status : integer -> integer
(check-expect (update-status 15) 14)
(check-expect (update-status -1) IMMUNITY)
(check-expect (update-status 0) 0)
(check-expect (update-status -4) -3)

; ★ Fix the body of update-status.
; Produce updated duration status of infection and immunity from current infection status.
; The status remains infectable if it is infectable.
; If the status represents one day of infection, the result is IMMUNITY days of immunity
; Otherwise the result represents one less day of infection or immunity.
(define (update-status status)
  (cond [(= status 0) 0]
        [(= status -1) IMMUNITY]
        [(> status 0) (- status 1)]
        [else (+ status 1)]))

; update-statuses : list -> list
(check-expect (update-statuses (list 5 0 -7 0 4 0)) (list 4 0 -6 0 3 0))
; ★ Write a full design check-expect for update-statuses.
(check-expect (update-statuses (list 5 0 -7 0 4 0))
              (map update-status (list 5 0 -7 0 4 0)))


; ★ Fix the body of update-statuses.
; Produce the list of updated duration of infection and immunity form a list of statuses.
(define (update-statuses statuses)
  (map update-status statuses))



; day-tick : list -> list
; Produce the next day's infection status for each subject from their neighbours.
(define (day-tick statuses)
  (update-statuses (map infect
                        statuses
                        (left-cycle statuses 1)
                        (right-cycle statuses 1)
                        (left-cycle statuses WIDTH)
                        (right-cycle statuses WIDTH))))


; status-color : number -> color
(check-expect (status-color 0) (make-color 0 255 0))
(check-expect (status-color 5) (make-color 0 0 255))
(check-expect (status-color -7) (make-color 255 0 0))

; ★ Fix the body of status-color according to the check-expects.
; Produce a colour corresponding to numerical infection status.
(define (status-color status)
  (cond [(zero? status) (make-color 0 255 0)]
        [(> status 0) (make-color 0 0 255)]
        [(< status 0) (make-color 255 0 0)]))

; draw-statuses : list -> image
; Produce rectangular image depicting statuses.
(define (draw-statuses statuses)
  (color-list->bitmap (map status-color statuses) WIDTH HEIGHT))

; start the universe!
; ★ Uncomment this:
(big-bang INITIAL-STATUSES
          (on-tick day-tick)
          (on-draw draw-statuses))