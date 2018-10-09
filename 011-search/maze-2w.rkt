;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname maze-2w) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Solve simple square mazes

;; maze-v1.rkt


;; Constants:

;; Data definitions:

;; Maze is (listof Boolean)
;; interp. a square maze
;;         each side length is (sqrt (length <maze>))
;;         true  (aka #t) means open, can move through this
;;         false (aka #f) means a wall, cannot move into or through a wall
;;

(define O #t) ;Open
(define W #f) ;Wall

(define M0
  (list O W W W
        W W W W
        W W W W
        W W W W))

(define M1
  (list O W W W W
        O O W O O
        W O W W W 
        O O W W W
        O O O O O))

(define M2
  (list O O O O O
        O W W W O
        O W W W O
        O W W W O
        O W W W O))

(define M3            
  (list O O O O O
        O W W W W
        O W W W W
        O W W W W 
        O O O O O))

(define M4
  (list O O O O O
        O W W W O
        O W O O O
        O W O W W
        W W O O O))

(define-struct pos (x y))
;; Pos is (make-pos Integer Integer)
;; interp. an x, y position in the maze.
;;         0, 0 is upper left.
;;         a position is only valid for a given maze if:
;;            - (<= 0 x (sub1 <size>))
;;            - (<= 0 y (sub1 <size>))
;;            - there is a true in the given cell
;;
(define P0 (make-pos 0 0)) ;upper left  in 4x4 maze
(define P1 (make-pos 3 0)) ;upper right  "  "   "
(define P2 (make-pos 0 3)) ;lower left   "  "   "
(define P3 (make-pos 3 3)) ;lower left   "  "   "


;; Functions:

;; Maze -> Boolean
;; produces true if the maze is solvable, else otherwise. A maze is solvable if you can start at list(0, 0) and go to list(length, length)
;; Assume: you can only move right or down one square at time

; (define (solution? m) false) ;Stub

(check-expect (solution? M0) false)
(check-expect (solution? M1)  true)
(check-expect (solution? M2)  true)
(check-expect (solution? M3)  true)
(check-expect (solution? M4) false)


(define (solution? x)
  (local [(define (helper mz p)
            (if (won? mz p)
                (won? mz p)
                (helper-mzs mz (next p mz))))

          (define (helper-mzs mz lops)
            (cond [(empty? lops) false]
                  [else
                   (local [(define try (helper mz (first lops)))] 
                     (if (not (false? try))                     
                         try                                    
                         (helper-mzs mz (rest lops))))]))]
    (helper x P0)))

;; WON
;; Maze Pos -> Boolean
;; produes true if pos-x and pos-y are equal (sqrt (lenght mz))

(define (won? mz p)
  (and (= (- (sqrt (length mz)) 1) (pos-x p))
       (= (- (sqrt (length mz)) 1) (pos-y p))))

;; NEXT
;; Maze Pos -> (listOf Pos)
;; produces a list with next valid positions

; (define (next pos mz) (list pos)) ;Stub

(check-expect (next P0 M0) empty)
(check-expect (next P1 M2) (list (make-pos 4 0)))
(check-expect (next P0 M2) (list (make-pos 1 0) (make-pos 0 1)))
(check-expect (next P2 M3) (list (make-pos 0 4)))

(define (next p mz)
  (filter (lambda (p)
            (and (in-maze? mz p) (mref mz p))) (next-paths p)))

;; NEXT-PATHS
;; Pos -> (listOf Pos)
;; produces a list of pos, where each element will be a pos going down and right

; (define (next-paths pos) empty) ;Stub

(check-expect (next-paths P0)
              (list (make-pos 1 0)
                    (make-pos 0 1)))
(check-expect (next-paths P1)
              (list (make-pos 4 0)
                    (make-pos 3 1)))

(define (next-paths pos)
  (list (make-pos (+ 1 (pos-x pos)) (pos-y pos))
        (make-pos (pos-x pos) (+ 1 (pos-y pos)))))

;; IN-MAZE
;; Maze Pos -> Boolean
;; returns true if pos is in maze

; (define (in-maze? mz p) false) ;Stub

(check-expect (in-maze? M0 P0) true)
(check-expect (in-maze? M0 (make-pos 5 5)) false)

(define (in-maze? mz p)
  (and (< (pos-x p) (sqrt (length mz)))
       (< (pos-y p) (sqrt (length mz)))))

;; Maze Pos -> Boolean
;; produce contents of given square in given maze
(check-expect (mref (list #t #f #f #f) (make-pos 0 0)) #t)
(check-expect (mref (list #t #t #f #f) (make-pos 0 1)) #f)

(define (mref m p)
  (local [(define s (sqrt (length m))) ;each side length
          (define x (pos-x p))
          (define y (pos-y p))]
    
    (list-ref m (+ x (* y s)))))

