#lang racket

(module+ main
  (require raylib/2d/unsafe)

  (struct state (snake apple last-move-x last-move-y))

  (define board-size 20)
  (define tile-size 16)
  (define window-size (* board-size tile-size))
  (define snake-color (make-Color 0 255 0 255))
  (define apple-color (make-Color 255 0 0 255))

  (SetTargetFPS 10)
  (InitWindow window-size window-size "Snake")

  (define (map-two f cell)
    (cond
      [(null? cell) null]
      [(null? (cdr cell)) null]
      [else (cons (f (car cell) (car (cdr cell))) (map-two f (cdr cell)))]))

  (define (move-snake snake move-x move-y)
    (let ([snake-x (vector-ref (car snake) 0)] [snake-y (vector-ref (car snake) 1)])
      (cons (vector (modulo (+ snake-x move-x) board-size) (modulo (+ snake-y move-y) board-size))
            (map-two (lambda (x _) x) snake))))

  (define (grow-snake snake)
    (append snake (list (last snake))))

  (define (is-snake-colliding snake apple)
    (foldr (lambda (seg acc) (or (equal? seg apple) acc)) false snake))

  (define (loop last-state)
    (when (not (WindowShouldClose))
      (BeginDrawing)
      (ClearBackground RAYWHITE)

      (define last-snake (state-snake last-state))
      (define last-apple (state-apple last-state))
      (define last-move-x (state-last-move-x last-state))
      (define last-move-y (state-last-move-y last-state))

      (for-each (lambda (seg)
                  (let ([x (vector-ref seg 0)] [y (vector-ref seg 1)])
                    (DrawRectangle (* x tile-size) (* y tile-size) tile-size tile-size snake-color)))
                last-snake)

      (let ([x (vector-ref last-apple 0)] [y (vector-ref last-apple 1)])
        (DrawRectangle (* x tile-size) (* y tile-size) tile-size tile-size apple-color))

      (EndDrawing)

      (define input-left (if (IsKeyDown KEY_LEFT) 1 0))
      (define input-right (if (IsKeyDown KEY_RIGHT) 1 0))
      (define input-x (- input-right input-left))
      (define input-up (if (IsKeyDown KEY_UP) 1 0))
      (define input-down (if (IsKeyDown KEY_DOWN) 1 0))
      (define input-y (- input-down input-up))
      (define got-no-input (and (zero? input-x) (zero? input-y)))
      (define move-x (if got-no-input last-move-x input-x))
      (define move-y
        (cond
          [(not (zero? input-x)) 0]
          [got-no-input last-move-y]
          [else input-y]))

      (define ate-apple (is-snake-colliding last-snake last-apple))
      (define next-apple (if ate-apple (vector (random board-size) (random board-size)) last-apple))
      (define grown-snake (if ate-apple (grow-snake last-snake) last-snake))
      (define next-snake (move-snake grown-snake move-x move-y))

      (loop (state next-snake next-apple move-x move-y))))

  (loop (state '(#(0 0)) #(4 4) 1 0))

  (CloseWindow))
