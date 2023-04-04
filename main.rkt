#lang racket

(module+ main
  (require raylib/2d/unsafe)

  (struct state (snake apple last-move-x last-move-y time))

  (define board-size 20)
  (define tile-size 16)
  (define window-size (* board-size tile-size))
  (define snake-color (make-Color 0 255 0 255))
  (define apple-color (make-Color 255 0 0 255))

  (SetTargetFPS 60)
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

  (define (is-snake-colliding-with-apple snake apple)
    (foldr (lambda (seg acc) (or (equal? seg apple) acc)) false snake))

  (define (is-snake-colliding-with-self snake)
    (let ([snake-head (car snake)] [snake-tail (cdr snake)])
      (foldr (lambda (seg acc) (or (equal? seg snake-head) acc)) false snake-tail)))

  (define (make-snake)
    (let ([board-center (/ board-size 2)])
        (list (vector board-center board-center))))

  (let loop ([last-state (state (make-snake) #(4 4) 1 0 0)])
    (when (not (WindowShouldClose))
      (define last-snake (state-snake last-state))
      (define last-apple (state-apple last-state))
      (define last-move-x (state-last-move-x last-state))
      (define last-move-y (state-last-move-y last-state))
      (define last-time (state-time last-state))

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

      (BeginDrawing)
      (ClearBackground RAYWHITE)

      (for-each (lambda (seg)
                  (let ([x (vector-ref seg 0)] [y (vector-ref seg 1)])
                    (DrawRectangle (* x tile-size) (* y tile-size) tile-size tile-size snake-color)))
                last-snake)

      (let ([x (vector-ref last-apple 0)] [y (vector-ref last-apple 1)])
        (DrawRectangle (* x tile-size) (* y tile-size) tile-size tile-size apple-color))

      (EndDrawing)

      (define next-time (+ last-time (GetFrameTime)))

      (if (> next-time 0.1)
          (let ()
            (define ate-apple (is-snake-colliding-with-apple last-snake last-apple))
            (define next-apple (if ate-apple (vector (random board-size) (random board-size)) last-apple))
            (define grown-snake (if ate-apple (grow-snake last-snake) last-snake))
            (define moved-snake (move-snake grown-snake move-x move-y))
            (define next-snake (if (is-snake-colliding-with-self moved-snake) (make-snake) moved-snake))

            (loop (state next-snake next-apple move-x move-y (- next-time 0.1))))

          (loop (state last-snake last-apple move-x move-y next-time)))))

  (CloseWindow))
