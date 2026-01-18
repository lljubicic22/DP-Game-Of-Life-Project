(defpackage :game-of-life
  (:use :cl)
  (:export :create-grid :evolve-grid :add-glider :add-blinker))

(in-package :game-of-life)

(defun create-grid (rows cols)
  (make-array (list rows cols) :initial-element 0))

(defun set-cell (grid x y value)
  (setf (aref grid x y) value))

(defun count-neighbors (grid x y)
  (let ((count 0))
    (dotimes (dx 3)
      (dotimes (dy 3)
        (let ((nx (+ x (- dx 1)))
              (ny (+ y (- dy 1))))
          (when (and (not (and (= dx 1) (= dy 1)))
                     (>= nx 0) (< nx (array-dimension grid 0))
                     (>= ny 0) (< ny (array-dimension grid 1))
                     (= (aref grid nx ny) 1))
            (incf count)))))
    count))

(defun evolve-grid (grid)
  (let ((new-grid (create-grid (array-dimension grid 0)
                               (array-dimension grid 1))))
    (dotimes (x (array-dimension grid 0))
      (dotimes (y (array-dimension grid 1))
        (let ((neighbors (count-neighbors grid x y)))
          (set-cell new-grid x y
                    (cond
                      ((= (aref grid x y) 1)
                       (if (or (= neighbors 2) (= neighbors 3)) 1 0))
                      ((= neighbors 3) 1)
                      (t 0))))))
    new-grid))

(defun add-glider (grid x y)
  (let ((coords '((0 1) (1 2) (2 0) (2 1) (2 2))))
    (dolist (offset coords)
      (let ((gx (+ x (first offset)))
            (gy (+ y (second offset))))
        (when (and (>= gx 0) (< gx (array-dimension grid 0))
                   (>= gy 0) (< gy (array-dimension grid 1)))
          (set-cell grid gx gy 1))))))

(defun add-blinker (grid x y)
  (let ((coords '((0 0) (0 1) (0 2))))
    (dolist (offset coords)
      (let ((gx (+ x (first offset)))
            (gy (+ y (second offset))))
        (when (and (>= gx 0) (< gx (array-dimension grid 0))
                   (>= gy 0) (< gy (array-dimension grid 1)))
          (set-cell grid gx gy 1))))))


(ql:quickload :sdl2)

(defpackage :game-of-life-sdl
  (:use :cl :game-of-life :sdl2)
  (:export :start-game-sdl))

(in-package :game-of-life-sdl)

(defparameter *cell-size* 6)
(defparameter *window-scale* 3)
(defparameter *background-color* '(0 0 0 255))
(defparameter *cell-color* '(0 200 0 255))

(defun draw-grid-sdl (renderer grid)
  (destructuring-bind (r g b a) *background-color*
    (sdl2:set-render-draw-color renderer r g b a))
  (sdl2:render-clear renderer)

  (let* ((rows (array-dimension grid 0))
         (cols (array-dimension grid 1))
         (cell-size (* *cell-size* *window-scale*)))
    (destructuring-bind (r g b a) *cell-color*
      (sdl2:set-render-draw-color renderer r g b a))
    (dotimes (x rows)
      (dotimes (y cols)
        (when (= 1 (aref grid x y))
          (let* ((px (* y cell-size))
                 (py (* x cell-size)))
            (sdl2:render-fill-rect
             renderer
             (sdl2:make-rect px py cell-size cell-size)))))))
  (sdl2:render-present renderer))

(defun run-loop (window renderer initial-grid &key (tick-ms 200))
  (let ((grid initial-grid)
        (last-tick (sdl2:get-ticks)))
    (sdl2:with-event-loop (:method :poll)
      (:quit () t)
      (:keydown (:keysym keysym)
        (when (eql (sdl2:scancode keysym) :scancode-escape)
          (sdl2:push-event :quit)))
      (:idle ()
        (let ((now (sdl2:get-ticks)))
          (when (>= (- now last-tick) tick-ms)
            (setf grid (evolve-grid grid))
            (setf last-tick now))
          (draw-grid-sdl renderer grid)
          (sdl2:delay 10))))))

(defun start-game-sdl ()
  (let* ((rows 40)
         (cols 60)
         (grid (create-grid rows cols))
         (cell-size (* *cell-size* *window-scale*))
         (win-width (* cols cell-size))
         (win-height (* rows cell-size)))
    (add-glider grid 2 2)
    (sdl2:with-init (:video)
      (sdl2:with-window (window :title "Game of Life (SDL2)"
                                :w win-width
                                :h win-height
                                :flags '(:shown))
        (sdl2:with-renderer (renderer window :index -1 :flags '(:accelerated))
          (run-loop window renderer grid))))))

(game-of-life-sdl:start-game-sdl)
