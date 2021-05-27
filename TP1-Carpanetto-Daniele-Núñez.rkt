;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname TP1-Carpanetto-Daniele-Núñez) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
#|
Trabajo Práctico 1: Programas interactivos con estructuras

Integrantes:
- [Carpanetto, Agustín Luciano].
- [Daniele, Nicolás].
- [Núñez, Jeremías].
|#


#|
Estructura para representar el estado del programa.
- ball-x:  La posición en x de la pelota.
- ball-y:  La posición en y de la pelota.
- bar-y:   La posición en y de la barra.
- ball-vx: La componente x de la velocidad de la pelota.
- ball-vy: La componente y de la velocidad de la pelota.
- points:  La cantidad de puntos acumulados.
|#

(define-struct st [ball-x ball-y bar-y ball-vx ball-vy points])

; Alto de la escena
(define HEIGHT 400)

; Ancho de la escena
(define WIDTH 500)

; Estado inicial
(define START (make-st 100 100 (/ HEIGHT 2) 6 -6 0))

#|
Constantes asociadas a la barra
- BAR-VEL:    La velocidad de la barra.
- BAR-X:      La posición en x de la barra.
- BAR-HEIGHT: Altura de la barra.
- BAR-WIDTH:  Ancho de la barra.
- BAR:        Imagen de la barra.
|#
(define BAR-VEL 8)
(define BAR-X 20)
(define BAR-HEIGHT 50)
(define BAR-WIDTH 20)
(define BAR (rectangle BAR-WIDTH BAR-HEIGHT "solid" "red"))

#|
Constantes asociadas a la pelota
- BALL-RADIUS: Radio de la pelota.
- BALL:        Imagen de la pelota.
|#
(define BALL-RADIUS 20)
(define BALL (circle BALL-RADIUS "solid" "blue"))

; Fondo
(define BACKGROUND (empty-scene WIDTH HEIGHT))

; Tamano de fuente del puntaje
(define FONT-SIZE 20)

; draw-points: st -> Image
; Toma un estado y retorna la imagen para la cantidad de puntos anotados
(define (draw-points s) (text (number->string (st-points s) ) FONT-SIZE "indigo") )

; draw: st -> Image
; La función draw se encarga de dibujar el estado del sistema.
; Ubica a la pelota en la posición dada por el estado
; Ubica a la barra en su posición constante en X, y la dada por el estado en Y
; Ubica al puntaje (su límite derecho y no su centro) a 10 unidades del margen derecho.
; y a 10 unidades del margen superior (su límite superior)
; Si el puntaje y la pelota se superponen, el puntaje se debe seguir viendo
(define (draw s)
  (place-image (draw-points s) (- WIDTH 10) 10
               (place-image BALL (st-ball-x s) (st-ball-y s)
                            (place-image BAR BAR-X (st-bar-y s) BACKGROUND)
               )
  )
)

; ball-next: st -> st
; Calcula la posición de la pelota ante un nuevo click del reloj,
; haciéndola rebotar y manteniéndola en los márgenes permitidos.
; Para ello, primero la mueve con "step" y luego corrige posibles rebotes con "bounce"
(define (ball-next s)
  (bounce (step s)
  )
)

; Caso cuando la pelota choca con la pared derecha
(check-expect (ball-next (make-st 475 200 100 6 6 1)) (make-st 479 206 100 -6 6 1))
; Caso cuando la pelota choca con la barra (+ BAR-X (/ BAR-WIDTH 2) BALL-RADIUS) = 50
(check-expect (ball-next (make-st 50 200 200 -6 6 1)) (make-st 56 206 200 6 6 2))

; step: st -> st
; Mueve la pelota según su velocidad actual, sin importar los límites de la escena
(define (step s)
  (make-st
   (+ (st-ball-x s) (st-ball-vx s) )
   (+ (st-ball-y s) (st-ball-vy s) )
   (st-bar-y s) (st-ball-vx s) (st-ball-vy s) (st-points s)
  )
)

(check-expect (step (make-st 50 200 200 -6 6 1)) (make-st 44 206 200 -6 6 1))
(check-expect (step (make-st 100 200 200 6 6 1)) (make-st 106 206 200 6 6 1))
(check-expect (step (make-st 150 280 200 -6 -6 1)) (make-st 144 274 200 -6 -6 1))

; bounce: st -> st
; Hace rebotar la pelota en las paredes o la barra.
(define (bounce s)
  (bounce-y (bounce-x s)
  )
)

; add-point: st-> st
; Incrementa en 1 el puntaje
(define (add-point s)
  (make-st (st-ball-x s) (st-ball-y s) (st-bar-y s) (st-ball-vx s) (st-ball-vy s) (+ (st-points s) 1)
  )
)
      
(check-expect (add-point (make-st 1 1 1 6 6 1000) ) (make-st 1 1 1 6 6 1001) )
(check-expect (add-point (make-st 75 75 75 -6 6 1) ) (make-st 75 75 75 -6 6 2) )
(check-expect (add-point (make-st 200 200 200 -6 6 1) ) (make-st 200 200 200 -6 6 2) )

; bounce-x: st -> st
; Hace rebotar la pelota en la pared derecha o la barra.
(define (bounce-x s)
  (cond
      [(hit-bar? s)        (add-point (reflect-ball-x s (- (+ BAR-X (/ BAR-WIDTH 2) BALL-RADIUS 6) (st-ball-x s))))]
      [(hit-right-wall? s) (reflect-ball-x s (* -1 (- (st-ball-x s) (- (- WIDTH BALL-RADIUS) 1))))]
      [else s]
  )
)

(check-expect (bounce-x (make-st 56 200 200 -6 6 1) ) (make-st 56 200 200 -6 6 1) )
(check-expect (bounce-x (make-st 430 200 200 6 6 1) ) (make-st 430 200 200 6 6 1) )
(check-expect (bounce-x (make-st 115 200 200 -6 6 1) ) (make-st 115 200 200 -6 6 1) )

; hit-bar? : st -> Boolean
; Decide si la pelota colisionó con la barra
(define (hit-bar? s)
  (cond [(and (< (st-ball-x s) (+ BAR-X (/ BAR-WIDTH 2) BALL-RADIUS))
              (> (st-ball-y s) (- (st-bar-y s) (/ BAR-HEIGHT 2)))
              (< (st-ball-y s) (+ (st-bar-y s) (/ BAR-HEIGHT 2)))) #t]
        [else #f]
  )
)

(check-expect (hit-bar? (make-st (+ BAR-X (/ BAR-WIDTH 2) BALL-RADIUS) 200 200 -6 6 1) ) #f)
(check-expect (hit-bar? (make-st 40 200 200 -6 6 1) ) #t)
(check-expect (hit-bar? (make-st 115 200 200 -6 6 1) ) #f)

; hit-right-wall? 
; Decide si la pelota colisionó con la pared derecha
(define (hit-right-wall? s)
  (>= (+ (st-ball-x s) BALL-RADIUS) WIDTH)
)

(check-expect (hit-right-wall? (make-st 481 200 100 6 6 1)) #t)
(check-expect (hit-right-wall? (make-st 480 200 100 6 6 1)) #t)
(check-expect (hit-right-wall? (make-st 479 200 100 6 6 1)) #f)

; bounce-y: st -> st
; Hace rebotar la pelota en la pared superior o inferior.
(define (bounce-y s)
  (cond
      [(hit-top-wall? s) (reflect-ball-y s 6)]
      [(hit-bot-wall? s) (reflect-ball-y s -6)]
      [else s]
  )
)

(check-expect (bounce-y (make-st 56 200 200 -6 6 1) ) (make-st 56 200 200 -6 6 1) )
(check-expect (bounce-y (make-st 430 200 200 6 6 1) ) (make-st 430 200 200 6 6 1) )
(check-expect (bounce-y (make-st 115 200 200 -6 6 1) ) (make-st 115 200 200 -6 6 1) )

; hit-top-wall? 
; Decide si la pelota colisionó con la pared superior
(define (hit-top-wall? s)
  (<= (- (st-ball-y s) BALL-RADIUS) (- HEIGHT HEIGHT) )
)

(check-expect (hit-top-wall? (make-st 200 0 200 6 6 0) ) #t)
(check-expect (hit-top-wall? (make-st 200 20 200 6 6 0) ) #t)
(check-expect (hit-top-wall? (make-st 200 21 200 6 6 0) ) #f)
(check-expect (hit-top-wall? (make-st 200 200 200 6 6 0) ) #f)

; hit-bot-wall? 
; Decide si la pelota colisionó con la pared inferior
(define (hit-bot-wall? s)
  (>= (+ (st-ball-y s) BALL-RADIUS) HEIGHT)
)

(check-expect (hit-bot-wall? (make-st 200 380 200 6 6 0) ) #t)
(check-expect (hit-bot-wall? (make-st 200 379 200 6 6 0) ) #f)
(check-expect (hit-bot-wall? (make-st 200 300 200 6 6 0) ) #f)

; reflect-ball-x: st Number -> st
; Gira el sentido en x de la pelota y acomoda su posición en x, sumandole n unidades
; FUNCIÓN AUXILIAR - SE RECOMIENDA UTILIZARLA

(define (reflect-ball-x s n)
  (make-st
   (+ (st-ball-x s) n)
   (st-ball-y s)
   (st-bar-y s)
   (* -1 (st-ball-vx s) )
   (st-ball-vy s)
   (st-points s)
  )
)

(check-expect (reflect-ball-x (make-st 200 200 200 6 6 0) 6) (make-st 206 200 200 -6 6 0) )
(check-expect (reflect-ball-x (make-st 450 200 200 6 6 0) 6) (make-st 456 200 200 -6 6 0) )
(check-expect (reflect-ball-x (make-st 122 200 200 -6 6 0) 6) (make-st 128 200 200 6 6 0) )

; reflect-ball-y: st Number -> st
; Gira el sentido en y de la pelota y acomoda su posición en y, sumandole n unidades
; FUNCIÓN AUXILIAR - SE RECOMIENDA UTILIZARLA

(define (reflect-ball-y s n)
  (make-st
   (st-ball-x s)
   (+ n (st-ball-y s) )
   (st-bar-y s)
   (st-ball-vx s)
   (* -1 (st-ball-vy s) )
   (st-points s)
  )
)

(check-expect (reflect-ball-y (make-st 200 200 200 6 6 0) 6) (make-st 200 206 200 6 -6 0) )
(check-expect (reflect-ball-y (make-st 200 478 200 6 6 0) 6) (make-st 200 484 200 6 -6 0) )
(check-expect (reflect-ball-y (make-st 200 253 200 6 -6 0) 6) (make-st 200 259 200 6 6 0) )

; handle-key: st String -> st
; Recibe el estado actual del programa, una tecla y devuelve el nuevo estado

(define (handle-key s k)
  (cond
    [(key=? k "down") (move-bar s BAR-VEL)]
    [(key=? k "up") (move-bar s (- BAR-VEL) )]
    [else (make-st (st-ball-x s) (st-ball-y s) (st-bar-y s) (st-ball-vx s) (st-ball-vy s) (st-points s) )]
  )
)

(check-expect (handle-key (make-st 1 1 (- HEIGHT (/ BAR-VEL 2) ) -1 -1 0) "down")
              (make-st 1 1 (- HEIGHT (/ BAR-HEIGHT 2)) -1 -1 0) )


; move-bar: st Number -> st
; Mueve la barra n unidades, manteniéndola en los márgenes permitidos.

(define (move-bar s n)
  (cond [(and (> (+ (st-bar-y s) n) (/ BAR-HEIGHT 2) )
              (< (+ (st-bar-y s) n) (- HEIGHT (/ BAR-HEIGHT 2) ) )
         )
         (make-st (st-ball-x s) (st-ball-y s) (+ (st-bar-y s) n) (st-ball-vx s) (st-ball-vy s) (st-points s) )]
        [(and (> (+ (st-bar-y s) n) (/ BAR-HEIGHT 2) )
              (not (< (+ (st-bar-y s) n) (- HEIGHT (/ BAR-HEIGHT 2) ) ) )
         )
         (make-st (st-ball-x s) (st-ball-y s) (- HEIGHT (/ BAR-HEIGHT 2) ) (st-ball-vx s) (st-ball-vy s) (st-points s))]
        [(and (not (> (+ (st-bar-y s) n) (/ BAR-HEIGHT 2) ) )
              (< (+ (st-bar-y s) n) (- HEIGHT (/ BAR-HEIGHT 2) ) )
         )
         (make-st (st-ball-x s) (st-ball-y s) (/ BAR-HEIGHT 2) (st-ball-vx s) (st-ball-vy s) (st-points s) )]

  )
)

(check-expect (move-bar (make-st 1 1 (- HEIGHT (/ BAR-VEL 2)) -1 -1 0) BAR-VEL)
              (make-st 1 1 (- HEIGHT (/ BAR-HEIGHT 2) ) -1 -1 0))


; stop?: st -> Boolean
; Si el puntaje llega a 20 o la pelota toca la pared izquierda el juego termina
(define (stop? s)
  (or (= (st-points s) 5) (<= (- (st-ball-x s) BALL-RADIUS) 0) )
)

(check-expect(stop? (make-st 100 200 100 6 6 1) )  #f)
(check-expect(stop? (make-st -1 200 100 6 6 1) )   #t)
(check-expect(stop? (make-st 130 270 150 6 6 4) ) #f)
(check-expect(stop? (make-st 400 250 70 6 6 5) )  #t)

; Imagen que aparece en caso de perder la partida
;(Grupo): para los profesores, decidimos dejar el nombre ULOST ya que para la correción
;pedían lo siguiente, aunque la función termina el juego gane o pierda.
(define (ULOST s)
  (place-image (if (<= (- (st-ball-x s) BALL-RADIUS) 0)
                   (text "Juego terminado" 36 "indigo")
                   (text "Ganaste" 36 "indigo") ) (/ WIDTH 2) (/ HEIGHT 2) BACKGROUND)
)

(big-bang START
  [to-draw draw]
  [on-tick ball-next]
  [on-key handle-key]
  [stop-when stop? ULOST]
)