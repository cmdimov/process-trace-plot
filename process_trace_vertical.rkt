#lang racket

(require 2htdp/image)

; A module is one of:
; -- "procedural"
; -- "temporal"
; -- "retrieval"
; -- "visual"
; -- "manual"
; -- "aural"
; -- "vocal"
; -- "goal"
; -- "imaginal"
; interpretation: a cognitive module

; A List-of-modules is one of:
; -- '()
; -- (cons Process List-of-modules)
; Interpretation: a list of cognitive modules

(define-struct process [module start end name description])
; A Process is a structure:
;    (make-process Module Number Number String String)
; Interpretation: (make-process module start end name description) specifies the
; start and end times of a process, the cognitive module that executes it, its name
; (e.g., chunk held by buffer or production index), and a verbose description of
; what it does (typically applicable for productions)

; A List-of-processes is one of:
; -- '()
; -- (cons Process List-of-processes)
; Interpretation: a collection of cognitive processes

;; Constants
(define MODULES '("visual""retrieval" "temporal"  "imaginal" "manual" "procedural")) ; modules to be included in process trace
(define PIXELS-PER-MS 1.0) ; how time is transformd into image size
(define FONT-SIZE 24)
(define PROCESS-FONT-SIZE 18)
(define AXIS-FONT-SIZE 16)
(define MIN-WIDTH (* PIXELS-PER-MS 50)) ; in pixels
(define TIME-BIN 50) 

; Process-name -> Image
(define (process-name->image pn)
  (if (= (length pn) 1)
      (text (first pn) PROCESS-FONT-SIZE "white")
      (let ([first-slot-value
             (beside/align
              "baseline"
              (text/font (first pn) PROCESS-FONT-SIZE "white" #f 'default 'italic 'normal #f)
              (text " " PROCESS-FONT-SIZE "white")
              (text (second pn) PROCESS-FONT-SIZE "white"))])
        (if (= (length pn) 2)
            first-slot-value
            (above/align "left"
                         first-slot-value
                         (process-name->image (rest (rest pn))))))))

; Process -> Image
; constructs a box that represents a process
(define (process->image prcs)
  (let* ([text-image (process-name->image (string-split (process-name prcs)))]
         [img-width (if (<= (image-width text-image) MIN-WIDTH) MIN-WIDTH (+ (image-width text-image) 10))]
         [img-height (* PIXELS-PER-MS (- (process-end prcs) (process-start prcs)))])
    (overlay
     text-image
     (rectangle img-width img-height "outline" "white")
     (rectangle img-width img-height "solid" "black"))))

; List-of-processes Module -> Image
; constructs the process trace for a module
(define (single-module-processes->image lop)
  (let ([current-process (if (= (length lop) 0) "" (first lop))])
    (cond
      [(= (length lop) 0) (line MIN-WIDTH 0 "white")]
      [(= (length lop) 1) (process->image current-process)]
      [else
       (underlay/align/offset
        "center" "top"
        (process->image current-process)
        0 (* PIXELS-PER-MS (- (process-start (second lop)) (process-start current-process)))
        (single-module-processes->image (rest lop)))])))

; List-of-processes -> Image
; a vertical dashed line of length proportional to end of final process
(define (dashed-line lop)
  (let ([start (apply min (map process-start lop))]
        [end (apply max (map process-end lop))])
    (line 0 (* PIXELS-PER-MS (- end start)) (make-pen "black" 1 "long-dash" "butt" "bevel"))))

; List-of-processes Module -> Image
; a single module process trace overlayed on the dashed line
(define (single-module-trace lop module)
  (let* ([lop-filtered (filter (lambda (x) (string=? (process-module x) module)) lop)]
         [global-start (apply min (map process-start lop))]
         [start (if (= (length lop-filtered) 0) global-start
                    (apply min (map process-start lop-filtered)))]
         [module-name (text module FONT-SIZE "black")]
         [title (text/font "Production description:" FONT-SIZE "black"  #f 'default 'normal 'bold #f)])
    (above
     (overlay
      module-name
      (rectangle (+ 10 (image-width module-name)) (image-height title) "solid" "white"))
     (underlay/align/offset
      "center" "top"
      (dashed-line lop)
      0 (* PIXELS-PER-MS (- start global-start))
      (single-module-processes->image lop-filtered)))))

; List-of-processes -> Image
; constructs a process trace without descriptions
(define (all-processes->image lop modules)
  (if (= (length modules) 1)
      (single-module-trace lop (first modules))
      (beside/align "top"
                    (single-module-trace lop (first modules))
                    (all-processes->image lop (rest modules)))))

; List-of-processes -> Image
; constructs an image of all process descriptions
(define (process-description-text lop)
  (let* ([current-process (first lop)]
        [text-image (text (string-append "   " (process-description current-process)) FONT-SIZE "black")])
    (if (= (length lop) 1)
         text-image
         (underlay/align/offset
         "left" "top"
         text-image
         0  (* PIXELS-PER-MS (- (process-start (second lop)) (process-start current-process)))
         (process-description-text (rest lop))))))

; List-of-processes -> Image
; add title to process description
(define (process-description-all lop)
  (let* ([start-time (apply min (map process-start lop))]
         [end-time (apply max (map process-end lop))]
         [description-text (process-description-text (filter (lambda (prcs) (string=? (process-module prcs) "procedural")) lop))]
         [first-process-type (process-module (first lop))]
         [first-start (if (string=? first-process-type "procedural") 0 (apply min (map process-start lop)))]
         [first-end (if (string=? first-process-type "procedural") 0 (apply min (map process-end lop)))]
         [title (text/font "   Production description:" FONT-SIZE "black"  #f 'default 'normal 'bold #f)])
    (above/align "left"
                 (overlay 
                  title
                  (rectangle (image-width title) (image-height title) "solid" "white"))
                 (rectangle (image-width description-text) (+ (- first-end first-start) 12.5) "solid" "white")
                ; (underlay/align "left" "top"
                       ;          (rectangle (image-width description-text) (* PIXELS-PER-MS (- end-time start-time)) "solid" "white")
                                 description-text)));)

; Number Number Number -> Image
; draws horizontal lines TIME-BIN height apart of a specified length
(define (time-lines nol line-length current-time)
  (let ([my-line
         (beside
          (text (number->string current-time) AXIS-FONT-SIZE "black")
          (line line-length 0 "black"))])
    (if (= nol 1)
        my-line
        (overlay/align/offset
         "right" "top"
         my-line
         0 (* PIXELS-PER-MS TIME-BIN)
         (time-lines (sub1 nol) line-length (+ current-time TIME-BIN))))))
      

; List-of-processes List-of-modules Image
; draw time lines every 50 ms with a length of (image-width Image)
; and a padding on top the size of a module text description
(define (time-axis-image lop lom img)
  (let* ([end-ms (apply max (map process-end lop))]
         [start-ms (apply min (map process-start lop))]
         [number-of-lines (add1 (floor (/ (- end-ms start-ms) TIME-BIN)))]
         [module-name (text (first lom) FONT-SIZE "black")]
         [title (text/font "   Production description:" FONT-SIZE "black"  #f 'default 'normal 'bold #f)]
         [module-img  
          (overlay
           module-name
           (rectangle (+ 10 (image-width module-name)) (image-height title) "solid" "white"))]
         [padding (- (image-height module-img) (/ (image-height (text "0" AXIS-FONT-SIZE "black")) 2.3))]
         [line-length (image-width img)])
    (beside/align "center"
                  (rotate 90 (text/font "Time (ms)" FONT-SIZE "black" #f 'default 'normal 'bold #f))
                  (above
                   (rectangle 100 padding "solid" "white")
                   (time-lines number-of-lines line-length start-ms)))))

; List-of-processes -> Image
; constructs a process trace (including descriptions)
(define (process-trace lop)
  (let* ([processes-image (all-processes->image lop MODULES)]
         [time-axis (time-axis-image lop MODULES processes-image)])
    (beside/align "top"
                  (overlay/align "right" "top"
                               processes-image
                               time-axis)
                (process-description-all lop))))

(define processes
  (list (make-process "procedural" 0 50 "1" "1. Attend to text on the left.")
        (make-process "procedural" 50 100 "2" "2. Read text on the left.")
        (make-process "visual" 100 185 "\"Paris\""  " ")
        (make-process "procedural" 185 235 "3" "3. Attempt to retrieve object and start timing.")
        (make-process "procedural" 235 285 "4" "4. Attend to visual object on the right.")
        (make-process "procedural" 285 335 "5" "5. Read text string on the right.")
        (make-process "retrieval" 235 300 "object Paris attribute name value \"Paris\""  " ")
        (make-process "temporal" 235 300 "ticks 3" " ")
        (make-process "visual" 335 420 "\"Lyon\"" " ")
        (make-process "procedural" 335 385 "6" "6. Store ticks and retrieved object from the left.")
        (make-process "imaginal" 385 585 "left-object Paris left-ticks 3" " ")
        (make-process "procedural" 420 470 "7" "7. Attempt to retrieve object and start timing.")
        (make-process "retrieval" 470 570 "object Lyon attribute name value \"Lyon\"" " ")
        (make-process "temporal" 470 570 "ticks 5" " ")
        (make-process "procedural" 585 635 "8" "8. Store ticks and retrieved object from the right.")
        (make-process "imaginal" 635 835 "left-object Paris left-ticks 3 right-object Lyon right-ticks 5" " ")))

(save-image (process-trace processes) "perceptual-trace2.png")

(define ttb-processes
  (list (make-process "imaginal" 635 835 "left-object Paris left-ticks 3 right-object Lyon right-ticks 5" " ")
        (make-process "procedural" 835 885 "9" "9. Retrieved most valid cue")
        (make-process "retrieval" 885 950 "task city-size current-cue empty next-cue capital" " ")
        (make-process "procedural" 950 1000 "10" "10. Store cue in imaginal buffer and retrieve left cue value")
        (make-process "imaginal" 1000 1200 "left-object Paris right-object Lyon cue capital" "")
        (make-process "retrieval" 1000 1100 "object Paris attribute capital value yes" "")
        (make-process "procedural" 1200 1250 "11" "11. Store left cue value in imaginal buffer and retrieve the right")
        (make-process "imaginal" 1250 1450 "left-object Paris right-object Lyon cue capital left-value yes" "")
        (make-process "retrieval" 1250 1400 "object Lyon attribute capital value no" "")
        (make-process "procedural" 1450 1500 "12" "12. Store right cue value in imaginal buffer")
        (make-process "imaginal" 1500 1700  "left-object Paris right-object Lyon cue capital left-value yes right-value no" "")
        (make-process "procedural" 1700 1750 "13" "13. Second command to index finger to press button")
        (make-process "manual" 1750 2000 "Press A" "")))
(process-trace ttb-processes)

(save-image (process-trace ttb-processes) "ttb-trace.png")


