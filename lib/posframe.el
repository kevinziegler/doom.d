(defun kdz/side-window-critical-dim (frame side)
  (with-selected-frame frame
    (let ((critical-dim-fn
           (cond ((member side '(right left)) #'window-pixel-width)
                 ((member side '(top bottom)) #'window-pixel-height)
                 (t (error (format "Invalid window side: %s" side)))))
          (windows (-filter (lambda (window)
                              (eq (window-parameter window 'window-side) side))
                            (window-list))))
      (if windows (-max (mapcar critical-dim-fn windows)) 0))))

;; Posframe notes:
;;
;; Coordinates can be either positive or negative integers:
;; - (0,0) -> Top left corner of frame
;; - (-1,-1) -> Bottom right corner of screen
;;
;; Interior centered:
;; X position: left-offset + ((interior-width - posframe-width) / 2)
;; Y position: top-offset  + ((interior-height - posframe-height) /2)

(defun kdz/frame-interior-width (info)
  "Get the width of the frame minus the width of left/right side windows"
  (let ((frame (plist-get info :parent-frame)))
    (- (plist-get info :parent-frame-width)
       (kdz/side-window-critical-dim frame 'left)
       (kdz/side-window-critical-dim frame 'right))))

(defun kdz/frame-interior-height (info)
  "Get the height of the frame minus the width of top/bottom side windows"
  (let ((frame (plist-get info :parent-frame)))
    (- (plist-get info :parent-frame-height)
       (kdz/side-window-critical-dim frame 'top)
       (kdz/side-window-critical-dim frame 'bottom))))

(defun kdz/posframe-center-interior-horizontal (info)
  "Get the centering X coordinate for a posframe displayed in frame interior"
  (+ (kdz/side-window-critical-dim (plist-get info :parent-frame) 'left)
     (/ (- (kdz/frame-interior-width info) (plist-get info :posframe-width)) 2)))

(defun kdz/posframe-center-interior-vertical (info)
  "Get the centering Y coordinate for a posframe displayed in frame interior"
  (+ (kdz/side-window-critical-dim (plist-get info :parent-frame) 'top)
     (/ (- (kdz/frame-interior-height info) (plist-get info :posframe-height)) 2)))

(defun kdz/posframe-interior-top-offset (info offset)
  (cons (kdz/posframe-center-interior-horizontal info)
        (+ (kdz/side-window-critical-dim (plist-get info :parent-frame) 'top)
           offset)))

(defun kdz/posframe-interior-bottom-offset (info offset)
  (cons (kdz/posframe-center-interior-horizontal info)
        (- (plist-get info :parent-frame-height)
           offset
           (plist-get info :posframe-height)
           (plist-get info :mode-line-height)
           (plist-get info :minibuffer-height)
           (kdz/side-window-critical-dim (plist-get info :parent-frame)
                                         'bottom))))

(defmacro kdz/posframe-interior-top (offset)
  `(lambda (info) (kdz/posframe-interior-top-offset info ,offset)))

(defmacro kdz/posframe-interior-bottom (offset)
  `(lambda (info) (kdz/posframe-interior-bottom-offset info ,offset)))

(defun kdz/posframe-interior-centered (info)
  (cons (kdz/posframe-center-interior-horizontal info)
        (kdz/posframe-center-interior-vertical info)))
