(defun vec3-x (v) (first v))

(defun vec3-y (v) (second v))

(defun vec3-z (v) (third v))

(defun add (v1 v2)
  (mapcar #'+ v1 v2))

(defun sub (v1 v2)
  (mapcar #'- v1 v2))

(defun mul (v1 v2)
  (mapcar #'* v1 v2))

(defun scale (v s)
  (mapcar (lambda (x) (* x s)) v))

(defun dot (v1 v2)
  (let ((v3 (mul v1 v2)))
    (reduce #'+ v3)))

(defun norm (v)
  (sqrt (dot v v)))

(defun normalise (v)
  (scale v (/ 1.0 (norm v))))

(defun cross (v1 v2)
  (list
   (- (* (vec3-y v1) (vec3-z v2)) (* (vec3-z v1) (vec3-y v2)))
   (- (* (vec3-z v1) (vec3-x v2)) (* (vec3-x v1) (vec3-z v2)))
   (- (* (vec3-x v1) (vec3-y v2)) (* (vec3-y v1) (vec3-x v2)))))

(defun reflect (v n)
  (sub v (scale n (* 2.0 (dot v n)))))

(defconstant +BLACK+ '(0 0 0))

(defconstant +WHITE+ '(1 1 1))

(defstruct hit
  t
  pos
  normal
  colour)

(defstruct ray
  origin
  dir)

(defun point-at-param (ray p)
  (add (ray-origin ray) (scale (ray-dir ray) p)))

(defun scatter (ray hit)
  (let* ((reflected (reflect (normalise (ray-dir ray)) (hit-normal hit)))
         (scattered (make-ray :origin (hit-pos hit)
                              :dir reflected)))
    (if (> (dot (ray-dir scattered) (hit-normal hit)) 0)
        (cons scattered (hit-colour hit)))))

(defstruct aabb
  min
  max)

(defun centre (aabb)
  (list
   (+ (vec3-x (aabb-min aabb)) (- (vec3-x (aabb-max aabb)) (vec3-x (aabb-min aabb))))
   (+ (vec3-y (aabb-min aabb)) (- (vec3-y (aabb-max aabb)) (vec3-y (aabb-min aabb))))
   (+ (vec3-z (aabb-min aabb)) (- (vec3-z (aabb-max aabb)) (vec3-z (aabb-min aabb))))))

(defun aabb-hit (aabb r tmin0 tmax0)
  (let* ((iter (lambda (min* max* origin* dir* tmin* tmax*)
                 (let* ((inv-d (handler-case (/ 1.0 dir*)
                                 (division-by-zero () most-positive-single-float)))
                        (t0 (handler-case (* (- min* origin*) inv-d)
                              (floating-point-overflow () most-positive-single-float)))
                        (t1 (handler-case (* (- max* origin*) inv-d)
                              (floating-point-overflow () most-positive-single-float)))
                        (t* (if (< inv-d 0)
                                (cons t1 t0)
                              (cons t0 t1)))
                        (tmin** (max (car t*) tmin*))
                        (tmax** (min (cdr t*) tmax*)))
                   (cons tmin** tmax**))))

         (t1 (funcall iter
                      (vec3-x (aabb-min aabb))
                      (vec3-x (aabb-max aabb))
                      (vec3-x (ray-origin r))
                      (vec3-x (ray-dir r))
                      tmin0
                      tmax0)))
    (if (<= (cdr t1) (car t1))
        nil
      (let ((t2 (funcall iter
                         (vec3-y (aabb-min aabb))
                         (vec3-y (aabb-max aabb))
                         (vec3-y (ray-origin r))
                         (vec3-y (ray-dir r))
                         (car t1)
                         (cdr t1))))
        (if (<= (cdr t2) (car t2))
            nil
          (let ((t3 (funcall iter
                             (vec3-z (aabb-min aabb))
                             (vec3-z (aabb-max aabb))
                             (vec3-z (ray-origin r))
                             (vec3-z (ray-dir r))
                             (car t2)
                             (cdr t2))))
            (> (cdr t3) (car t3))))))))

(defun enclosing (aabb1 aabb2)
  (let ((small (list (min (vec3-x (aabb-min aabb1)) (vec3-x (aabb-min aabb2)))
                     (min (vec3-y (aabb-min aabb1)) (vec3-y (aabb-min aabb2)))
                     (min (vec3-z (aabb-min aabb1)) (vec3-z (aabb-min aabb2)))))
        (big (list (max (vec3-x (aabb-max aabb1)) (vec3-x (aabb-max aabb2)))
                   (max (vec3-y (aabb-max aabb1)) (vec3-y (aabb-max aabb2)))
                   (max (vec3-z (aabb-max aabb1)) (vec3-z (aabb-max aabb2))))))
    (make-aabb :min small :max big)))

;; Bvh is either ('leaf aabb t) or ('split aabb bvh bvh)
(defun bvh-leaf-p (bvh)
  (eq (first bvh) 'leaf))

(defun bvh-aabb (bvh) (second bvh))

(defun make-bvh (to-aabb all-objs)
  (labels ((mk (d n xs)
               (cond
                ((null xs) (error "mk_bvh: no nodes"))
                ((= 1 (length xs)) (list 'leaf (funcall to-aabb (first xs)) (first xs)))
                (t
                 (let*
                     ((xs-sorted
                       ;; TODO: Parallelise
                       (sort xs (lambda (a b)
                                  (let ((axis
                                         (cond
                                          ((= (mod d 3) 0) #'vec3-x)
                                          ((= (mod d 3) 1) #'vec3-y)
                                          (t #'vec3-z))))
                                    (< (funcall axis (centre (funcall to-aabb a)))
                                       (funcall axis (centre (funcall to-aabb b))))))))
                      (xs-left (subseq xs-sorted 0 (floor n 2)))
                      (xs-right (subseq xs-sorted (floor n 2)))
                      (do-left (lambda () (mk (1+ d) (floor n 2) xs-left)))
                      (do-right (lambda () (mk (1+ d) (- n (floor n 2)) xs-right)))
                      (both
                       (if (< n 100)
                           (cons (funcall do-left)
                                 (funcall do-right))
                         ;; TODO: Parallelise
                         (cons (funcall do-left)
                               (funcall do-right))))
                      (box (enclosing (bvh-aabb (car both))
                                      (bvh-aabb (cdr both)))))
                   (list 'split box (car both) (cdr both)))))))
    (mk 0 (length all-objs) all-objs)))

(defstruct sphere
  pos
  colour
  radius)

(defun sphere-hit (sphere r t-min t-max)
  (let* ((oc (sub (ray-origin r) (sphere-pos sphere)))
         (a (dot (ray-dir r) (ray-dir r)))
         (b (dot oc (ray-dir r)))
         (c (- (dot oc oc) (* (sphere-radius sphere) (sphere-radius sphere))))
         (discriminant (- (* b b) (* a c)))
         (helper (lambda (temp)
                   (if (and (< temp t-max) (> temp t-min))
                       (make-hit :t temp
                                 :pos (point-at-param r temp)
                                 :normal (scale (sub (point-at-param r temp) (sphere-pos sphere))
                                                (/ 1.0 (sphere-radius sphere)))
                                 :colour (sphere-colour sphere))))))
      (if (<= discriminant 0.0)
          nil
        (let ((hit (funcall helper (/ (- (- b) (sqrt discriminant)) a))))
          (if hit
              hit
            (funcall helper (/ (+ (- b) (sqrt discriminant)) a)))))))

(defun sphere-aabb (sphere)
  (let ((radius (sphere-radius sphere))
        (pos (sphere-pos sphere)))
    (make-aabb :min (sub pos (list radius radius radius))
               :max (add pos (list radius radius radius)))))

(defun objs-hit (bvh r t-min t-max)
  (if (bvh-leaf-p bvh)
      (sphere-hit (third bvh) r t-min t-max)
    (let ((box (second bvh))
          (left (third bvh))
          (right (fourth bvh)))
      (if (not (aabb-hit box r t-min t-max))
          nil
        (let ((h (objs-hit left r t-min t-max)))
          (if h
              (or (objs-hit right r t-min (hit-t h))
                  h)
            (objs-hit right r t-min t-max)))))))

(defun colour (ray objs depth)
  (let ((hit (objs-hit objs ray 0.001 1000000000.0)))
    (if hit
        (let ((scatter-res (scatter ray hit)))
          (if scatter-res
              (if (< depth 50)
                  (mul (cdr scatter-res)
                     (colour (car scatter-res) objs (1+ depth)))
                +BLACK+)
            +BLACK+))
      (let* ((unit-dir (normalise (ray-dir ray)))
             (s (* 0.5 (1+ (vec3-y unit-dir))))
             (bg (list 0.5 0.7 1.0)))
        (add (scale +WHITE+ (- 1.0 s)) (scale bg s))))))

(defstruct camera
  origin
  llc
  horizontal
  vertical)

(defun camera (look-from look-at vup vfov aspect)
  (let* ((theta (/ (* vfov pi) 180.0))
         (half-height (tan (/ theta 2.0)))
         (half-width (* aspect half-height))
         (w (normalise (sub look-from look-at)))
         (u (normalise (cross vup w)))
         (v (cross w u)))
    (make-camera :origin look-from
                 :llc (sub (sub (sub look-from (scale u half-width))
                                (scale v half-height))
                           w)
                 :horizontal (scale u (* 2.0 half-width))
                 :vertical (scale v (* 2.0 half-height)))))

(defun get-ray (cam x y)
  (make-ray :origin (camera-origin cam)
            :dir (sub (add (add (camera-llc cam) (scale (camera-horizontal cam) x))
                           (scale (camera-vertical cam) y))
                      (camera-origin cam))))

(defun trace-ray (objs width height cam j i)
  (let* ((u (/ (float i) (float width)))
         (v (/ (float j) (float height)))
         (ray (get-ray cam u v)))
    (colour ray objs 0)))

(defun colour-to-pixel (v)
  (let* ((ir (truncate (* 255.99 (float (vec3-x v)))))
         (ig (truncate (* 255.99 (float (vec3-y v)))))
         (ib (truncate (* 255.99 (float (vec3-z v))))))
    (list ir ig ib)))

(defstruct image
  pixels
  height
  width)

(defun image-to-ppm (out image)
  (defun on-pixel (pixel)
    (format out "~A ~A ~A~%" (first pixel) (second pixel) (third pixel)))
  (format out "P3~%~A ~A~%255~%" (image-width image) (image-height image))
  (mapc #'on-pixel (image-pixels image)))

(defun render (objs width height cam)
  (defun pixel (l)
    (let* ((i (mod l width))
           (j (- height (/ l width))))
      (colour-to-pixel (trace-ray objs width height cam j i))))
  ;; TODO: Parallelise
  (make-image :pixels (loop for n from 0 to (* height width)
                            collect (pixel n))
              :height height
              :width width))

(defstruct scene
  cam-look-from
  cam-look-at
  cam-fov
  spheres)

(defun objs-and-camera-from-scene (width height scene)
  (cons (make-bvh #'sphere-aabb (scene-spheres scene))
        (camera (scene-cam-look-from scene)
                (scene-cam-look-at scene)
                '(0.0 1.0 0.0)
                (scene-cam-fov scene)
                (/ (float width) (float height)))))

(defun tabulate-2d (m n f)
  (loop for x from 0 to (* m n)
        collect (multiple-value-bind (j i)
                    (floor x n)
                  (funcall f j i))))

(defconstant +RGBBOX+
  (let* ((n 10)
         (k 60.0)
         (leftwall (tabulate-2d
                    n n
                    (lambda (y z)
                      (make-sphere
                       :pos (list (/ (- k) 2.0)
                                  (+ (/ (- k) 2.0) (* (/ k (float n)) (float y)))
                                  (+ (/ (- k) 2.0) (* (/ k (float n)) (float z))))
                       :colour '(1.0 0.0 0.0)
                       :radius (/ k (* (float n) 2.0))))))
         (midwall (tabulate-2d
                   n n
                   (lambda (x y)
                     (make-sphere
                      :pos (list (+ (/ (- k) 2.0) (* (/ k (float n)) (float x)))
                                 (+ (/ (- k) 2.0) (* (/ k (float n)) (float y)))
                                 (/ (- k) 2.0))
                      :colour '(1.0 1.0 0.0)
                      :radius (/ k (* (float n) 2.0))))))
         (rightwall (tabulate-2d
                    n n
                    (lambda (y z)
                      (make-sphere
                       :pos (list (/ k 2.0)
                                  (+ (/ (- k) 2.0) (* (/ k (float n)) (float y)))
                                  (+ (/ (- k) 2.0) (* (/ k (float n)) (float z))))
                       :colour '(0.0 0.0 1.0)
                       :radius (/ k (* (float n) 2.0))))))
         (bottom (tabulate-2d
                  n n
                  (lambda (x z)
                    (make-sphere
                     :pos (list (+ (/ (- k) 2.0) (* (/ k (float n)) (float x)))
                                (/ (- k) 2.0)
                                (+ (/ (- k) 2.0) (* (/ k (float n)) (float z))))
                     :colour '(1.0 1.0 1.0)
                     :radius (/ k (* (float n) 2.0)))))))
    (make-scene
     :spheres (concatenate 'list leftwall midwall rightwall bottom)
     :cam-look-from '(0.0 30.0 30.0)
     :cam-look-at '(0.0 -1.0 -1.0)
     :cam-fov 75.0)))

(defconstant +IRREG+
  (let* ((n 100)
         (k 600.0)
         (bottom (tabulate-2d
                  n n
                  (lambda (x z)
                    (make-sphere
                     :pos (list (+ (/ (- k) 2.0) (* (/ k (float n)) (float x)))
                                0.0
                                (+ (/ (- k) 2.0) (* (/ k (float n)) (float z))))
                     :colour +WHITE+
                     :radius (/ k (* (float n) 2.0)))))))
    (make-scene
     :spheres bottom
     :cam-look-from '(0.0 12.0 30.0)
     :cam-look-at '(0.0 10.0 -1.0)
     :cam-fov 75.0)))

(defun bench-and-write (scene filename width height)
  (let* ((objs-and-camera (time (objs-and-camera-from-scene width height scene)))
         (image (time (render (car objs-and-camera) width height (cdr objs-and-camera)))))
    (with-open-file (stream filename :direction :output :if-exists :overwrite :if-does-not-exist :create)
                    (image-to-ppm stream image))))
