;;;; ray.asd

(asdf:defsystem #:ray
  :description "A parallel functional raytracer"
  :author "Philip Munksgaard <philip@munksgaard.me>"
  :license  "ICS"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "ray")))
