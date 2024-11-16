;;;; weekend-raytracer.asd

(asdf:defsystem #:weekend-raytracer
  :description "WEEKEND-RAYTRACER is another n-dimensional raytracer."
  :author "Patrick Stein <pat@nklein.com>"
  :license "UNLICENSE"
  :version "1.2.20241115"
  :depends-on (#:policy-cond #:zpng #:bordeaux-threads #:alexandria)
  :in-order-to ((asdf:test-op (asdf:test-op :weekend-raytracer/test)))
  :components
  ((:static-file "README.md")
   (:static-file "UNLICENSE.txt")
   (:module "src"
    :components ((:file "package")
                 (:file "compile" :depends-on ("package"))
                 (:file "types" :depends-on ("package"
                                             "compile"))
                 (:file "box-muller" :depends-on ("package"
                                                  "compile"
                                                  "types"))
                 (:file "interval" :depends-on ("package"
                                                "compile"
                                                "types"))
                 (:file "vector" :depends-on ("package"
                                              "compile"
                                              "types"
                                              "box-muller"))
                 (:file "nullspace" :depends-on ("package"
                                                 "compile"
                                                 "types"
                                                 "vector"))
                 (:file "ray" :depends-on ("package"
                                           "compile"
                                           "types"
                                           "vector"))
                 (:file "color" :depends-on ("package"
                                             "compile"
                                             "types"))
                 (:file "hit" :depends-on ("package"
                                           "compile"
                                           "types"
                                           "interval"
                                           "vector"
                                           "ray"
                                           "color"))
                 (:file "material" :depends-on ("package"
                                                "compile"
                                                "types"
                                                "vector"
                                                "ray"
                                                "color"
                                                "hit"))
                 (:file "sphere" :depends-on ("package"
                                              "compile"
                                              "types"
                                              "interval"
                                              "vector"
                                              "ray"
                                              "color"
                                              "hit"))
                 (:file "csg-intersect" :depends-on ("package"
                                                     "compile"
                                                     "types"
                                                     "interval"
                                                     "vector"
                                                     "ray"
                                                     "color"
                                                     "hit"))
                 (:file "csg-complement" :depends-on ("package"
                                                      "compile"
                                                      "types"
                                                      "interval"
                                                      "vector"
                                                      "ray"
                                                      "color"
                                                      "hit"))
                 (:file "camera" :depends-on ("package"
                                              "compile"
                                              "types"
                                              "interval"
                                              "vector"
                                              "nullspace"
                                              "ray"
                                              "color"
                                              "hit"))
                 (:file "output" :depends-on ("package"
                                              "compile"
                                              "types"))))))

(asdf:defsystem #:weekend-raytracer/examples
  :description "Examples for the WEEKEND-RAYTRACER package."
  :author "Patrick Stein <pat@nklein.com>"
  :license "UNLICENSE"
  :version "1.2.20241115"
  :depends-on ((:version #:weekend-raytracer "1.2.20241115"))
  :in-order-to ((asdf:test-op (asdf:test-op :weekend-raytracer/test)))
  :components
  ((:static-file "README.md")
   (:static-file "UNLICENSE.txt")
   (:module "examples"
    :components ((:file "package")
                 (:file "B1C2-2image" :depends-on ("package"))
                 (:file "B1C4-2image" :depends-on ("package"))
                 (:file "B1C5-2image" :depends-on ("package"))
                 (:file "B1C6-1image" :depends-on ("package"))
                 (:file "B1C6-7image" :depends-on ("package"))
                 (:file "B1C7-1image" :depends-on ("package"))
                 (:file "B1C8-2image" :depends-on ("package"))
                 (:file "B1C9-7image" :depends-on ("package"))
                 (:file "B1C9-10image" :depends-on ("package"))
                 (:file "B1C9-12image" :depends-on ("package"))
                 (:file "B1C10-13image" :depends-on ("package"))
                 (:file "B1C10-14image" :depends-on ("package"))
                 (:file "B1C11-16image" :depends-on ("package"))
                 (:file "B1C11-17image" :depends-on ("package"))
                 (:file "B1C11-18image" :depends-on ("package"))
                 (:file "B1C12-19image" :depends-on ("package"))
                 (:file "B1C12-20image" :depends-on ("package"))
                 (:file "B1C12-21image" :depends-on ("package"))
                 (:file "B1C13-22image" :depends-on ("package"))
                 (:file "B1C14-23image" :depends-on ("package"))
                 (:file "CSG-Intersect" :depends-on ("package"))
                 (:file "CSG-Complement" :depends-on ("package"))))))

(asdf:defsystem #:weekend-raytracer/test
  :description "Tests for the WEEKEND-RAYTRACER package."
  :author "Patrick Stein <pat@nklein.com>"
  :license "UNLICENSE"
  :version "1.2.20241115"
  :depends-on ((:version #:weekend-raytracer "1.2.20241115") #:nst)
  :perform (asdf:test-op (o c)
                         (uiop:symbol-call :weekend-raytracer/test :run-all-tests))
  :components
  ((:static-file "README.md")
   (:static-file "UNLICENSE.txt")
   (:module "test"
    :components ((:file "package")
                 (:file "criteria" :depends-on ("package"))
                 (:file "fixtures" :depends-on ("package"))
                 (:file "box-muller" :depends-on ("package"))
                 (:file "interval" :depends-on ("package"))
                 (:file "vector" :depends-on ("package"))
                 (:file "nullspace" :depends-on ("package"
                                                 "criteria"))
                 (:file "ray" :depends-on ("package"
                                           "criteria"))
                 (:file "color" :depends-on ("package"))
                 (:file "hit" :depends-on ("package"
                                           "criteria"
                                           "fixtures"))
                 (:file "sphere" :depends-on ("package"
                                              "criteria"
                                              "fixtures"))
                 (:file "csg-intersect" :depends-on ("package"
                                                     "criteria"
                                                     "fixtures"))
                 (:file "csg-complement" :depends-on ("package"
                                                      "criteria"
                                                      "fixtures"))
                 (:file "camera" :depends-on ("package"))
                 (:file "output" :depends-on ("package"))
                 (:file "run" :depends-on ("package"))))))
