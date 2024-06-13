;;;; weekend-raytracer.asd

(asdf:defsystem #:weekend-raytracer
  :description "WEEKEND-RAYTRACER is another n-dimensional raytracer."
  :author "Patrick Stein <pat@nklein.com>"
  :license "UNLICENSE"
  :version "0.2.20240613"
  :depends-on (#:policy-cond #:zpng)
  :in-order-to ((asdf:test-op (asdf:test-op :weekend-raytracer/test)))
  :components
  ((:static-file "README.md")
   (:static-file "UNLICENSE.txt")
   (:module "src"
    :components ((:file "package")
                 (:file "compile" :depends-on ("package"))
                 (:file "types" :depends-on ("package"
                                             "compile"))
                 (:file "interval" :depends-on ("package"
                                                "compile"
                                                "types"))
                 (:file "vector" :depends-on ("package"
                                              "compile"
                                              "types"))
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
                                           "color"
                                           "ray"))
                 (:file "sphere" :depends-on ("package"
                                              "compile"
                                              "types"
                                              "interval"
                                              "vector"
                                              "color"
                                              "hit"))
                 (:file "output" :depends-on ("package"
                                              "compile"
                                              "types"))))))

(asdf:defsystem #:weekend-raytracer/examples
  :description "Examples for the WEEKEND-RAYTRACER package."
  :author "Patrick Stein <pat@nklein.com>"
  :license "UNLICENSE"
  :version "0.2.20240613"
  :depends-on ((:version #:weekend-raytracer "0.2.20240613"))
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
                 (:file "B1C6-7image" :depends-on ("package"))))))

(asdf:defsystem #:weekend-raytracer/test
  :description "Tests for the WEEKEND-RAYTRACER package."
  :author "Patrick Stein <pat@nklein.com>"
  :license "UNLICENSE"
  :version "0.2.20240613"
  :depends-on ((:version #:weekend-raytracer "0.2.20240613") #:nst)
  :perform (asdf:test-op (o c)
                         (uiop:symbol-call :weekend-raytracer/test :run-all-tests))
  :components
  ((:static-file "README.md")
   (:static-file "UNLICENSE.txt")
   (:module "test"
    :components ((:file "package")
                 (:file "interval" :depends-on ("package"))
                 (:file "vector" :depends-on ("package"))
                 (:file "ray" :depends-on ("package"))
                 (:file "color" :depends-on ("package"))
                 (:file "hit" :depends-on ("package"))
                 (:file "sphere" :depends-on ("package"))
                 (:file "output" :depends-on ("package"))
                 (:file "run" :depends-on ("package"))))))
