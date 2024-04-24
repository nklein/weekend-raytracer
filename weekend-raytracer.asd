;;;; weekend-raytracer.asd

(asdf:defsystem #:weekend-raytracer
  :description "WEEKEND-RAYTRACER is another n-dimensional raytracer."
  :author "Patrick Stein <pat@nklein.com>"
  :license "UNLICENSE"
  :version "0.1.20240424"
  :depends-on (#:zpng)
  :in-order-to ((asdf:test-op (asdf:test-op :weekend-raytracer/test)))
  :components
  ((:static-file "README.md")
   (:static-file "UNLICENSE.txt")
   (:module "src"
    :components ((:file "package")
                 (:file "output" :depends-on ("package"))))))

(asdf:defsystem #:weekend-raytracer/examples
  :description "Examples for the WEEKEND-RAYTRACER package."
  :author "Patrick Stein <pat@nklein.com>"
  :license "UNLICENSE"
  :version "0.1.20240424"
  :depends-on ((:version #:weekend-raytracer "0.1.20240424"))
  :in-order-to ((asdf:test-op (asdf:test-op :weekend-raytracer/test)))
  :components
  ((:static-file "README.md")
   (:static-file "UNLICENSE.txt")
   (:module "examples"
    :components ((:file "package")
                 (:file "B1C2-2image" :depends-on ("package"))))))

(asdf:defsystem #:weekend-raytracer/test
  :description "Tests for the WEEKEND-RAYTRACER package."
  :author "Patrick Stein <pat@nklein.com>"
  :license "UNLICENSE"
  :version "0.1.20240424"
  :depends-on ((:version #:weekend-raytracer "0.1.20240424") #:nst)
  :perform (asdf:test-op (o c)
                         (uiop:symbol-call :weekend-raytracer/test :run-all-tests))
  :components
  ((:static-file "README.md")
   (:static-file "UNLICENSE.txt")
   (:module "test"
    :components ((:file "package")
                 (:file "output" :depends-on ("package"))
                 (:file "run" :depends-on ("package"))))))
