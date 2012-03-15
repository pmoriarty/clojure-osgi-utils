(ns com.annadaletech.clojure.osgi.utils-test
  (:use
    clojure.java.io
    clojure.test
    clojure.tools.namespace
    com.annadaletech.clojure.osgi.utils
    )
  (:import
    [java.io File]
    [java.util.jar Manifest]
    )
  )

(deftest get-ns-libs-test
  (let [decl '(ns dummy (:use a b [c d]) (:require x [y.z :as zz]))]
    (is (= '(a b [c d]) (get-ns-libs :use decl)))
    (is (= '(x [y.z :as zz]) (get-ns-libs :require decl)))
    (is (= '() (get-ns-libs :import decl)))
    )
  (testing "with doc string"
    (let [decl '(ns dummy "doc-string" (:use a b [c d]) (:require x [y.z :as zz]))]
      (is (= '(a b [c d]) (get-ns-libs :use decl)))
      (is (= '(x [y.z :as zz]) (get-ns-libs :require decl)))
      (is (= '() (get-ns-libs :import decl)))
      )
    )
  )

(deftest get-ns-name-test
  (let [decl '(ns dummy (:use a b [c d]) (:require x [y.z :as zz]))]
    (is (= 'dummy (get-ns-name decl)))
    )
  )

(deftest get-package-tests
  (testing "libspec"
    (is (= "x.y" (get-package 'x.y.z)))
    (is (nil? (get-package 'x)))
    (is (= "x" (get-package '[x.y :as z])))
    (is (= "x" (get-package '[x.y :exclude z])))
    (is (= "x" (get-package '[x.y])))
    )
  (testing "prefix lists"
    (is (= "x.y" (get-package '[x.y z])))
    (is (= "x" (get-package '[x y1 [y2 :as z]])))
    )
  (testing "flags are ignored"
    (is (nil? (get-package :reload)))
    )
  )

(defn temp-file
  [prefix suffix]
  (doto (File/createTempFile prefix suffix)
    (.deleteOnExit)))

(deftest get-manifest-attr-tests
  (let [f (temp-file "TEST_MANIFEST" "MF")]
    (spit f (str "Bundle-Name: Dummy" \newline
              "Export-Package: a1;version=\"[1.23,1.24]\";resolution:=mandatory,a2," \newline
              " d1.e1" \newline
              "Import-Package: c1,y1," \newline
              " d2.e2" \newline))
    (let [mf (Manifest. (input-stream (.toURI f)))]
      (is (= "Dummy" (get-manifest-attr mf "Bundle-Name")))
      (is (= (str "a1;version=\"[1.23,1.24]\";resolution:=mandatory,a2,d1.e1") (get-manifest-attr mf "Export-Package")))
      (is (= (str "c1,y1,d2.e2") (get-manifest-attr mf "Import-Package")))
      )))

(deftest get-osgi-header-packages-tests
  (is (= ["Dummy"] (get-osgi-header-packages "Dummy")))
  (is (= ["a1" "a2" "d1.e1"] (get-osgi-header-packages "a1;version=\"[1.23,1.24]\";resolution:=mandatory,a2,d1.e1;resolution:=mandatory")))
  )

(defn str->manifest [s]
  (doto (Manifest.) (.read (input-stream (.getBytes s)))))

(deftest find-missing-imports-tests
  (with-redefs [find-ns-decls-in-dir
                (fn [dir]
                  '((ns ns1 (:use a1.b1 [c1 d]) (:require x1 [y1.z :as zz]) (:import d1.e1.f java.io.File javax.servlet.HttpServletRequest))
                     (ns ns2 (:use a2.b2 [c2 d]) (:require x2 [y2.z :as zz]) (:import d2.e2.f java.util.concurrent.Future))
                     ))]
    (testing "no ignored packages"
      (let [base-dir 'dummy-dir
            ignored-packages []
            mf (str->manifest (str "Bundle-Name: Dummy" \newline
                                "Export-Package: a1,a2," \newline
                                " d1.e1" \newline
                                "Import-Package: c1,y1," \newline
                                " d2.e2" \newline))]
        (is (= ["a1" "a2" "c2" "clojure" "d1.e1" "javax.servlet" "y2"] (find-missing-imports base-dir mf ignored-packages)))
        ))
    (testing "ignored packages"
      (let [base-dir 'dummy-dir
            ignored-packages ["a1" "a2"]
            mf (str->manifest (str "Bundle-Name: Dummy" \newline
                                "Export-Package: a1,a2;resolution:=mandatory," \newline
                                " d1.e1" \newline
                                "Import-Package: c1,y1," \newline
                                " d2.e2" \newline))]
        (is (= ["c2" "clojure" "d1.e1" "javax.servlet" "y2"] (find-missing-imports base-dir mf ignored-packages)))
        ))
  )
  (testing "missing same package in use and require"
    (with-redefs [find-ns-decls-in-dir
                  (fn [dir]
                    '((ns ns1 (:use a.b))
                       (ns ns2 (:require [a.b :as c]))
                       ))]
      (let [base-dir 'dummy-dir
            ignored-packages []
            mf (str->manifest (str "Bundle-Name: Dummy" \newline
                                "Export-Package: w,x" \newline
                                "Import-Package: y,z" \newline))]
        (is (= ["a" "clojure"] (find-missing-imports base-dir mf ignored-packages)))
        ))
    )
  (testing "imports are namespace-munged"
    (with-redefs [find-ns-decls-in-dir
                  (fn [dir]
                    '((ns ns1 (:use a-b.c-d.e))))]
      (let [base-dir 'dummy-dir
            ignored-packages []
            mf (str->manifest (str "Bundle-Name: Dummy" \newline
                                "Export-Package: w,x" \newline
                                "Import-Package: y,z" \newline))]
        (is (= ["a_b.c_d" "clojure"] (find-missing-imports base-dir mf ignored-packages)))
        ))
    )
  )


(deftest find-missing-exports-tests
  (with-redefs [find-ns-decls-in-dir
                (fn [dir]
                  '((ns a1.b1.ns1) (ns a2.b2.ns2) (ns a3.b3.ns3)))]
    (testing "no ignored packages"
      (let [base-dir 'dummy-dir
            ignored-packages []
            mf (str->manifest (str "Bundle-Name: Dummy" \newline
                                "Export-Package: a3.b3" \newline))]
        (is (= ["a1.b1" "a2.b2"] (find-missing-exports base-dir mf ignored-packages)))
        ))
    (testing "ignored packages"
      (let [base-dir 'dummy-dir
            ignored-packages ["a1.b1"]
            mf (str->manifest (str "Bundle-Name: Dummy" \newline
                                "Export-Package: a3.b3;resolution:=mandatory" \newline))]
        (is (= ["a2.b2"] (find-missing-exports base-dir mf ignored-packages)))
        ))
    )
  (testing "duplicate packages"
    (with-redefs [find-ns-decls-in-dir
                  (fn [dir]
                    '((ns a1.b1.ns1) (ns a1.b1.ns2) (ns a3.b3.ns3)))]
      (let [base-dir 'dummy-dir
            ignored-packages []
            mf (str->manifest (str "Bundle-Name: Dummy" \newline
                                "Export-Package: a3.b3" \newline))]
        (is (= ["a1.b1"] (find-missing-exports base-dir mf ignored-packages)))
        ))
    )
  (testing "exports are namespace-munged"
    (with-redefs [find-ns-decls-in-dir
                  (fn [dir]
                    '((ns a-b.c-d.ns1)))]
      (let [base-dir 'dummy-dir
            ignored-packages []
            mf (str->manifest (str "Bundle-Name: Dummy" \newline
                                "Export-Package: a3.b3" \newline))]
        (is (= ["a_b.c_d"] (find-missing-exports base-dir mf ignored-packages)))
        ))
    )
  )

(deftest add-package-imports-tests
  (let [mf (str->manifest (str
                            "Bundle-Name: Dummy" \newline
                            "Import-Package: c1,y1;version=\"[1.23,1.24]\";resolution:=mandatory," \newline
                            " d2.e2" \newline))]
    (is (= "c1,y1;version=\"[1.23,1.24]\";resolution:=mandatory,d2.e2,c2,y2,d1.e1"
          (get-manifest-attr (add-package-imports mf ["c2" "y2" "d1.e1"]) IMPORT-PACKAGE))))
  )

(deftest add-package-exports-tests
  (let [mf (str->manifest (str
                            "Bundle-Name: Dummy" \newline
                            "Export-Package: c1,y1;version=\"[1.23,1.24]\";resolution:=mandatory," \newline
                            " d2.e2" \newline))]
    (is (= "c1,y1;version=\"[1.23,1.24]\";resolution:=mandatory,d2.e2,c2,y2,d1.e1"
          (get-manifest-attr (add-package-exports mf ["c2" "y2" "d1.e1"]) EXPORT-PACKAGE))))
  )

(deftest write-manifest-tests
  (let [f (temp-file "TEST_MANIFEST" "MF")
        text (str "Bundle-Name: Dummy" \newline
                  "Export-Package: a1;version=\"[1.23,1.24]\";resolution:=mandatory,a2," \newline
                  " d1.e1" \newline
                  "Import-Package: c1,y1," \newline
                  " d2.e2" \newline)
        mf (str->manifest text)]
    (write-manifest mf f)
    (let [written (str->manifest (slurp f))]
      (is (= "Dummy" (get-manifest-attr written "Bundle-Name")))
      (is (= "a1;version=\"[1.23,1.24]\";resolution:=mandatory,a2,d1.e1" (get-manifest-attr written "Export-Package")))
      (is (= "c1,y1,d2.e2" (get-manifest-attr written IMPORT-PACKAGE)))
      )))

; Uncomment to run these tests in IDEA, recomment again as clojure-maven-plugin does not need this
(run-tests 'com.annadaletech.clojure.osgi.utils-test)

