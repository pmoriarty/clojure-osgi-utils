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

(deftest find-missing-imports-tests
  (with-redefs [find-ns-decls-in-dir
                (fn [dir]
                  '((ns ns1 (:use a1.b1 [c1 d]) (:require x1 [y1.z :as zz]) (:import d1.e1.f java.io.File javax.servlet.HttpServletRequest))
                     (ns ns2 (:use a2.b2 [c2 d]) (:require x2 [y2.z :as zz]) (:import d2.e2.f java.util.concurrent.Future))
                     ))]
    (testing "no ignored packages"
      (let [base-dir 'dummy-dir
            ignored-packages []
            f (temp-file "TEST_MANIFEST" "MF")]
        (spit f (str "Bundle-Name: Dummy" \newline
                  "Export-Package: a1,a2," \newline
                  " d1.e1" \newline
                  "Import-Package: c1,y1," \newline
                  " d2.e2" \newline))
        (let [mf (.toURI f)]
          (is (= ["c2" "a1" "a2" "y2" "d1.e1" "javax.servlet"] (find-missing-imports base-dir mf ignored-packages)))
          )))
    (testing "ignored packages"
      (let [base-dir 'dummy-dir
            ignored-packages ["a1" "a2"]
            f (temp-file "TEST_MANIFEST" "MF")]
        (spit f (str "Bundle-Name: Dummy" \newline
                  "Export-Package: a1,a2;resolution:=mandatory," \newline
                  " d1.e1" \newline
                  "Import-Package: c1,y1," \newline
                  " d2.e2" \newline))
        (let [mf (.toURI f)]
          (is (= ["c2" "y2" "d1.e1" "javax.servlet"] (find-missing-imports base-dir mf ignored-packages)))
          )))
    ))

; Uncomment to run these tests in IDEA, recomment again as clojure-maven-plugin does not need this
(run-tests 'com.annadaletech.clojure.osgi.utils-test)

