(ns com.annadaletech.clojure.osgi.utils
  (:use
    clojure.pprint
    clojure.tools.namespace
    ))

(defn get-ns-libs
  "Returns a seq of libs of the given type (:use, :require, :import) from the namespace declaration."
  [type ns]
  (rest (first (filter #(and (sequential? %1) (= type (first %1))) ns))))

(defn get-ns-name
  "Returns the name from the given namespace declaration."
  [ns]
  (second ns))

(defn get-package
  "Returns the package (in Java terms) as a String for a given reference (libspec, prefix list or flag).
  The libspec may be a symbol or a sequential.
    symbol:
    x.y.z => x.y
    sequence:
    (x.y z1 z2) => x.y
    [x.y.z :as zz] => x.y"
  [reference]
  (letfn [(get-base [package-and-class]
            (let [package-and-class (name package-and-class)
                  [_ package class] (re-find #"(.*)\.(.*)" package-and-class)]
              package))]
    (cond
      ;libspecs
      (symbol? reference) (get-base reference)
      ;prefix lists
      (sequential? reference) (cond
                                (= 1 (count reference)) (get-base (first reference))
                                (keyword? (second reference)) (get-base (first reference))
                                :else (name (first reference)))
      ;flags
      (keyword? reference) nil)))

(defn find-missing-imports
  "Finds packages in use/require/import declarations in clojure source files in base-dir
   and checks that all are present in the manifest file.
   Takes seq of ignored-packages (Strings) to exclude certain packages from consideration.
   Also excludes any packages provided by the clojure source files in base-dir.
   "
  [base-dir manifest ignored-packages]
  (let [ignored-packages (into #{} ignored-packages)
        ns-decls (find-ns-decls-in-dir base-dir)
        declared-packages (into #{} (map (comp get-package get-ns-name) ns-decls))
        uses (reduce concat [] (map (partial get-ns-libs :use) ns-decls))
        used-packages (into #{} (map get-package uses))
        requires (reduce concat [] (map (partial get-ns-libs :require) ns-decls))
        required-packages (into #{} (map get-package requires))
        imports (reduce concat [] (map (partial get-ns-libs :import) ns-decls))
        imported-packages (into #{} (map get-package imports))]
;    (println "declared: " \newline (with-out-str (pprint (sort declared-packages))))
;    (println "used: " \newline (with-out-str (pprint (sort used-packages))))
;    (println "required: " \newline (with-out-str (pprint (sort required-packages))))
;    (println "ignored: " \newline (with-out-str (pprint (sort ignored-packages))))
;    (println "imported: " \newline (with-out-str (pprint (sort imported-packages))))
    (for [ns (concat used-packages required-packages imported-packages)
          :when (and ns
                     (not (.startsWith ns "java."))
                     (not (declared-packages ns))
                     (not (ignored-packages ns))
                     (not (.contains manifest ns)))]
      ns)))
