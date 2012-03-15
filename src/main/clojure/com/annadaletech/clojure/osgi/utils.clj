(ns com.annadaletech.clojure.osgi.utils
  (:use
    clojure.java.io
    clojure.pprint
    clojure.tools.namespace
    )
  (:import
    [java.util Map HashMap]
    [java.util.jar Manifest]
    [aQute.libg.header OSGiHeader]
    [aQute.lib.osgi Analyzer Processor Jar]
    [java.io IOException])
  )


(def IMPORT-PACKAGE "Import-Package")
(def EXPORT-PACKAGE "Export-Package")

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

(defn get-manifest-attr
  "Returns a seq of packages (Strings) from the given manifest Import-Package header"
  [^Manifest manifest attr]
  (-> manifest (.getMainAttributes) (.getValue attr)))

(defn get-osgi-header-packages
  ""
  [header]
  (seq (.keySet (OSGiHeader/parseHeader header))))

(defn find-missing-imports
  "Finds packages in use/require/import declarations in clojure source files in base-dir
   and checks that all are present in the manifest file.
   manifest will be coerced to an InputStream using java.io.input-stream
   Takes seq of ignored-packages (Strings) to exclude certain packages from consideration.
   Also excludes any packages provided by the clojure source files in base-dir.
   "
  [base-dir ^Manifest manifest ignored-packages]
  (let [manifest-imports (into #{} (get-osgi-header-packages (get-manifest-attr manifest IMPORT-PACKAGE)))
        ignored-packages (into #{} ignored-packages)
        ns-decls (find-ns-decls-in-dir base-dir)
        declared-packages (into #{} (map (comp get-package get-ns-name) ns-decls))
        uses (reduce concat [] (map (partial get-ns-libs :use) ns-decls))
        used-packages (map get-package uses)
        requires (reduce concat [] (map (partial get-ns-libs :require) ns-decls))
        required-packages (map get-package requires)
        imports (reduce concat [] (map (partial get-ns-libs :import) ns-decls))
        imported-packages (map get-package imports)]
    (for [ns (sort (into #{} (concat ["clojure"] used-packages required-packages imported-packages)))
          :when (and ns
                     (not (.startsWith ns "java."))
                     (not (declared-packages ns))
                     (not (ignored-packages ns))
                     (not (manifest-imports ns)))]
      (namespace-munge ns))))

(defn find-missing-exports
  "Finds decsared namespaces in clojure source files in base-dir
   and checks that all are present in the manifest file.
   manifest will be coerced to an InputStream using java.io.input-stream
   Takes seq of ignored-packages (Strings) to exclude certain packages from consideration.
   "
  [base-dir ^Manifest manifest ignored-packages]
  (let [manifest-exports (into #{} (get-osgi-header-packages (get-manifest-attr manifest EXPORT-PACKAGE)))
        ignored-packages (into #{} ignored-packages)
        ns-decls (find-ns-decls-in-dir base-dir)
        declared-packages (into #{} (map (comp get-package get-ns-name) ns-decls))]
    (for [ns (sort declared-packages)
          :when (and ns
                     (not (ignored-packages ns))
                     (not (manifest-exports ns)))]
      (namespace-munge ns))))

(defn add-packages
  "Adds given imported packages to the manifest's Import-Package attribute."
  [^Manifest manifest header-name packages]
  (let [header (OSGiHeader/parseHeader (get-manifest-attr manifest header-name))
        manifest (Manifest. manifest)]
    (doseq [package packages]
      (.put header package (HashMap.)))
    (-> manifest
      (.getMainAttributes)
      (.putValue header-name (Processor/printClauses header)))
    manifest))

(defn add-package-imports
  "Adds packages to the manifest's Import-Package attribute."
  [^Manifest manifest new-imports]
  (add-packages manifest IMPORT-PACKAGE new-imports))

(defn add-package-exports
  "Adds packages to the manifest's Export-Package attribute."
  [^Manifest manifest new-exports]
  (add-packages manifest EXPORT-PACKAGE new-exports))


(defn read-manifest
  "Reads a Manifest from the given file (or filename) f"
  [f]
  (Manifest. (input-stream f)))

(defn write-manifest
  "Write the given manifest object to the file (or filename) f"
  [^Manifest manifest f]
  (with-open [os (output-stream f)]
    (Jar/writeManifest manifest os)))
