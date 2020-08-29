(defproject adxlib "0.1.0-SNAPSHOT"
  :description "Library for generating adx files"
  :url ""
  :license {:name "GNU General Public License version 3 or later, with Clojure exception"
            :url ""}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [dataorigin/dataorigin "0.1.0-SNAPSHOT"]]
  :aot [adxlib.core])
