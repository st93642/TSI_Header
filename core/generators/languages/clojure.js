/**
 * Clojure Code Base Generator
 * Generates boilerplate code for Clojure projects
 */

function generateClojureCodeBase() {
    return `;; Basic Clojure program

;; Namespace declaration
(ns tsi-application.core
  (:gen-class))

;; Main function
(defn -main
  "Main entry point"
  [& args]
  (println "Hello, World!")
  (println "This is a basic Clojure program.")

  ;; Run the application
  (run-app))

;; Application logic
(defn run-app
  "Run the main application logic"
  []
  (let [message "Welcome to TSI!"
        version "1.0"]

    (println message)
    (println (str "Version: " version))

    ;; Example with sequences
    (let [languages ["Clojure" "Scala" "Java"]]
      (println "Languages:")
      (doseq [lang languages]
        (println (str "  " lang))))

    ;; Example with maps
    (let [config {:debug true
                  :port 8080
                  :features ["logging" "caching"]}]
      (println "Configuration:")
      (println (str "  Debug: " (:debug config)))
      (println (str "  Port: " (:port config))))))

;; Utility functions
(defn greet
  "Greet a person"
  [name]
  (str "Hello, " name "!"))

(defn get-info
  "Get application info"
  []
  {:name "TSI Application"
   :version "1.0"
   :author "TSI Student"})

;; Example usage (uncomment to test)
;; (println (greet "TSI Student"))
;; (println (get-info))`;
}

module.exports = {
    generateClojureCodeBase
};