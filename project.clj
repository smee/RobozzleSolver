(defproject robozzleGA "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0-alpha4"]
                 [incanter/incanter-charts "1.5.5"
                  :exclusions [incanter/jfreechart]]
                 [chart-utils "1.1.0-SNAPSHOT"
                  :exclusions [[org.jzy3d/jzy3d-api]
                               [org.jzy3d/jzy3d-jdt-core]]]])
