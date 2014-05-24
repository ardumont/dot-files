{:user {:plugins [
                  [lein-swank                "1.4.4"]
                  [lein-ritz                 "0.7.0"]
                  [lein-cljs                 "0.2.2"]
                  [lein-difftest             "1.3.7"]
                  [lein-marginalia           "0.7.1"]
                  [lein-pprint               "1.1.1"]
                  [lein-midje                "3.0.1"]
                  [lein-noir                 "1.2.1"]
                  [com.palletops/pallet-lein "0.6.0-beta.7"]
                  [pallet/lein-template      "0.2.10"]
                  [lein-cljsbuild            "0.3.0"]]
        :pallet {:dependencies [[org.virtualbox/vboxjws "4.2.6"]]
                 :source-paths ["src"] :resource-paths []}
        :dependencies [[clojure-complete            "0.2.3"]
                       [leiningen                   #=(leiningen.core.main/leiningen-version)]
                       [im.chit/vinyasa             "0.2.0"]
                       [org.clojure/tools.namespace "0.2.4"]
                       [expectations                "2.0.7"]]
        :injections [(require '[vinyasa.inject :as inj])
                     (inj/inject 'clojure.core '>
                                 '[[vinyasa.inject inject]
                                   [vinyasa.pull pull]
                                   [vinyasa.lein lein]
                                   [vinyasa.reimport reimport]
                                   [cemerick.pomegranate add-classpath get-classpath resources]
                                   [clojure.tools.namespace.repl refresh]
                                   ;; [clojure.repl apropos dir doc find-doc source pst
                                   ;;  [root-cause >cause]]
                                   ;; [clojure.pprint pprint]
                                   [clojure.java.shell sh]])]
        :search-page-size "30"}
 :repositories {"stuart"              "http://stuartsierra.com/maven2"
                "googleapis"          "http://mavenrepo.google-api-java-client.googlecode.com/hg/"
                "google-api-services" "http://google-api-client-libraries.appspot.com/mavenrepo"
                "sonatype"            "http://oss.sonatype.org/content/repositories/releases"
                "sonatype-snapshots"  "http://oss.sonatype.org/content/repositories/snapshots"}}
