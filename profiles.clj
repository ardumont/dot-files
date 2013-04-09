{:user {:plugins [[lein-swank                "1.4.4"]
                  [lein-ritz                 "0.7.0"]
                  [lein-cljs                 "0.2.2"]
                  [lein-difftest             "1.3.7"]
                  [lein-marginalia           "0.7.1"]
                  [lein-pprint               "1.1.1"]
                  [lein-midje                "2.0.4"]
                  [lein-noir                 "1.2.1"]
                  [com.stuartsierra/lazytest "1.2.3"]
                  [com.palletops/pallet-lein "0.6.0-beta.7"]
                  ;;            [pallet/lein-template      "0.2.10"]
                  [hiccup-bridge             "1.0.0-SNAPSHOT"]
                  [lein-cljsbuild            "0.3.0"]
                  [lein-droid                "0.1.0-beta6"]
                  [lein-pedantic             "0.0.5"]
                  [lein-outdated             "1.0.0"]
                  ]
        :pallet {:dependencies [[org.virtualbox/vboxjws "4.2.6"]]
                 :source-paths ["src"] :resource-paths []}
        :dependencies [[ritz/ritz-nrepl-middleware "0.7.0"]
                       [clojure-complete           "0.2.2"]]
        :repl-options {:nrepl-middleware
                       [ritz.nrepl.middleware.javadoc/wrap-javadoc
                        ritz.nrepl.middleware.simple-complete/wrap-simple-complete]}
        :search-page-size "30"
        :repositories {"stuart"              "http://stuartsierra.com/maven2"
                       "googleapis"          "http://mavenrepo.google-api-java-client.googlecode.com/hg/"
                       "google-api-services" "http://google-api-client-libraries.appspot.com/mavenrepo"
                       "sonatype"            "http://oss.sonatype.org/content/repositories/releases"
                       "sonatype-snapshots"  "http://oss.sonatype.org/content/repositories/snapshots"}}}
