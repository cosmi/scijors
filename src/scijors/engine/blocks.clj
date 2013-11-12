(ns scijors.engine.blocks
  (:use [scijors.engine.elements :only [elements-grammar]]
        [scijors.engine.variables]
        [scijors.engine.expr])
  (:require [instaparse.core :as insta]
            [clojure.string :as strings]))


(def blocks-grammar
