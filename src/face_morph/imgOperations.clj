(ns face-morph.imgOperations
  (:use quil.core))


(def img (ref nil))

(defn setup []
  (background 0)
  (dosync (ref-set img (load-image "../data/img/hillary_clinton.jpg"))))

(defn getImg
  [imgPath]
  (load-image imgPath)
  )

(defn draw []
  (image @img 0 0))

(defn getBounds []
  (vector (.width @img) (.height @img)))

(defn showImg []
  (def bounds (vector 600 800))
  (defsketch example
    :title "image demo"
    :setup setup
    :draw draw
    :size bounds))
