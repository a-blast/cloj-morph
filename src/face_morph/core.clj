(ns face-morph.core
  (:require [clojure.string :as str])
  (:require [clojure.pprint :refer (cl-format)])
  (:require [uncomplicate.neanderthal.linalg :as linear])
  (:require [uncomplicate.neanderthal.core :as nd])
  (:require [uncomplicate.neanderthal.aux :as naux])
  (:require [uncomplicate.neanderthal.native :as na])
  (:require [clojure.core.matrix :as m])
  (:require [taoensso.tufte :as tufte :refer (defnp p profiled profile)])
  (:require [mikera.image.core :as img])
  (:require clojure.core)
  (:use face-morph.helpers)
  (:require [clojure.edn :as edn])
  (:gen-class))

(defn -main
  [& args]
  (tufte/add-basic-println-handler! {})

  (def morphData1
    {:triPoints (getTrianglePoints "./data/points/hillary_clinton.points.txt"
                                   "./data/points/tri_new_76points.txt")
     :image (img/load-image "./data/img/hillary_clinton.jpg" )})

  (def morphData2
    {:triPoints (getTrianglePoints "./data/points/ted_cruz.points.txt"
                                   "./data/points/tri_new_76points.txt")
     :image (img/load-image "./data/img/ted_cruz.jpg")})

  (defn initVariables
    [alpha]

    (def avgMorph
      {:triPoints (map #(avgPoints %1 %2 alpha)
                       (morphData1 :triPoints) (morphData2 :triPoints))
       :image (img/new-image (reduce max (map img/width [(morphData1 :image) (morphData2 :image)]))
                             (reduce max (map img/height [(morphData1 :image) (morphData2 :image)])))})

    (def findMaxPoint #(reduce max %))
    (def findMinPoint #(reduce min %))
    (def getX #(map (partial map first) %))
    (def getY #(map (partial map last) %))


    (def xVals
      (partition 3 (apply interleave
                          (map (comp getX #(% :triPoints))
                               [morphData1 morphData2 avgMorph]))))
    (def yVals
      (partition 3 (apply interleave
                          (map (comp getY #(% :triPoints))
                               [morphData1 morphData2 avgMorph]))))

    (def maxY (map #(findMaxPoint (flatten %)) yVals))
    (def maxX (map #(findMaxPoint (flatten %)) xVals))
    (def minY (map #(findMinPoint (flatten %)) yVals))
    (def minX (map #(findMinPoint (flatten %)) xVals))

    (def boundingBoxes (map (partial zipmap [:xMax :yMax :xMin :yMin])
                            (partition 4 (apply interleave [maxX maxY minX minY]))))

    (def morphData1 (merge morphData1
                           {:affineTransforms
                            (map #(getTriAffineTransform %1 %2)
                                 (morphData1 :triPoints)
                                 (avgMorph :triPoints))}))

    (def morphData2 (merge morphData2
                           {:affineTransforms
                            (map #(getTriAffineTransform %1 %2)
                                 (morphData2 :triPoints)
                                 (avgMorph :triPoints))})))

  (defn scalePixelColor
    "scales the color intensity of the input pixel to match it's expected value for a weightedAvg"
    [color alpha]
    (apply getHexColor (map #(* % alpha) (getRGBA color))))


  (defnp applyTransformation
    [data outImg alpha]
    (let [imgIn  (data :image)]
      (doseq [index (range (count (data :triPoints)))]
        (let [;;_           (println index)
              ;;__          (img/show outImg)
              box         (nth boundingBoxes index)
              tri         (nth (data :triPoints) index)
              numberInTri 0
              transform   (nth (data :affineTransforms) index)]
          (doseq [x (range (int (Math/floor (box :xMin))) (int (Math/ceil (box :xMax))))
                  y (range (int (Math/floor (box :yMin))) (int (Math/ceil (box :yMax))))]
            (p ::isInTriLoop
               (if (isInTri tri (seq [(int x) (int y)]))
                 (let [newCords (nd/mm transform (na/dge 3 1 [x y 1]))
                       newX     (int (Math/ceil (first (nd/row newCords 0))))
                       newY     (int (Math/ceil (first (nd/row newCords 1))))]
                   (if (and (> newX 0) (> newY 0) (< newX (img/width outImg)) (< newY (img/height outImg)))
                     (img/set-pixel outImg newX newY
                                    (scalePixelColor (img/get-pixel imgIn (int x) (int y)) alpha)))
                   )))))))

    (doseq [x (range 1 (- (img/width outImg) 1))
            y (range 1 (- (img/height outImg) 1))]
      (if (= (img/get-pixel outImg x y) 0)
        (let [surroundingPixels (filter (partial not= 0)
                                        (seq [(img/get-pixel outImg (- x 1) (- y 1))
                                              (img/get-pixel outImg (- x 1) y)
                                              (img/get-pixel outImg (- x 1) (+ y 1))
                                              (img/get-pixel outImg x (+ y 1))
                                              (img/get-pixel outImg (+ x 1) (+ y 1))
                                              (img/get-pixel outImg (+ x 1) y)
                                              (img/get-pixel outImg (+ x 1) (- y 1))
                                              (img/get-pixel outImg x (- y 1))]))]
          (img/set-pixel
           outImg x y
           (apply getHexColor (map #(/ % (count surroundingPixels))
                                   (map (partial reduce +)
                                        (partition
                                         (count surroundingPixels)
                                         (apply interleave (map getRGBA surroundingPixels)))))))))))

  ;; iterate over alpha values
  (doseq [alpha (range 0.01 1.0 0.04)]
    (initVariables alpha)
    (def img1 (img/new-image (reduce max (map img/width [(morphData1 :image) (morphData2 :image)]))
                             (reduce max (map img/height [(morphData1 :image) (morphData2 :image)]))))
    (def img2 (img/new-image (reduce max (map img/width [(morphData1 :image) (morphData2 :image)]))
                             (reduce max (map img/height [(morphData1 :image) (morphData2 :image)]))))
    (applyTransformation morphData1 img1 (- 1 alpha))
    (applyTransformation morphData2 img2 alpha)
    (def outImg (img/new-image (reduce max (map img/width [(morphData1 :image) (morphData2 :image)]))
                               (reduce max (map img/height [(morphData1 :image) (morphData2 :image)]))))
    (doseq [x (range (img/width (avgMorph :image)))
            y (range (img/height (avgMorph :image)))]
      (img/set-pixel outImg x y
                     (apply getHexColor
                            (map (partial reduce #(+ %1 %2))
                                 (partition  2 (apply interleave
                                                      (map getRGBA
                                                           [(img/get-pixel img1 x y)
                                                            (img/get-pixel img2 x y)])))))))
    (img/show outImg)
    (img/write outImg (str "./data/out/" alpha ".png") "png")))



