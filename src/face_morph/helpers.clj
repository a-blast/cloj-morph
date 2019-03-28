(ns face-morph.helpers
  (:require [clojure.string :as str])
  (:require [clojure.pprint :refer (cl-format)])
  ;; (:require [clojure.math.combinatorics :as math])
  (:require [uncomplicate.neanderthal.linalg :as linear])
  (:require [uncomplicate.neanderthal.core :as nd])
  (:require [uncomplicate.neanderthal.aux :as naux])
  (:require [uncomplicate.neanderthal.native :as na])
  (:require [clojure.core.matrix :as m])
  (:require [taoensso.tufte :as tufte :refer (defnp p profiled profile)])
  ;; (:require [clojure.core.matrix.linear :as l])
  (:require [mikera.image.core :as img])
  (:require clojure.core)
  ;;  (:load "imgOperations")
  (:require [clojure.edn :as edn]))

(defn readPoints
  [filePath partitionSize]
  (partition partitionSize
             (map edn/read-string (str/split (slurp filePath) #"\n| "))))

(defn weightedAvgCoordinates
  [alpha point1 point2]
  (map (fn [val1 val2]
         (+ (* (- 1 alpha) val1)
            (* alpha val2)))
       [(nth point1 0) (nth point1 1)]
       [(nth point2 0) (nth point2 1)]))

(defn avgPoints
  [points1 points2 alpha]
  (map #(weightedAvgCoordinates alpha %1 %2) points1 points2))

(defn getTrianglePoints
  [pointPath triangleDefPath]
  (map
   #(map (partial nth (readPoints pointPath 2)) %)
   (readPoints triangleDefPath 3)))

(defn getBoundsAroundTri
  "get bounds for bounding box"
  [trianglePoints]
  (hash-map
   :minX (reduce min (map first trianglePoints))
   :maxX (reduce max (map first trianglePoints))
   :minY (reduce min (map last trianglePoints))
   :maxY (reduce max (map last trianglePoints))))

(defn getTriAffineTransform
  "Get the transformation matrix needed to transform inputTri to outputTri
  @inputTri: a seq of x,y coordinate points
  @outputTri: a seq of x,y coordinate points
  @returns: a transformation matrix that takes inputTri to output"
  [inputTri outputTri]
  (let [addZAxis (partial map #(concat % `(1)))
        matrixForms (map (comp nd/trans na/dge addZAxis) [inputTri outputTri])
        inversePoints (linear/tri (linear/trf (first matrixForms)))]
    (nd/mm (last matrixForms) inversePoints)))

;; Helper function to print a hex string from a number
(def hexOut (partial cl-format nil "~x"))

(defn get2ByteValue
  "Removes a secified range of bytes from an input
     assumes a 8 byte value is passed."
  [hexValue startByte]
  (reduce #(bit-clear %1 %2)
          (bit-shift-right hexValue (* 4 startByte))
          (range 8 32)))

(defn getRGBA
  "Takes a hex color code and converts it to a RGBA value"
  [hexColor]
  (map (partial get2ByteValue hexColor) [0 2 4 6]))

(defn getHexColor
  "Takes an RGBA seq and retuns the single hex representation"
  [R G B A]
  (let [out 0]
    (reduce #(bit-or (int %2) (bit-shift-left (int %1) 8)) [A B G R])))

(defnp isInTri
  "Filtering function to determine if a given coordinate pair exists within the
  bounds of a bounding triangle"
  [triVerticies pointCords]
  (let [a (nth triVerticies 0)
        b (nth triVerticies 1)
        c (nth triVerticies 2)
        as_x (- (first pointCords) (first a))
        as_y (- (last pointCords) (last a))
        s_ab (> (- (* (- (first b) (first a)) as_y)
                   (* (- (last b) (last a)) as_x)) 0)]
    (if (or (< (first pointCords) 0) (< (last pointCords) 0))
      (println ["ERROR!!:" pointCords]))
    (if (= (> (- (* (- (first c) (first a)) as_y)
                 (* (- (last c) (last a)) as_x)) 0) s_ab)
      false
      (= (> (- (* (- (first c) (first b)) (- (last pointCords) (last b)))
               (* (- (last c) (last b)) (- (first pointCords) (first b)))) 0) s_ab))))
