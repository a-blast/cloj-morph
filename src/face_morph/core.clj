(ns face-morph.core
  (:require [clojure.string :as str])
  (:require [clojure.pprint :refer (cl-format)])
  ;; (:require [clojure.math.combinatorics :as math])
  (:require [uncomplicate.neanderthal.linalg :as linear])
  (:require [uncomplicate.neanderthal.core :as nd])
  (:require [uncomplicate.neanderthal.aux :as naux])
  (:require [uncomplicate.neanderthal.native :as na])
  (:require [clojure.core.matrix :as m])
  ;; (:require [clojure.core.matrix.linear :as l])
  (:require [mikera.image.core :as img])
  (:require clojure.core)
  ;;  (:load "imgOperations")
  (:require [clojure.edn :as edn])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (def hillaryTriPoints
    {:triPoints (getTrianglePoints "../../data/points/hillary_clinton.points.txt"
                                   "../../data/points/tri_new_76points.txt")})
  (def tedTriPoints
    {:triPoints (getTrianglePoints "../../data/points/ted_cruz.points.txt"
                                   "../../data/points/tri_new_76points.txt")})
  (def avgTriPoints
    {:triPoints (map #(avgPoints %1 %2 0.5)
                     (hillaryTriPoints :triPoints) (tedTriPoints :triPoints))})

  ;; for each triangle definition shared between each triPoints seq,
  ;; find the max & min x/y values to get the largest bounding box

  (def findMaxPoint #(reduce max %))
  (def findMinPoint #(reduce min %))
  (def getX #(map (partial map first) %))
  (def getY #(map (partial map last) %))

  (def xVals
    (partition 3 (apply interleave
                        (map (comp getX #(% :triPoints))
                             [hillaryTriPoints tedTriPoints avgTriPoints]))))
  (def yVals
    (partition 3 (apply interleave
                        (map (comp getY #(% :triPoints))
                             [hillaryTriPoints tedTriPoints avgTriPoints]))))

  (def maxY (map #(findMaxPoint (flatten %)) yVals))
  (def maxX (map #(findMaxPoint (flatten %)) xVals))
  (def minY (map #(findMinPoint (flatten %)) yVals))
  (def minX (map #(findMinPoint (flatten %)) xVals))

  (map (partial zipmap [:xMax :yMax :xMin :yMin])
       (partition 4 (apply interleave [maxX maxY minX minY]))

  (println "Hello, World!"))


;; TEST VARIABLES FOR REPL
;; (def triPoints
;;   (getTrianglePoints
;;    "./data/points/hillary_clinton.points.txt"
;;    "./data/points/tri_new_76points.txt"))
;; (def tri (first triPoints))
;; (def transformPoints (na/fge (vector (apply concat tri))))
;; (def matOut (getTriAffineTransform tri tri))
;; (def floatMatOut (na/dge matOut))

;; END TEST REPL VARIABLES

(defn applyAffineTransform
[]
)


(defn readPoints
[filePath partitionSize]
(def pointFile (slurp filePath))
(def slicedValues (map edn/read-string (str/split pointFile #"\n| ")))
(partition partitionSize slicedValues))

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
(def points (readPoints pointPath 2))
(def triangleDef (readPoints triangleDefPath 3))
(map
#(map (partial nth points) %)
triangleDef))

(defn getBoundsAroundTri
[trianglePoints]
(def xvals (map first trianglePoints))
  (def yvals (map last trianglePoints))
  (def maxX (reduce max xvals))
  (def minX (reduce min xvals))
  (def maxY (reduce max yvals))
  (def minY (reduce min yvals))

  (hash-map :minX minX :maxX maxX :minY minY :maxY maxY))



;; 1. Get location points of features in both img
;; 2. Calculate the morphed img point locations via weightedAvg
;; 3. Map the triangulation point defs to their actual points
;; 4. Calculate the affine transformation from the three points of each
;;    input img triangle to the wAvg output
;; 5. Calculate a masked bounding box around each triangle
;; 6. Use the affine transforms to transform all pixels inside the triangle to the
;;    Morphed img
;; 7. Construct a 0 matrix of required size to store the morphed output
;; 8. Multiply the morphed outputs by the resppective alpha blending level
;; 9. Add outputs to the output matrix

(def getImgRegion
  [])

(defn getFirstRow
  "Get array row in the form of [x1 y1 1 0 0 0 -x1x2 -y1y2]
    to be joined with getSecondRow & mapped across points in the triangles"
  [inputCordPair transformedCordPair]
  (m/array [[(first inputCordPair) (last inputCordPair) 1 0 0 0
             (* -1 (first inputCordPair) (first transformedCordPair))
             (* -1 (last inputCordPair) (last transformedCordPair))]]))

(defn getSecondRow
  "Get array row in the form of [0 0 0 x1 y1 1 -x1x2 -y1y2]
    to be joined with getFirstRow & mapped across points in the triangles"
  [inputCordPair transformedCordPair]
  (m/array [[0 0 0 (first inputCordPair) (last inputCordPair) 1
             (* -1 (first inputCordPair) (first transformedCordPair))
             (* -1 (last inputCordPair) (last transformedCordPair))]]))

(defn getTriAffineTransform
  "Get the transformation matrix needed to transform inputTri to outputTri
  @inputTri: a seq of x,y coordinate points
  @outputTri: a seq of x,y coordinate points
  @returns: a transformation matrix that takes inputTri to output"
  [inputTri outputTri]
  ;; (def pointRows #(m/join (apply getFirstRow %) (apply getSecondRow %)))
  ;; (def zippedPoints (map vector inputTri outputTri))
  ;; ((comp
  ;;   (partial reduce m/join)
  ;;   (partial map pointRows))
  ;;  zippedPoints)
  (def addZAxis (partial map #(concat % `(1))))
  (def matrixForms (map (comp nd/trans na/dge addZAxis) [inputTri outputTri]))
  (apply linear/sv matrixForms))

(def hexOut (partial cl-format nil "~x"))

(defn getRGBA
  "Takes a hex color code and converts it to a RGBA value"
  [hexColor]
  (map (partial get2ByteValue hexColor) [0 2 4 6])
  )

(defn get2ByteValue
  "Removes a secified range of bytes from an input
     assumes a 8 byte value is passed."
  [hexValue startByte]
  (reduce #(bit-clear %1 %2)
          (bit-shift-right hexValue (* 4 startByte))
          (range 8 32))
  )

(defn getLineSegments
  "Gets all possible line segments from a sequence of points"
  [pointSeq]
  ;;(def pointQueue (reduce conj clojure.lang.PersistentQueue/EMPTY pointSeq))
  
  (def x (map #(concat % `(1 1))
              (concat (map #(concat [(first (first tri))] [(first %)]) (rest tri))
                      (map #(concat [(first (nth tri 1))] [(first %)]) [(last tri)]))))
  (def xMat (map #(na/dge 2 2 %) x))
  (def y (concat (map #(concat [(last (first tri))] [(last %)]) (rest tri))
                 (map #(concat [(last (nth tri 1))] [(last %)]) [(last tri)])))
  (def yMat (map #(na/dge 2 1 %) y))
  (map (partial apply linear/sv) (partition 2 (interleave xMat yMat))))

(defn isInTri
  "Filtering function to determine if a given coordinate pair exists within the
  bounds of a bounding triangle"
  [triBounds triPoint pointCords]
  ;; triBounds -> (def boundaryLines (getLineSegments triVerticies))

  ;; inspect point to see if any line segment from the point
  ;; to a vertex intersects one of the boundary lines.
  ;; (def x (map #(concat % `(1 1))
  ;;             (concat (map #(concat [(first (first pointCords))] [(first %)]) tri))))
  ;; (def y (concat (map #(concat [(last (first pointCords))] [(last %)]) tri)))
  ;; (def xMat (map #(na/dge 2 2 %) x))
  ;; (def yMat (map #(na/dge 2 1 %) y))
  ;; (def pointLines (map (partial apply linear/sv) (partition 2 (interleave xMat yMat))))

  (def cords (apply interleave triVerticies))
  (def cords (zipmap [:x1 :x2 :x3 :y1 :y2 :y3] cords))


  (defn abs [n] (max n (- n)))
  (defn calcArea [cords] (abs (/ (+ (* (cords :x1)
                                       (- (cords :y2) (cords :y3)))
                                    (* (cords :x2)
                                       (- (cords :y3) (cords :y1)))
                                    (* (cords :x3)
                                       (- (cords :y1) (cords :y2)))) 2)))

  (def area (calcArea cords))

  (def newTriangles
    (map #(replace (vec (concat triVerticies pointCords)) %)
         `((0 1 3) (0 3 2) (3 1 2))))

  (def newCords
    (map #(apply interleave %) newTriangles))
  (def newCords
    (map #(zipmap [:x1 :x2 :x3 :y1 :y2 :y3] %) newCords))

  (= (reduce + (map calcArea newCords)) area))


(defn morphTriangle
  [triangle1 triangle2]


  )
