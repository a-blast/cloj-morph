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
  (def morphData1
    {:triPoints (getTrianglePoints "./data/points/hillary_clinton.points.txt"
                                   "./data/points/tri_new_76points.txt")
     :image (img/load-image "./data/img/hillary_clinton.jpg" )})

  (def morphData2
    {:triPoints (getTrianglePoints "./data/points/ted_cruz.points.txt"
                                   "./data/points/tri_new_76points.txt")
     :image (img/load-image "./data/img/ted_cruz.jpg")})
  (def avgMorph
    {:triPoints (map #(avgPoints %1 %2 0.9)
                     (morphData1 :triPoints) (morphData2 :triPoints))
     :image (img/new-image (reduce max (map img/width [(morphData1 :image) (morphData2 :image)]))
                           (reduce max (map img/height [(morphData1 :image) (morphData2 :image)])))})


  ;; for each triangle definition shared between each triPoints seq,
  ;; find the max & min x/y values to get the largest bounding box

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

  (defn stateTracker
    [function emitState]
    (def state (function))
    (if emitState state))

  (defn scalePixelColor
    "scales the color intensity of the input pixel to match it's expected value for a weightedAvg"
    [color alpha]
    (apply getHexColor (map #(/ (* % alpha) 2) (getRGBA color))))

  (def outImg (img/new-image (+ (img/width imgIn)) (+ (img/height imgIn))))


  (stateTracker (partial + 0) false)
  (doseq [index (range (count (morphData1 :triPoints)))]
    (let [_         (println index)
          __        (img/show outImg)
          box       (nth boundingBoxes index)
          tri       (nth (morphData1 :triPoints) index)
          numberInTri 0
          transform (nth (morphData1 :affineTransforms) index)]
      (doseq [x (range (int (Math/floor (box :xMin))) (int (Math/ceil (box :xMax))))
              y (range (int (Math/floor (box :yMin))) (int (Math/ceil (box :yMax))))]
          (if (isInTri tri (seq [(int x) (int y)]))
            (let [newCords (nd/mm transform (na/dge 3 1 [x y 1]))
                  newX (first (nd/row newCords 0))
                  neyY (first (nd/row newCords 1))]
              (stateTracker (partial + 1 state) false)
              ;; (println [x y newCords])
              (img/set-pixel outImg newX newY
                             (+ (img/get-pixel outImg newX newY) (img/get-pixel imgIn (int x) (int y)))))))
      (let [state (stateTracker (partial + 0 state) true)]
        (do));(if (= state 0) (println ["FAIL!" index box tri transform])
          ;  (println ["PASS!" index box tri transform])))
      (stateTracker (partial + 0) false)))

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
                                       (apply interleave (map getRGBA surroundingPixels))))))))))
        

  (doseq [x (range 0.0 146)
          y (range 363 447)]
    (println [x y])
    (if (isInTri tri (seq [(int x) (int y)]))
      (let [newCords (nd/mm transform (na/dge 3 1 [x y 1]))]
        ;;(stateTracker (partial + 1 state) false)
        (println [x y newCords])
        )))

  ;; (if (isInTri tri (seq [x y]))
  ;;   (let [newCords (nd/mm transform (na/dge 3 1 [x y 1]))]
  ;;     ;; (println [x y newCords])
  ;;     (img/set-pixel outImg
  ;;     (+ (first (nd/row newCords 0)) 250) (+ (first (nd/row newCords 1)) 250)
  ;;     (img/get-pixel imgIn x y)))
  ;;   nil))

  ;; (defn getpoints
  ;;   "Get all points within a boundingBox"
  ;;   [box]
  ;;   (map (fn [x](map (fn [y]
  ;;                      (if (isInTri )))
  ;;                    (range (box :yMin) (box :yMax))))
  ;;        (range (box :xMin) (box :xMax)))

  ;;   )
  ;; (map (map (fn [x](map #(img/get-pixel imgIn x %)
  ;;                       (range (img/height imgIn))))
  ;;           (range (img/width imgIn))))

  ;; (map (fn [x](map #(let [pixel (img/get-pixel imgIn x %)]
  ;;                     ;; pixel is 0 if it has already been ID'ed into a img triangle
  ;;                     (if (!= pixel 0)
  ;;                       ()))

  ;;                  (range (img/height imgIn))))
  ;;      (range (img/width imgIn)))


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
  (def inversePoints (linear/tri (linear/trf (first matrixForms))))
  (nd/mm (last matrixForms) inversePoints))

(def hexOut (partial cl-format nil "~x"))


(defn get2ByteValue
  "Removes a secified range of bytes from an input
     assumes a 8 byte value is passed."
  [hexValue startByte]
  (reduce #(bit-clear %1 %2)
          (bit-shift-right hexValue (* 4 startByte))
          (range 8 32))
  )
(defn getRGBA
  "Takes a hex color code and converts it to a RGBA value"
  [hexColor]
  (map (partial get2ByteValue hexColor) [0 2 4 6])
  )

(defn getHexColor
  [R G B A]
  (let [out 0]
    (reduce #(bit-or (int %2) (bit-shift-left (int %1) 8)) [A B G R])))

;; (defn getLineSegments
;;   "Gets all possible line segments from a sequence of points"
;;   [pointSeq]
;;   ;;(def pointQueue (reduce conj clojure.lang.PersistentQueue/EMPTY pointSeq))
  
;;   (def x (map #(concat % `(1 1))
;;               (concat (map #(concat [(first (first tri))] [(first %)]) (rest tri))
;;                       (map #(concat [(first (nth tri 1))] [(first %)]) [(last tri)]))))
;;   (def xMat (map #(na/dge 2 2 %) x))
;;   (def y (concat (map #(concat [(last (first tri))] [(last %)]) (rest tri))
;;                  (map #(concat [(last (nth tri 1))] [(last %)]) [(last tri)])))
;;   (def yMat (map #(na/dge 2 1 %) y))
;;   (map (partial apply linear/sv) (partition 2 (interleave xMat yMat))))

(defn isInTri
  "Filtering function to determine if a given coordinate pair exists within the
  bounds of a bounding triangle"
  [triVerticies pointCords]
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
    (map #(replace (vec (concat triVerticies [pointCords])) %)
         `((0 1 3) (0 3 2) (3 1 2))))

  (def newCords
    (map #(apply interleave %) newTriangles))
  (def newCords
    (map #(zipmap [:x1 :x2 :x3 :y1 :y2 :y3] %) newCords))

  (= (reduce + (map calcArea newCords)) area))


(defn morphTriangle
  [triangle1 triangle2]


  )
