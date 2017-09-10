(defmulti insert' (fn [e list] (empty? list)))
(defmethod insert' true [e list] [e [] []])
(defmethod insert' false [e [x l r :as all]] (cond (< e x) [x (insert' e l) r]
                                          (> e x) [x l (insert' e r)]
                                          :else (throw (new Exception "Key is already in tree"))))
(defn insert [e l] (try (insert' e l) (catch Exception e l)))
