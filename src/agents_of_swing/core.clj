(ns agents-of-swing.core
  (:gen-class))

(import '(javax.swing JPanel JFrame JButton JTextField JLabel Timer SwingUtilities)
        '(java.awt GridBagLayout Insets))

(defn new-flipper []
  (agent {:total 0
          :heads 0 
          :running false
          :random (java.util.Random.)}))

(defn calculate [state]
  (if (:running state)
    (do (send *agent* calculate)
        (assoc state
               :total (inc (:total state))
               :heads (if (.nextBoolean (:random state))
                        (inc (:heads state))
                        (:heads state))))
    state))

(defn start [state]
  (send *agent* calculate)
  (assoc state :running true))

(defn stop [state]
  (assoc state :running false))

(defn error [state]
  (if (zero? (:total state))
    0.0
    (- (/ (double (:heads state))
          (:total state))
       0.5)))

(defn text-field [value]
  (doto (JTextField. value 15)
    (.setEnabled false)
    (.setHorizontalAlignment JTextField/RIGHT)))

(defmacro with-action [component event & body]
  `(. ~component addActionListener
      (proxy [java.awt.event.ActionListener] []
        (actionPerformed [~event] ~@body))))

(defmacro set-grid! [constraints field value]
  `(set! (. ~constraints ~(symbol (name field)))
         ~(if (keyword? value)
            `(. java.awt.GridBagConstraints
                ~(symbol (name value)))
            value)))

(defmacro grid-bag-layout [container & body]
  (let [c (gensym "c")
        cntr (gensym "cntr")]
    `(let [~c (new java.awt.GridBagConstraints)
           ~cntr ~container]
       ~@(loop [result '() body body]
           (if (empty? body)
             (reverse result)
             (let [expr (first body)]
               (if (keyword? expr)
                 (recur (cons `(set-grid! ~c ~expr
                                          ~(second body))
                              result)
                        (next (next body)))
                 (recur (cons `(.add ~cntr ~expr ~c)
                              result)
                        (next body)))))))))

(defn flipper-app2 []
  ;; Construct components:
  (let [flipper (new-flipper)
        b-start (JButton. "Start")
        b-stop (doto (JButton. "Stop")
                 (.setEnabled false))
        total (text-field "0")
        heads (text-field "0")
        t-error (text-field "0.0")
        timer (Timer. 100 nil)]

    ;; Setup actions:
    (with-action timer e
      (let [state @flipper]
        (.setText total (str (:total state)))
        (.setText heads (str (:heads state)))
        (.setText t-error (format "%.10g" (error state)))))
    (with-action b-start e
      (send flipper start)
      (.setEnabled b-stop true)
      (.setEnabled b-start false)
      (.start timer))
    (with-action b-stop e
      (send flipper stop)
      (.setEnabled b-stop false)
      (.setEnabled b-start true)
      (.stop timer))

    ;; Create window and layout:
    (doto (JFrame. "Flipper")
      (.setContentPane
       (doto (JPanel. (GridBagLayout.))
         (grid-bag-layout
          :insets (Insets. 5 5 5 5)

          :gridx 0, :anchor :LINE_END
          :gridy 0, (JLabel. "Total:")
          :gridy 1, (JLabel. "Heads:")
          :gridy 2, (JLabel. "Error:")

          :gridx 1, :anchor :LINE_START
          :gridy 0, total
          :gridy 1, heads
          :gridy 2, t-error

          :gridx 0, :gridy 3, :gridwidth 2, :anchor :CENTER
          (doto (JPanel.)
            (.add b-start)
            (.add b-stop)))))
      (.pack)
      (.setVisible true)
      (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE))))

(defn -main
  "Going through Agents of Swing tutorial. https://stuartsierra.com/2010/01/08/agents-of-swing"
  [& args]
  (SwingUtilities/invokeLater flipper-app2))
