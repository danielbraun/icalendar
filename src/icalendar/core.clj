(ns icalendar.core
  (:require [clojure [walk :as walk] [string :as string]]
            compojure.response
            [clj-time [coerce :as tc] [core :as t] [format :as tf]]))

(def example-cal
  [:vcalendar {:version 2.0
               :prodid "//hacksw/handcal//NONSGML v1.0//EN"}
   [:event {:dtstart (java.util.Date.)
            :dtend (java.util.Date.)
            :summary "Bastille Day Party 2"}]
   [:event {:dtstart (java.util.Date.)
            :dtend (java.util.Date.)
            :summary "Bastille Day Party 3"}]])

(def default-cal-attrs
  {:version 2.0
   :prodid "//hacksw/handcal//NONSGML v1.0//EN"})

(defmulti as-icalendar type)

(defmethod as-icalendar :default [x] x)

(defmethod as-icalendar clojure.lang.PersistentVector [form]
  (let [[el attrs & children] form]
    (if (map? attrs)
      (concat (into [["BEGIN" el]] attrs)
              (apply concat children)
              [["END" el]])
      form)))

(defmethod as-icalendar java.util.Date [d]
  (as-icalendar (tc/to-date-time d)))

(let [formatter (tf/formatter "yyyyMMdd'T'HHmmss'Z'")]
  (defmethod as-icalendar org.joda.time.DateTime [dt]
    (tf/unparse formatter dt)))

(defmethod as-icalendar clojure.lang.Keyword [kw]
  (string/upper-case (name kw)))

(do
  (defrecord iCalendar [events]
    Object
    (toString [this]
      (->> events
           (map (partial vector :vevent))
           (into [:vcalendar (-> this
                                 (merge default-cal-attrs)
                                 (dissoc :events))])
           (walk/postwalk as-icalendar)
           (map (partial string/join ":"))
           (string/join "\r\n")))
    compojure.response/Renderable
    (render [this req]
      {:body (str this)
       :status 200
       :headers {"Content-Type" "text/calendar"}}))

  (let [cal
        (assoc (->iCalendar [{:dtstart (java.util.Date.)
                              :dtend (java.util.Date.)
                              :summary "Bastille Day Party 2"}])
               :X-WR-CALNAME "test-name")]
    (str cal)))
