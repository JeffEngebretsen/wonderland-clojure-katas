(ns alphabet-cipher.coder
  (:require [clojure.string :as string]))

;; ABCDEFGHIJKLMNOPQRSTUVWXYZ
;A abcdefghijklmnopqrstuvwxyz
;B bcdefghijklmnopqrstuvwxyza
;C cdefghijklmnopqrstuvwxyzab
;D defghijklmnopqrstuvwxyzabc
;E efghijklmnopqrstuvwxyzabcd
;F fghijklmnopqrstuvwxyzabcde
;G ghijklmnopqrstuvwxyzabcdef
;H hijklmnopqrstuvwxyzabcdefg
;I ijklmnopqrstuvwxyzabcdefgh
;J jklmnopqrstuvwxyzabcdefghi
;K klmnopqrstuvwxyzabcdefghij
;L lmnopqrstuvwxyzabcdefghijk
;M mnopqrstuvwxyzabcdefghijkl
;N nopqrstuvwxyzabcdefghijklm
;O opqrstuvwxyzabcdefghijklmn
;P pqrstuvwxyzabcdefghijklmno
;Q qrstuvwxyzabcdefghijklmnop
;R rstuvwxyzabcdefghijklmnopq
;S stuvwxyzabcdefghijklmnopqr
;T tuvwxyzabcdefghijklmnopqrs
;U uvwxyzabcdefghijklmnopqrst
;V vwxyzabcdefghijklmnopqrstu
;W wxyzabcdefghijklmnopqrstuv
;X xyzabcdefghijklmnopqrstuvw
;Y yzabcdefghijklmnopqrstuvwx
;Z zabcdefghijklmnopqrstuvwxy
(def alph '(\a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p \q \r \s \t \u \v \w \x \y \z))

(defn- encode-letter [key-letter message-letter]
  (nth (take 26 (drop (.indexOf alph key-letter) (cycle alph))) (.indexOf alph message-letter)))

(defn encode [keyword message]
  (string/join
    (let [padded-key (take (count message) (cycle keyword))]
      (for [i (range (count message))]
        (encode-letter (nth padded-key i)
                       (nth message i))))))

(defn- decode-letter [key-letter encrypted-letter]
  (nth alph (.indexOf (take 26 (drop (.indexOf alph key-letter) (cycle alph))) encrypted-letter)))

(defn decode [keyword message]
  (string/join
    (let [padded-key (take (count message) (cycle keyword))]
      (for [i (range (count message))]
        (decode-letter (nth padded-key i)
                       (nth message i))))))

(defn decipher [cipher message]
  (string/join (first (first (for [i (range 1 (count cipher))
                                   :let [p (partition i (decode message cipher))]
                                   :when (= (first p) (second p))]
                               p)))))
