;;;=====================================================================
;;; MUSIC-CYPHER  --  Text <-> Melodie Chiffre fuer OpenMusic
;;; Nicholas Acher Bonfiglio -- HfM Karlsruhe
;;;
;;; Diese Datei enthaelt nur die Zeichen-Ebene der Chiffre.
;;; Die musikalische Rechnung (Tonklasse, Dauer, Schluessel) passiert
;;; im Patch  music-cypher.omp  mit OM-Boxen.
;;;
;;; Verfahren: "pitch + duration pair"
;;;   Jedes Zeichen hat einen Index i im Alphabet (siehe *MC-ALPHABET*):
;;;     Tonklasse = (mod i 12)      -> Position innerhalb der Gruppe
;;;     Dauer     = (floor i 12)    -> Gruppe 0/1/2 aus *MC-DURATIONS*
;;;     Pause     = Leerzeichen
;;;     KEY       = Caesar-Verschiebung der Tonklassen (0-11)
;;;
;;; Bijektiv und oktavunabhaengig: die Melodie darf oktaviert werden.
;;;
;;; Laden:  OM-Menue  File > Load...   ODER  im Listener:
;;;         (load "/Users/nascbon/Documents/HFM/LISP/music-cypher/music-cypher.lisp")
;;;         ZUERST laden, DANN music-cypher.omp oeffnen.
;;;
;;; Boxen fuer das Patch : MC-TEXT->CODES  MC-SCORE->NOTES  MC-CODES->TEXT
;;; Boxen als Abkuerzung : MC-ENCRYPT  MC-DECRYPT  MC-TABLE
;;;=====================================================================

(in-package :om)

;;;---------------------------------------------------------------------
;;; 1. Konfiguration
;;;---------------------------------------------------------------------

(defparameter *mc-alphabet*
  '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l    ; Gruppe 0 -> kurz
    #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x    ; Gruppe 1 -> mittel
    #\y #\z #\. #\, #\! #\? #\' #\-)                   ; Gruppe 2 -> lang
  "32 Zeichen. Position in dieser Liste = Index (Code) des Zeichens.")

(defparameter *mc-durations* '(250 500 1000)
  "Dauern (ms) der drei Gruppen. Bei Tempo 60: 16tel / 8tel / Viertel.")

(defparameter *mc-base-pitch* 6000
  "Grundton in Midicents (6000 = C4). Tonklasse 0 liegt hier.")

(defparameter *mc-rest-dur* 250
  "Dauer der Pause, die ein Leerzeichen darstellt.")

;;;---------------------------------------------------------------------
;;; 2. Kern (reines Common Lisp)
;;;---------------------------------------------------------------------

(defun mc-as-string (x)
  "Macht aus String / Symbol / Liste von Zeilen einen String.
   (Die TEXT-VIEW-Box in OM liefert direkt einen String.)"
  (cond ((stringp x) x)
        ((null x) "")
        ((symbolp x) (string-downcase (symbol-name x)))
        ((listp x) (format nil "~{~a~^ ~}" (mapcar #'mc-as-string x)))
        (t (format nil "~a" x))))

(defun mc-char->index (ch)
  "Index eines Zeichens im Alphabet, NIL wenn unbekannt."
  (position (char-downcase ch) *mc-alphabet* :test #'char=))

(defun mc-index->char (i)
  "Zeichen zu einem Index. Unbekannte Indizes -> #\? "
  (if (and (integerp i) (>= i 0) (< i (length *mc-alphabet*)))
      (nth i *mc-alphabet*)
    #\?))

(defun mc-index->pitch (i key)
  "Index -> Midicent."
  (+ *mc-base-pitch* (* 100 (mod (+ (mod i 12) key) 12))))

(defun mc-index->dur (i)
  "Index -> Dauer der zugehoerigen Gruppe."
  (nth (min (floor i 12) (1- (length *mc-durations*))) *mc-durations*))

(defun mc-space-p (ch)
  (member ch '(#\Space #\Tab #\Newline #\Return) :test #'char=))

(defun mc-nearest-group (d unit)
  "Welcher Gruppe (0,1,2) entspricht die Dauer D am ehesten?
   UNIT = Dauer der kuerzesten Stufe (skaliert *MC-DURATIONS*)."
  (let ((scale (/ (float unit) (float (first *mc-durations*))))
        (best 0) (dist most-positive-fixnum))
    (loop for x in *mc-durations*
          for k from 0
          do (let ((diff (abs (- d (* x scale)))))
               (when (< diff dist) (setf dist diff best k))))
    best))

(defun mc-flat1 (l)
  "((6000) (6200)) -> (6000 6200); flache Listen bleiben unveraendert."
  (mapcar #'(lambda (x) (if (listp x) (first x) x)) l))

;;; --- Text -> Zahlen ---

(defun mc-text->data (text &optional (key 0))
  "TEXT -> (values pitches onsets durs). Nur fuer MC-ENCRYPT gebraucht."
  (let ((pitches nil) (onsets nil) (durs nil) (time 0))
    (loop for ch across (mc-as-string text) do
          (if (mc-space-p ch)
              (incf time *mc-rest-dur*)
            (let ((i (mc-char->index ch)))
              (when i
                (push (mc-index->pitch i key) pitches)
                (push time onsets)
                (let ((d (mc-index->dur i)))
                  (push d durs)
                  (incf time d))))))
    (values (nreverse pitches)
            (append (nreverse onsets) (list time))
            (nreverse durs))))

(defun mc-text->codes-fun (text)
  "TEXT -> (values codes onsets). CODES sind die Alphabet-Indizes der
   Noten (ohne Leerzeichen), ONSETS die Anfangszeiten in ms inklusive
   der Pausen; ONSETS ist um einen Wert laenger (Gesamtdauer)."
  (let ((codes nil) (onsets nil) (time 0))
    (loop for ch across (mc-as-string text) do
          (if (mc-space-p ch)
              (incf time *mc-rest-dur*)
            (let ((i (mc-char->index ch)))
              (when i
                (push i codes)
                (push time onsets)
                (incf time (mc-index->dur i))))))
    (values (nreverse codes)
            (append (nreverse onsets) (list time)))))

;;; --- Zahlen -> Text ---

(defun mc-codes->text-fun (codes &optional gaps)
  "CODES (Alphabet-Indizes) + GAPS (Anzahl Leerzeichen vor jeder Note)
   -> String. GAPS darf NIL sein."
  (let ((out (make-string-output-stream)))
    (loop for c in codes
          for k from 0
          do (let ((g (if gaps (or (nth k gaps) 0) 0)))
               (dotimes (n (max 0 g)) (write-char #\Space out)))
             (write-char (mc-index->char (if (numberp c) (round c) c)) out))
    (get-output-stream-string out)))

(defun mc-score->notes-fun (pitches onsets durs unit)
  "-> (values pitch-classes duration-groups gaps)"
  (let* ((u (if (and unit (> unit 0)) unit (if durs (reduce #'min durs) 250)))
         (rest-u (* *mc-rest-dur* (/ (float u) (float (first *mc-durations*)))))
         (pcs nil) (groups nil) (gaps nil) (prev-end nil))
    (loop for p in pitches
          for d in durs
          for on = (if onsets (pop onsets) (or prev-end 0))
          do (push (mod (round (- p *mc-base-pitch*) 100) 12) pcs)
             (push (mc-nearest-group d u) groups)
             (push (if (and prev-end (> rest-u 0))
                       (max 0 (round (- on prev-end) rest-u))
                     0)
                   gaps)
             (setf prev-end (+ on d)))
    (values (nreverse pcs) (nreverse groups) (nreverse gaps))))

(defun mc-obj->lists (self)
  "CHORD-SEQ / VOICE / Liste -> (values pitches onsets durs)."
  (cond ((and (find-class 'chord-seq nil) (typep self 'chord-seq))
         (values (mc-flat1 (lmidic self)) (lonset self) (mc-flat1 (ldur self))))
        ((listp self) (values (mc-flat1 self) nil nil))
        (t (values nil nil nil))))

;;;---------------------------------------------------------------------
;;; 3. OM-Boxen fuer das Patch
;;;---------------------------------------------------------------------

(defmethod! mc-text->codes ((text t))
  :initvals '("bach ist gut")
  :indoc '("Text: String, Symbol oder Liste von Zeilen")
  :numouts 2
  :doc "MUSIC-CYPHER : Text -> Zahlen.

Zerlegt den Text in Alphabet-Indizes (Codes).

Ausgang 1 : CODES   - ein Index 0..31 pro Zeichen (Leerzeichen erzeugen
                      keine Note, sondern eine Luecke)
Ausgang 2 : ONSETS  - Anfangszeiten in ms, Pausen eingerechnet
                      (ein Wert mehr als Codes = Gesamtdauer)

Aus den Codes rechnet das Patch mit OM-Boxen weiter:
  Code -> OM// 12 -> Rest   = Tonklasse -> (+ Key) -> OM-MOD 12 -> *100 -> +6000
                  -> Quotient = Gruppe   -> POSN-MATCH (250 500 1000) -> Dauer

Zeichen ausserhalb von *MC-ALPHABET* werden uebersprungen."
  (multiple-value-bind (codes onsets) (mc-text->codes-fun text)
    (values codes onsets)))

(defmethod! mc-score->notes ((self t) (unit number))
  :initvals '(nil 250)
  :indoc '("VOICE oder CHORD-SEQ mit der verschluesselten Melodie"
           "Dauer der kuerzesten Notenstufe in ms (250 = 16tel bei Tempo 60)")
  :numouts 3
  :doc "MUSIC-CYPHER : Melodie -> Zahlen.

Liest eine VOICE oder CHORD-SEQ aus und liefert

Ausgang 1 : Tonklassen   (0-11, Oktave wird ignoriert)
Ausgang 2 : Dauergruppen (0 = kurz, 1 = mittel, 2 = lang)
Ausgang 3 : GAPS - wie viele Leerzeichen vor jeder Note stehen
                   (aus den Pausen der Melodie)

Das Patch rechnet daraus mit OM-Boxen den Code zurueck:
  Tonklasse -> (- Key) -> OM-MOD 12 -> pc
  Gruppe -> OM* 12 -> OM+ pc -> Code -> MC-CODES->TEXT

UNIT ist die Dauer der kuerzesten Notenstufe. Bei Tempo 60 und der
Standardtabelle ist das 250 ms (16tel). Spielst du die Melodie in einem
anderen Tempo, passe UNIT im gleichen Verhaeltnis an."
  (multiple-value-bind (p o d) (mc-obj->lists self)
    (multiple-value-bind (pcs groups gaps) (mc-score->notes-fun p o d unit)
      (values pcs groups gaps))))

(defmethod! mc-codes->text ((codes list) (gaps list))
  :initvals '(nil nil)
  :indoc '("Liste von Alphabet-Indizes" "Leerzeichen vor jeder Note (darf leer sein)")
  :numouts 1
  :doc "MUSIC-CYPHER : Zahlen -> Text.

Setzt aus den Codes und den Wortluecken wieder einen String zusammen.
Codes ausserhalb des Alphabets werden zu '?'."
  (mc-codes->text-fun codes gaps))

;;;---------------------------------------------------------------------
;;; 4. Abkuerzungen (eine Box statt der ganzen Kette)
;;;---------------------------------------------------------------------

(defmethod! mc-encrypt ((text t) &optional (key 0))
  :initvals '("bach" 0)
  :indoc '("Text" "Schluessel: Verschiebung der Tonklassen, 0-11")
  :numouts 3
  :doc "MUSIC-CYPHER : Text -> Melodie in einem Schritt.

Ausgang 1 : CHORD-SEQ
Ausgang 2 : Liste der Midicents
Ausgang 3 : Liste der Dauern (ms)

Die ausfuehrliche Version derselben Rechnung steht als Boxenkette im
Patch music-cypher.omp."
  (multiple-value-bind (p o d) (mc-text->data text key)
    (values (make-instance 'chord-seq
                           :lmidic (mapcar #'list p)
                           :lonset o
                           :ldur (mapcar #'list d))
            p d)))

(defmethod! mc-decrypt ((self t) &optional (key 0) (unit 250))
  :initvals '(nil 0 250)
  :indoc '("CHORD-SEQ oder VOICE" "Schluessel" "Dauer der kuerzesten Stufe (ms)")
  :numouts 1
  :doc "MUSIC-CYPHER : Melodie -> Text in einem Schritt.

Die Tonhoehe wird modulo Oktave gelesen, die Dauern relativ zu UNIT -
die Melodie darf also oktaviert werden. KEY muss derselbe sein wie beim
Verschluesseln."
  (multiple-value-bind (p o d) (mc-obj->lists self)
    (multiple-value-bind (pcs groups gaps) (mc-score->notes-fun p o d unit)
      (mc-codes->text-fun
       (loop for pc in pcs for g in groups
             collect (+ (* g 12) (mod (- pc key) 12)))
       gaps))))

(defmethod! mc-table ((key integer))
  :initvals '(0)
  :indoc '("Schluessel")
  :numouts 1
  :doc "MUSIC-CYPHER : Codetabelle als Liste (zeichen code midicent dauer).
Wird ausserdem im Listener ausgedruckt."
  (let ((rows (loop for ch in *mc-alphabet*
                    for i from 0
                    collect (list ch i (mc-index->pitch i key) (mc-index->dur i)))))
    (format t "~&--- MUSIC-CYPHER Tabelle (key ~a) ---~%" key)
    (dolist (r rows)
      (format t "  ~a  code ~2d  ->  ~a mc  /  ~a ms~%"
              (first r) (second r) (third r) (fourth r)))
    rows))

;;;---------------------------------------------------------------------
;;; 5. Selbsttest  --  im Listener aufrufen: (mc-test)
;;;---------------------------------------------------------------------

(defun mc-test (&optional (key 0))
  (let ((ok t))
    (dolist (s '("bach"
                 "hallo welt"
                 "the quick brown fox jumps over the lazy dog."
                 "musik ist mathematik!"
                 "nicholas, wie geht's?"))
      ;; Weg 1: die Kette, die auch das Patch benutzt
      (multiple-value-bind (codes onsets) (mc-text->codes-fun s)
        (let* ((pitches (mapcar #'(lambda (c) (mc-index->pitch c key)) codes))
               (durs    (mapcar #'mc-index->dur codes)))
          (multiple-value-bind (pcs groups gaps)
              (mc-score->notes-fun pitches onsets durs 250)
            (let ((back (mc-codes->text-fun
                         (loop for pc in pcs for g in groups
                               collect (+ (* g 12) (mod (- pc key) 12)))
                         gaps)))
              (unless (string= s back) (setf ok nil))
              (format t "~&~:[FAIL~;ok  ~] ~s -> ~s~%" (string= s back) s back))))))
    (format t "~&MC-TEST: ~:[FEHLGESCHLAGEN~;bestanden~]~%" ok)
    ok))
