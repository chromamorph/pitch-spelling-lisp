;; This file contains functions for processing pitch representations.(defun chromatic-pitch-chroma (chromatic-pitch)  (mod chromatic-pitch 12))(defun pn2p (pn-as-input)  (if pn-as-input    (let* ((pn (string pn-as-input))           (letter-name (elt pn 0))           (octave-pos (do* ((op 1))                            ((member (elt pn op)                                     '(#\- #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))                             op)                         (setf op (1+ op))))           (inflection (subseq pn 1 octave-pos))           (asa-octave-number (read-from-string (subseq pn octave-pos)))           (morph (position (char-upcase letter-name) '(#\A #\B #\C #\D #\E #\F #\G)))           (undisplaced-chroma (elt '(0 2 3 5 7 8 10) morph))           (displacement (if (or (string= inflection "")                                 (char= #\n (char-downcase (elt inflection 0))))                           0                           (if (or (char= #\f (char-downcase (elt inflection 0)))                                   (char= #\b (char-downcase (elt inflection 0))))                             (- (length inflection))                             (length inflection))))           (morphetic-octave (if (< morph 2)                               asa-octave-number                               (1- asa-octave-number)))           (chromatic-pitch (+ displacement undisplaced-chroma (* 12 morphetic-octave)))           (morphetic-pitch (+ morph (* 7 morphetic-octave))))      (list chromatic-pitch morphetic-pitch))))(defun p2pn (pitch)  (let (generic-chroma undisplaced-chroma displacement inflection-char        morph letter-name inflection asa-octave-number)    (setf morph (mod (elt pitch 1) 7))    (setf letter-name (elt '("A" "B" "C" "D" "E" "F" "G") morph))    (setf generic-chroma (- (elt pitch 0) (* 12 (floor (elt pitch 1) 7))))    (setf undisplaced-chroma (elt '(0 2 3 5 7 8 10) morph))    (setf displacement (- generic-chroma undisplaced-chroma))    (setf inflection "")    (if (/= displacement 0)      (progn         (if (< displacement 0)          (setf inflection-char "f")          (setf inflection-char "s"))        (dotimes (i (abs displacement))          (setf inflection (concatenate 'string inflection inflection-char))))      (setf inflection "n"))    (setf asa-octave-number (floor (elt pitch 1) 7))    (if (> morph 1)      (setf asa-octave-number (1+ asa-octave-number)))    (concatenate 'string letter-name inflection (format nil "~d" asa-octave-number))))(defun pn-tran (pn pin)  (if (and pn pin)      (p2pn (p-tran (pn2p pn) (pin2pi (string-downcase pin))))))(defun p-tran (p pint)  (mapcar #'+ p pint))(defun pn2pnc (pn-as-input)  (let* ((pn (string pn-as-input)))    (string-right-trim (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\-)                       pn)))(defun pnc-to-line-of-fifths-position (pitch-name-class)  (let* ((pnc (canonize-pitch-name pitch-name-class))	 (lofclass (position (char pnc 0)			     '(#\F #\C #\G #\D #\A #\E #\B)))	 (lofcycle (cond ((or (= 1 (length pnc))			      (eq #\N (char pnc 1)))			  0)			 ((member (char pnc 1)				  '(#\# #\S))			  (- (length pnc) 1))			 ((member (char pnc 1)				  '(#\B #\F))			  (- 1 (length pnc))))))    (if (and (numberp lofclass)	     (numberp lofcycle))	(+ lofclass 	   (* 7 lofcycle)))))(defun pnc2lofpos (pnc)  (pnc-to-line-of-fifths-position pnc))(defun canonize-pitch-name (pitch-name)  (substitute #\F #\B (substitute #\S #\# (string-upcase (string pitch-name)))              :start 1))(defun pin2pi (pin-as-input)  (let (i diat-size-str diat-size dir-str undisp-chroma-int        abs-morph-int undisp-qual quality int-disp undisp-cp-int        chromatic-pitch-int morphetic-pitch-int pin)    (setf pin (string-downcase pin-as-input))    (setf dir-str (subseq pin 0 1))    (if (not (member dir-str '("f" "r") :test #'string=))      (setf dir-str ""))    (setf quality "")    (if (string= dir-str "")      (setf i 0)      (setf i 1))    (do ()        ((not (and (< i (length pin))                   (not (member (elt pin i) '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))))))      (setf quality (concatenate 'string quality (subseq pin i (1+ i))))      (setf i (1+ i)))    (setf diat-size-str (subseq pin i (length pin)))    (setf diat-size (read-from-string diat-size-str))    (setf morphetic-pitch-int (1- diat-size))    (if (string= dir-str "f")      (setf morphetic-pitch-int (- morphetic-pitch-int)))    (setf abs-morph-int (mod (abs morphetic-pitch-int) 7))    (setf undisp-chroma-int (elt '(0 2 4 5 7 9 11) abs-morph-int))    (setf undisp-cp-int (+ undisp-chroma-int (* 12 (floor (abs morphetic-pitch-int) 7))))    (setf undisp-qual (elt '("p" "ma" "ma" "p" "p" "ma" "ma") abs-morph-int))    (if (string= undisp-qual "p")      (if (string= quality "p")        (setf int-disp 0)        (if (char= #\a (elt quality 0))          (setf int-disp (length quality))          (setf int-disp (- (length quality)))))      (if (string= quality "ma")        (setf int-disp 0)        (if (string= quality "mi")          (setf int-disp -1)          (if (char= (elt quality 0) #\a)            (setf int-disp (length quality))            (setf int-disp (- 0 (length quality) 1))))))    (if (< morphetic-pitch-int 0)      (setf chromatic-pitch-int (- (+ int-disp undisp-cp-int)))      (setf chromatic-pitch-int (+ int-disp undisp-cp-int)))    (list chromatic-pitch-int morphetic-pitch-int)))(defun m-from-c-on-line-of-fifths (c)  (mod (+ (* 4 (mod (+ (* 7 c)		       5)		    12))	  1)       7))(defun cpm2pn (chromatic-pitch morph)  (let* (morph-oct-1 morph-oct-2 morph-oct-3 mp1 mp2 mp3 chroma cp diff-list morph-oct-list best-morph-oct best-morphetic-pitch pn)    (setf morph-oct-1 (floor chromatic-pitch 12))    (setf morph-oct-2 (+ morph-oct-1 1))    (setf morph-oct-3 (- morph-oct-1 1))    (setf mp1 (+ morph-oct-1 (/ morph 7)))    (setf mp2 (+ morph-oct-2 (/ morph 7)))    (setf mp3 (+ morph-oct-3 (/ morph 7)))    (setf chroma (mod chromatic-pitch 12))    (setf cp (+ morph-oct-1 (/ chroma 12)))    (setf diff-list (list (abs (- cp mp1))			  (abs (- cp mp2))			  (abs (- cp mp3))))    (setf morph-oct-list (list morph-oct-1 morph-oct-2 morph-oct-3))    (setf best-morph-oct (elt morph-oct-list (position (apply #'min diff-list) diff-list)))    (setf best-morphetic-pitch (+ morph (* 7 best-morph-oct)))    (setf pn (p2pn (list chromatic-pitch best-morphetic-pitch)))    pn))(defun lofcp2pn (lof cp)  (let (accidental displacement letter-name octave-number undisplaced-chromatic-octave undisplaced-chromatic-pitch)    (setf displacement (floor lof 7))    (setf accidental (make-string (abs displacement) :initial-element (if (< displacement 0)									  #\f #\s)))    (if (string= accidental "") (setf accidental "n"))    (setf letter-name (elt '("F" "C" "G" "D" "A" "E" "B") (mod lof 7)))    (setf undisplaced-chromatic-pitch (- cp displacement))    (setf undisplaced-chromatic-octave (floor undisplaced-chromatic-pitch 12))    (if (member letter-name '("A" "B") :test #'string=)	(setf octave-number undisplaced-chromatic-octave)	(setf octave-number (1+ undisplaced-chromatic-octave)))    (concatenate 'string letter-name accidental (format nil "~d" octave-number))    ))(defun test-lofcp2pn (pn)  (pn= pn       (lofcp2pn (pnc2lofpos (pn2pnc pn))		 (first (pn2p pn)))))(defun batch-test-lof-cp-2pn (min-cp max-cp min-mp max-mp)  (do ((pn nil)       (cp min-cp (1+ cp)))      ((= cp (1+ max-cp)))    (do ((mp min-mp (1+ mp)))	((= mp (1+ max-mp)))      (setf pn (p2pn (list cp mp)))      (format t "~%~10a~10a~40a~10a"	      cp mp pn 	      (test-lofcp2pn pn))      (force-output))))(defun pn2c (pn)  (mod (first (pn2p pn)) 12))(defun pn2acc (pn)  (let* ((acc (substitute #\f #\b (substitute #\s #\# (string-downcase (subseq (pn2pnc pn) 1))))))    (if (string= acc "")	"n"	acc)))(defun p2pi (p1 p2)  (list (- (elt p2 0) (elt p1 0))        (- (elt p2 1) (elt p1 1))))(defun pn2pin (pn1 pn2)  (if (and pn1 pn2)    (pi2pin (p2pi (pn2p pn1) (pn2p pn2)))))(defun pin= (pin1 pin2)  (and pin1 pin2       (equalp (pin2pi pin1) (pin2pi pin2))))(defun pitch-name-equal-p (pn1 pn2)  (and pn1 pn2       (equalp (pn2p pn1) (pn2p pn2))))(defun pn= (pn1 pn2)  (pitch-name-equal-p pn1 pn2))(defun pi2pin (pitch-int)  (let* ((morphetic-pitch-int (elt pitch-int 1))         (dir-str (if (= 0 morphetic-pitch-int)                    ""                    (if (> morphetic-pitch-int 0)                      "r"                      "f")))         (diat-size (+ 1 (abs morphetic-pitch-int)))         (diat-size-str (format nil "~d" diat-size))         (abs-morph-int (mod (abs morphetic-pitch-int) 7))         (undisp-chroma-int (elt '(0 2 4 5 7 9 11) abs-morph-int))         (undisp-qual (elt '("p" "ma" "ma" "p" "p" "ma" "ma") abs-morph-int))         (int-disp (if (>= morphetic-pitch-int 0)                     (- (pi2cgi pitch-int) undisp-chroma-int)                     (- (pi2cgi (invert-pi pitch-int)) undisp-chroma-int)))         (quality "")         (qual-char ""))    (if (string= undisp-qual "p")      (if (= int-disp 0)        (setf quality "p")        (progn (if (> int-disp 0)                 (setf qual-char "a")                 (setf qual-char "d"))               (dotimes (i (abs int-disp))                 (setf quality (concatenate 'string quality qual-char)))))      (if (= int-disp 0)        (setf quality "ma")        (if (= -1 int-disp)          (setf quality "mi")          (if (< int-disp -1)            (dotimes (i (- (abs int-disp) 1))              (setf quality (concatenate 'string quality "d")))            (dotimes (i int-disp)              (setf quality (concatenate 'string quality "a")))))))    (concatenate 'string dir-str quality diat-size-str)))(defun invert-pi (pitch-int)  (list (- (elt pitch-int 0))        (- (elt pitch-int 1))))(defun pi2cgi (pitch-int)  (- (elt pitch-int 0) (* 12 (floor (elt pitch-int 1) 7))))(defun pn2midi (pn)  (if pn    (+ 21 (first (pn2p pn)))))(defun tpcmidi2pn (tpc midi-note)  (let* ((chromatic-pitch (compute-chromatic-pitch midi-note))         (morph (compute-morph tpc))         (inflection (compute-inflection tpc))         (morphetic-pitch (compute-morphetic-pitch chromatic-pitch morph inflection)))    (p2pn (list chromatic-pitch                morphetic-pitch))))(defun compute-chromatic-pitch (midi-note-number)  (- midi-note-number 21))(defun compute-morph (tpc)  (mod (+ 1 (* 4 tpc)) 7))(defun compute-inflection (tpc)  (floor (1- tpc) 7))(defun compute-morphetic-pitch (cp m i)  (let* ((undisplaced-chromatic-pitch (- cp i))         (morphetic-octave (floor undisplaced-chromatic-pitch 12))         ;(morphetic-octave (floor cp 12))         ;compare (compute-morphetic-pitch tolerance 0 -1) with morphetic-octave = (floor cp 12) and         ;morphetic-octave = (floor undisplaced-chromatic-pitch 12)         )    (+ (* 7 morphetic-octave)       m)))(defun pinc2ncipci (pinc)  (let (pitch-int)    (setf pitch-int (pin2pi pinc))    (list (mod (elt pitch-int 1) 7)          (mod (elt pitch-int 0) 12))))(defun pnc2pinc (pnc1-as-input pnc2-as-input)  (let* ((pnc1 (string pnc1-as-input))         (pnc2 (string pnc2-as-input))         (pn1 (concatenate 'string pnc1 "1"))         (pn2 (concatenate 'string pnc2 "1"))         (p1 (pn2p pn1))         (p2 (pn2p pn2))         (p2 (if (< (elt p2 1) (elt p1 1))               (list (+ 12 (elt p2 0))                     (+ 7 (elt p2 1)))               p2)))    (pi2pin (p2pi p1 p2))))(defun pncmidi2pn (pnc midi-note-number)  (let (inflection displacement undisplaced-midi-note-number asa-octave-number)    (setf inflection (subseq pnc 1))    (if (char= (char inflection 0) #\n)      (setf displacement 0)      (if (char= (char inflection 0) #\s)        (setf displacement (length inflection))        (setf displacement (- (length inflection)))))    (setf undisplaced-midi-note-number (- midi-note-number displacement))    (setf asa-octave-number (- (floor undisplaced-midi-note-number 12) 1))    (concatenate 'string pnc (format nil "~d" asa-octave-number))))(defun midiindex2pn (midi index)  (let (i p-dashed o c istr j ostr lstr)    (setf i (floor (1+ index) 7)          p-dashed (- midi i)          o (- (floor p-dashed 12) 1))    (if (> i 0)      (setf c "s")      (setf c "f"))    (setf istr ""          j 1)    (do ()        ((> j (abs i)))      (setf istr (concatenate 'string istr c)            j (1+ j)))    (if (zerop i)      (setf istr "n"))    (setf ostr (format nil "~d" o)          lstr (elt '("C" "D" "E" "F" "G" "A" "B") (mod (* 4 index) 7)))    (concatenate 'string lstr istr ostr)))(defun p-int-pm-int (pint)  (second pint))(defun p-int-pc-int (pint)  (first pint))(defun pi-inverse (pint)  (mapcar #'- pint))(defun invp (pint)  (list (- (p-int-pc-int pint))        (- (p-int-pm-int pint))))(defun p-int-gc-int (pint)  (- (p-int-pc-int pint)     (* 12         (floor (p-int-pm-int pint)             7))))(defun p-om (p)  (floor (p-pm p) 7))(defun p-gc (p)  (- (p-pc p)     (* 12 (p-om p))))(defun p-m (p)  (mod (p-pm p) 7))(defun transpose-pitch-name (pitch-name pitch-interval-name)  (pn-tran pitch-name pitch-interval-name))(defun pitch-name-chromatic-pitch (pitch-name)  (p-pc (pn2p pitch-name)))(defun pitch-name-morphetic-pitch (pitch-name)  (p-pm (pn2p pitch-name)))(defun chromatic-pitch-midi-note-number (chromatic-pitch)  (+ 21 chromatic-pitch))(defun p-pm (p)  (second p))(defun p-pc (p)  (first p))(defun pin-power (pin power)  (case (signum power)    (0 "p1")    (-1 (pin-power (pin-inverse pin) (- power)))    (1 (let ((result (pin-string pin)))         (dotimes (i (1- power) result)           (setf result                 (pin-sum result pin)))))))(defun pin-sum (&rest pin-list)  (pi2pin (apply #'pi-sum (mapcar #'pin2pi pin-list))))(defun pi-sum (&rest pi-list)  (list (apply #'+ (mapcar #'first pi-list))        (apply #'+ (mapcar #'second pi-list))))(defun pin-inverse (pin)  (pi2pin (pi-inverse (pin2pi pin))))(defun pin-string (pin)  (if (stringp pin)    (string-downcase pin)    (string-downcase (string pin))))(defun BASE-40-INTERVAL-PITCH-NAME-INTERVAL (base-40-interval)  (let* ((sign-of-interval (signum base-40-interval))         (absolute-interval (abs base-40-interval))         (absolute-interval-class (mod absolute-interval 40))         (absolute-interval-octave (floor absolute-interval 40))         (undirected-pitch-interval-name-class (case absolute-interval-class                                                 (0 "p1")                                                 (1 "a1")                                                 (4 "d2")                                                 (5 "mi2")                                                 (6 "ma2")                                                 (7 "a2")                                                 (10 "d3")                                                 (11 "mi3")                                                 (12 "ma3")                                                 (13 "a3")                                                 (16 "d4")                                                 (17 "p4")                                                 (18 "a4")                                                 (22 "d5")                                                 (23 "p5")                                                 (24 "a5")                                                 (27 "d6")                                                 (28 "mi6")                                                 (29 "ma6")                                                 (30 "a6")                                                 (33 "d7")                                                 (34 "mi7")                                                 (35 "ma7")                                                 (36 "a7")                                                 (39 "d8")                                                 ;(40 "p8") ;absolute-interval-class cannot be equal to 40                                                 (otherwise (format t "~%~%ERROR: Transposing interval (~d) not in range.~%~%"                                                                    base-40-interval)                                                            (throw 'throw-tag nil))))         (undirected-pitch-interval-name (string-left-trim '(#\r #\f)                                                            (pin-sum undirected-pitch-interval-name-class                                                                    (pin-power 'rp8 absolute-interval-octave)))))    (concatenate 'string                 (case sign-of-interval                   (0 "")                   (1 "r")                   (-1 "f"))                 undirected-pitch-interval-name)    ))