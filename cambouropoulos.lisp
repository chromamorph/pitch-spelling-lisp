#|
cambouropoulos.lisp
Copyright © 2002-2003 by David Meredith.

This file contains functions implementing 
Cambouropoulos's GPIR as described in
Cambouropoulos1996 and Cambouropoulos1998.

It also contains an implementation of his pitch-spelling algorithm.
|#

(setf *save-local-symbols* t)
(setf *verbose-eval-selection* t)
;(load #P"hd:Users:davemeredith:files:MIDI-to-notation:03 Pitch-spelling:2002-09-26-dphil:programs:lisp:pitch-spelling-common.lisp")

(defvar mum 7)
(setf mum 7)
(defvar muc 12)
(setf muc 12)

(defun well-formed-number-string-p (s)
  (let ((wf t))
    (dotimes (i (length s) wf)
      (if (not (or (<= (char-code #\0) (char-code (char s i)) (char-code #\9))
                   (and (= i 0) 
                        (equalp (char s i) #\-))))
        (setf wf nil)))))

(defun pathname-directory-to-string (pn)
  (let ((pns (concatenate 'string (second pn) ":")))
    (dolist (pnelt (cddr pn) pns)
      (setf pns (concatenate 'string
                             pns 
                             pnelt
                             ":")))))

(defun pn-p (pn-as-input)
  (let* ((n (if (stringp pn-as-input)
              (string-upcase pn-as-input)
              (string-upcase (string pn-as-input))))
         (n (if (and (>= (length n) 2)
                     (member (elt n 1) '(#\- #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))
              (concatenate 'string 
                           (string (elt n 0))
                           "N"
                           (subseq n 1))
              n))
         (n (if (and (>= (length n) 3)
                     (eq (elt n 1) #\#))
              (concatenate 'string 
                           (string (elt n 0))
                           "S"
                           (subseq n 2))
              n))
         (l (string (elt n 0)))
         (i (do* ((i "")
                  (x 2)
                  (j (string (elt n (- x 1))) (string (elt n (- x 1))))
                  (i (concatenate 'string i j) (concatenate 'string i j))
                  (x (+ 1 x) (+ 1 x)))
                 ((or (>= x (length n))
                      (member (elt n (- x 1)) '(#\- #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))
                  i)))
         (is-good-i (well-formed-inflection-p i))
         (o (if is-good-i
              (do* ((y (length i))
                    (x (+ y 2))
                    (o (string (elt n (- x 1))))
                    (x (+ 1 x) (+ 1 x))
                    (j (if (<= x (length n))
                         (string (elt n (- x 1)))
                         "")
                       (if (<= x (length n))
                         (string (elt n (- x 1)))
                         ""))
                    (o (if (equalp j "") o
                           (concatenate 'string o j))
                       (if (equalp j "") o
                           (concatenate 'string o j))))
                   ((equalp j "")
                    o))))
         (oasa (if is-good-i (read-from-string o)))
         (m (if is-good-i (position l
                                    '("A" "B" "C" "D" "E" "F" "G")
                                    :test #'equalp)))
         (cdash (if is-good-i (elt '(0 2 3 5 7 8 10) m)))
         (e (if is-good-i (cond ((equalp i "N") 0)
                                ((equalp (elt i 0) #\F) (* -1 (length i)))
                                ((member (elt i 0) '(#\S #\#)) (length i)))))
         (om (if is-good-i (if (or (= m 1) (= m 0))
                             oasa (- oasa 1))))
         (pc (if is-good-i (+ e cdash (* muc om))))
         (pm (if is-good-i (+ m (* om mum)))))
    (if is-good-i (list pc pm))))

(defun well-formed-inflection-p (i-as-input)
  (let ((i (string-upcase i-as-input)))
    (or (equalp i "N")
        (let ((wf t))
          (dotimes (j (length i) wf)
            (if (not (equalp (char i j) #\F))
              (setf wf nil))))
        (let ((wf t))
          (dotimes (j (length i) wf)
            (if (not (member (char i j) '(#\S #\#)))
              (setf wf nil)))))))
#|
Cambouropoulos1998, p.50

`In the GPIR every pitch is represented by an array of the sort
[nc, mdf, pc, oct] where nc (name-class) takes values from
{0, 1, 2, ..., M} for an M-tone scale, mdf (modifier) take
values from {-u, ..., -1, 0, 1, ..., u} where u is the number of
pitch interval units in the largest scale-step interval, pc (pitch-class)
takes values from {0, 1, 2, ..., N} for an N-tone discrete equal-tempered
pitch space and oct is octave range (middle C octave is 4).
For example, in the diatonic system D4 is [1, 0, 2, 4], D#4 is [1, 1, 3, 4],
Eb5 is [2, -1, 3, 5], Gb3 is [4, -1, 6, 3].'
|#

(defvar *m*)
(setf *m* 7)
(defvar *n*)
(setf *n* 12)

(defun set-n (n)
  (setf *n* n))

(defun set-m (m)
  (setf *m* m))

(defun gpir-pitch-nc (gpir-pitch)
  (first gpir-pitch))

(defun set-gpir-pitch-nc (gpir-pitch nc)
  (setf (first gpir-pitch) nc))

(defun gpir-pitch-mdf (gpir-pitch)
  (second gpir-pitch))

(defun set-gpir-pitch-mdf (gpir-pitch mdf)
  (setf (second gpir-pitch) mdf))

(defun gpir-pitch-pc (gpir-pitch)
  (third gpir-pitch))

(defun set-gpir-pitch-pc (gpir-pitch pc)
  (setf (third gpir-pitch) pc))

(defun gpir-pitch-oct (gpir-pitch)
  (fourth gpir-pitch))

(defun set-gpir-pitch-oct (gpir-pitch oct)
  (setf (fourth gpir-pitch) oct))

(defun pitch-name-gpir-pitch (pitch-name)
  (let* ((pitch-name-string (pitch-name-pitch-name-string pitch-name))
         (nc (pitch-name-string-gpir-pitch-nc pitch-name-string))
         (mdf (pitch-name-string-gpir-pitch-mdf pitch-name-string))
         (oct (pitch-name-string-gpir-pitch-oct pitch-name-string))
         (pc (nc-mdf-gpir-pitch-pc nc mdf)))
    (list nc mdf pc oct)))

(defun pitch-name-pitch-name-string (pitch-name)
  (if (stringp pitch-name)
    (string-upcase pitch-name)
    (string-upcase (string pitch-name))))

(defun pitch-name-string-gpir-pitch-nc (pitch-name-string)
  (mod (- (char-code (char pitch-name-string 0))
          (char-code #\A)
          2)
       *m*))

(defun pitch-name-string-gpir-pitch-mdf (pitch-name-string)
  (let ((abs-mdf (length (subseq pitch-name-string
                                 1
                                 (position-if #'(lambda (c)
                                                  (or (eq c #\-)
                                                      (digit-char-p c)))
                                              pitch-name-string)))))
    (case  (char pitch-name-string 1)
      (#\S abs-mdf)
      (#\F (- abs-mdf))
      (t 0))))

(defun pitch-name-string-gpir-pitch-oct (pitch-name-string)
  (parse-integer (subseq pitch-name-string
                         (position-if #'(lambda (c)
                                          (or (eq c #\-)
                                              (digit-char-p c)))
                                      pitch-name-string))))

(defun nc-mdf-gpir-pitch-pc (nc mdf)
  (if (and (= *n* 12) (= *m* 7))
    (mod (+ 5 (* 7 (+ (* mdf 7)
                      (mod (+ 1 (* 2 nc)) 7))))
         12)
    (progn (format t "pc can only be derived from pitch name when *m* = 7 and *n* = 12.~%")
           (abort))))
#|
Can this function be generalized for any kinds of pitch systems?
Presumably it will work for systems in which the generator is the same
size as *m*.

tpc	mdf	nc	genus-name
-7	-1	3	Ff
-6	-1	0	Cf
-5	-1	4	Gf
-4	-1	1	Df
-3	-1	5	Af
-2	-1	2	Ef
-1	-1	6	Bf
0	0	3	F
1	0	0	C
2	0	4	G
3	0	1	D
4	0	5	A
5	0	2	E
6	0	6	B
7	1	3	Fs
8	1	0	Cs
9	1	4	Gs
10	1	1	Ds
11	1	5	As
12	1	2	Es
13	1	6	Bs
|#


(defun gpir-pitch-pitch-name (gpir-pitch)
  (let* ((letter-name (gpir-pitch-pitch-name-letter-name gpir-pitch))
         (accidental (gpir-pitch-pitch-name-accidental gpir-pitch))
         (octave (gpir-pitch-pitch-name-octave gpir-pitch)))
    (string-downcase (concatenate 'string
                                  letter-name
                                  accidental
                                  octave))))

(defun gpir-pitch-pitch-name-letter-name (gpir-pitch)
  (string (code-char (+ (char-code #\A)
                        (mod (+ 2 (gpir-pitch-nc gpir-pitch)) *m*)))))

(defun gpir-pitch-pitch-name-accidental (gpir-pitch)
  (let ((mdf (gpir-pitch-mdf gpir-pitch)))
    (case (signum mdf)
    (0 "N")
    (1 (let ((ac ""))
         (dotimes (i mdf ac)
           (setf ac (concatenate 'string ac "S")))))
    (-1 (let ((ac ""))
         (dotimes (i (- mdf) ac)
           (setf ac (concatenate 'string ac "F"))))))))

(defun gpir-pitch-pitch-name-octave (gpir-pitch)
  (format nil "~d" (gpir-pitch-oct gpir-pitch)))

;;;;;;;;;;;;

#|
Pitch interval representation (Cambouropoulos1998, p.51):

`In the GPIR every pitch interval may be accurately represented
by an array of the sort [dir, nci, mdl, pci, oct] where dir (direction)
takes values from {-, =, +} depending on the direction of the interval,
nci (name class [interval]) takes values from {0, 1, 2, ... M} for an
M-tone scale, mdl (modality) takes values from class A, B, C or D,
pci (pitch-class interval) takes values from {0, 1, 2, ...N} for an N-tone
discrete equal-tempered pitch space and oct is the number of octaves
within compound intervals. For instance, in the traditional diatonic
system an ascending augmented 2nd is [+, 1, C1, 3, 0], a descending
minor 3rd is [-, 2, B1, 3, 0], an ascending minor ninth is [+, 1, B2, 2, 1]
whereas the same intervals in the 12-tone system are [+, 3, A, 3, 0],
[-, 3, A, 3, 0] and [+, 2, A, 2, 1]. In the latter case the nci and
mdl entries become redundant.'
|#

(defun gpir-int-dir (gpir-int)
  (first gpir-int))

(defun set-gpir-int-dir (gpir-int dir)
  (setf (first gpir-int) dir))

(defun gpir-int-nci (gpir-int)
  (second gpir-int))

(defun set-gpir-int-nci (gpir-int nci)
  (setf (second gpir-int) nci))

(defun gpir-int-mdl (gpir-int)
  (third gpir-int))

(defun set-gpir-int-mdl (gpir-int mdl)
  (setf (third gpir-int) mdl))

(defun gpir-int-pci (gpir-int)
  (fourth gpir-int))

(defun set-gpir-int-pci (gpir-int pci)
  (setf (fourth gpir-int) pci))

(defun gpir-int-oct (gpir-int)
  (fifth gpir-int))

(defun set-gpir-int-oct (gpir-int oct)
  (setf (fifth gpir-int) oct))

#|
Computing the modality table (see Cambouropoulos1998, pp.54--55)
for a single scale.
Two types of table can be generated: one showing frequency of occurrence
the other showing the modality category (A, B, C or D). The categorization
depends on the cut-off points between the modality boundaries.

The first step is to compute all the seconds, thirds, etc.:

For a diatonic set:
Scale-degree-size
second 	1 (2  2  1  2  2  2  1)
third 	2 (4  3  3  4  4  3  3)
fourth 	3 (5  5  5  6  5  5  5)
fifth	4 (7  7  7  7  7  7  6)
sixth	5 (9  9  8  9  9  8  8)
seventh	6 (11 10 10 11 10 10 10)

From this table we can compute a list of triples, (nci, f, pci)
where f is the number of times nci occurs within the scale with
size pci divided by the number of times nci occurs within the
scale in total (i.e., number of scale degrees). So for the diatonic scale,

(1 2/7 1)
(1 5/7 2)
(2 4/7 3)
(2 3/7 4)
(3 6/7 5)
(3 1/7 6)
(4 1/7 6)
(4 6/7 7)
(5 3/7 8)
(5 4/7 9)
(6 5/7 10)
(6 2/7 11)

If M = number of scale degrees and N is number of `semitones per octave' 
(i.e., M is morphetic modulus and N is chromatic modulus) then there 
are in total, NxM chromamorph intervals. Each of these intervals is assigned
a modality in Cambouropoulos's system. This means that, for example,
a sextuply-diminished prime has the same modality as a sextuply augmented one
- the usual ambiguity inherent in chromamorph type representations.

We can construct a list of all possible chromamorph intervals, each with its
frequency of occurrence (f). For all intervals that are not in the scale,
f = 0.

Anyway, in Emilios's system, each chromamorph interval is assigned a
modality which consists of a letter (A, B, C or D) (the
modality category) and a number (the modality index). The modality index
increases with the chromatic interval (although in his implementation
some modality indices are negative - I don't understand why).
The letter depends upon a modality category threshold (mct) which
he typically sets to equal .25.

Thus, 
if f >= 1-mct and f <= 1, mc = A;
if f > mct and f < 1-mct, mc = B;
if f > 0 and f <= mct, mc = C;
if f = 0, mc = D.

In his implementation, Emilios actually puts
	F>=0,	  F=<0.25, C=c),
which is wrong because if f = 0, the interval is not in the scale
and therefore it's modality category is D.

So we can now add modality categories to all possible intervals using:

(cond ((<= (- 1 mct) f 1) 'A)
      ((< mct f (- 1 mct)) 'B)
      ((and (< 0 f) (<= f mct)) 'C)
      ((zerop f) 'D))

This can be done by iterating through all possible chromamorph intervals
ending up with an n x m table in which each element is a list as follows
(nci f pci mc mi)
where mi is the modality index.



|#

(defun modality-table (interval-sequence 
                       modality-category-threshold
                       )
  (let* ((mct modality-category-threshold)
         (n (apply #'+ interval-sequence))
         (m (list-length interval-sequence))
         (double-int-seq (append interval-sequence interval-sequence))
         (within-scale-interval-table 
          (do* ((ints-for-this-nci nil)
                (wsi nil)
                (nci 0 (1+ nci)))
               ((= nci m)
                wsi)
            (setf ints-for-this-nci
                  (do* ((iftn nil)
                        (i 0 (1+ i)))
                       ((= i m)
                        iftn)
                    (setf iftn
                          (append iftn
                                  (list (list nci (apply #'+ (subseq double-int-seq
                                                                     i (+ i nci)))))))))
            (setf wsi (append wsi
                              ints-for-this-nci))))
         (modality-table
          (do* ((mt nil)
                (nci 0 (1+ nci)))
               ((= nci m)
                (reverse mt))
            (do* ((pci 0 (1+ pci)))
                 ((= pci n))
              (setf mt
                    (cons (list nci
                                (/ (count (list nci pci) 
                                          within-scale-interval-table
                                          :test #'equalp)
                                   m)
                                pci)
                          mt)))))
         (modality-table 
          (mapcar #'(lambda (triple)
                      (append triple
                              (let ((f (second triple)))
                                (list (* 1.0 f)
                                      (cond ((<= (- 1 mct) f 1) 'A)
                                            ((< mct f (- 1 mct)) 'B)
                                            ((and (< 0 f) (<= f mct)) 'C)
                                            ((zerop f) 'D))))))
                  modality-table)))
    modality-table))

(defun modality-table-within-scale (interval-sequence 
                                    modality-category-threshold)
  (remove-if #'zerop
             (modality-table interval-sequence
                             modality-category-threshold)
             :key #'second))

#|
For diatonic scale:
((0 1 0 1.0 A)
 (1 2/7 1 0.2857142857142857 B)
 (1 5/7 2 0.7142857142857143 B)
 (2 4/7 3 0.5714285714285714 B)
 (2 3/7 4 0.42857142857142855 B)
 (3 6/7 5 0.8571428571428571 A)
 (3 1/7 6 0.14285714285714285 C)
 (4 1/7 6 0.14285714285714285 C)
 (4 6/7 7 0.8571428571428571 A)
 (5 3/7 8 0.42857142857142855 B)
 (5 4/7 9 0.5714285714285714 B)
 (6 5/7 10 0.7142857142857143 B)
 (6 2/7 11 0.2857142857142857 B))

For ascending melodic minor scale
((0 1 0 1.0 A)
 (1 2/7 1 0.2857142857142857 B)
 (1 5/7 2 0.7142857142857143 B)
 (2 4/7 3 0.5714285714285714 B)
 (2 3/7 4 0.42857142857142855 B)
 (3 1/7 4 0.14285714285714285 C)
 (3 4/7 5 0.5714285714285714 B)
 (3 2/7 6 0.2857142857142857 B)
 (4 2/7 6 0.2857142857142857 B)
 (4 4/7 7 0.5714285714285714 B)
 (4 1/7 8 0.14285714285714285 C)
 (5 3/7 8 0.42857142857142855 B)
 (5 4/7 9 0.5714285714285714 B)
 (6 5/7 10 0.7142857142857143 B)
 (6 2/7 11 0.2857142857142857 B))

For harmonic minor scale 2 1 2 2 1 3 1:
((0 1 0 1.0 A)
 (1 3/7 1 0.42857142857142855 B)
 (1 3/7 2 0.42857142857142855 B)
 (1 1/7 3 0.14285714285714285 C)
 (2 4/7 3 0.5714285714285714 B)
 (2 3/7 4 0.42857142857142855 B)
 (3 1/7 4 0.14285714285714285 C)
 (3 4/7 5 0.5714285714285714 B)
 (3 2/7 6 0.2857142857142857 B)
 (4 2/7 6 0.2857142857142857 B)
 (4 4/7 7 0.5714285714285714 B)
 (4 1/7 8 0.14285714285714285 C)
 (5 3/7 8 0.42857142857142855 B)
 (5 4/7 9 0.5714285714285714 B)
 (6 1/7 9 0.14285714285714285 C)
 (6 3/7 10 0.42857142857142855 B)
 (6 3/7 11 0.42857142857142855 B))

For pentatonic scale: 2 2 3 2 3
((0 1 0 1.0 A)
 (1 3/5 2 0.6 B)
 (1 2/5 3 0.4 B)
 (2 1/5 4 0.2 C)
 (2 4/5 5 0.8 A)
 (3 4/5 7 0.8 A)
 (3 1/5 8 0.2 C)
 (4 2/5 9 0.4 B)
 (4 3/5 10 0.6 B))

For blues scale 3 2 1 1 3 2:
((0 1 0 1.0 A)
 (1 1/3 1 0.3333333333333333 B)
 (1 1/3 2 0.3333333333333333 B)
 (1 1/3 3 0.3333333333333333 B)
 (2 1/6 2 0.16666666666666666 C)
 (2 1/6 3 0.16666666666666666 C)
 (2 1/6 4 0.16666666666666666 C)
 (2 1/2 5 0.5 B)
 (3 1/6 4 0.16666666666666666 C)
 (3 1/6 5 0.16666666666666666 C)
 (3 1/3 6 0.3333333333333333 B)
 (3 1/6 7 0.16666666666666666 C)
 (3 1/6 8 0.16666666666666666 C)
 (4 1/2 7 0.5 B)
 (4 1/6 8 0.16666666666666666 C)
 (4 1/6 9 0.16666666666666666 C)
 (4 1/6 10 0.16666666666666666 C)
 (5 1/3 9 0.3333333333333333 B)
 (5 1/3 10 0.3333333333333333 B)
 (5 1/3 11 0.3333333333333333 B))

For octatonic scale 2 1 2 1 2 1 2 1
((0 1 0 1.0 A)
 (1 1/2 1 0.5 B)
 (1 1/2 2 0.5 B)
 (2 1 3 1.0 A)
 (3 1/2 4 0.5 B)
 (3 1/2 5 0.5 B)
 (4 1 6 1.0 A)
 (5 1/2 7 0.5 B)
 (5 1/2 8 0.5 B)
 (6 1 9 1.0 A)
 (7 1/2 10 0.5 B)
 (7 1/2 11 0.5 B))

For whole-tone scale 2 2 2 2 2 2
((0 1 0 1.0 A)
 (1 1 2 1.0 A)
 (2 1 4 1.0 A)
 (3 1 6 1.0 A)
 (4 1 8 1.0 A)
 (5 1 10 1.0 A))

For 12-tone scale 1 1 1 1 1 1 1 1 1 1 1 1
((0 1 0 1.0 A)
 (1 1 1 1.0 A)
 (2 1 2 1.0 A)
 (3 1 3 1.0 A)
 (4 1 4 1.0 A)
 (5 1 5 1.0 A)
 (6 1 6 1.0 A)
 (7 1 7 1.0 A)
 (8 1 8 1.0 A)
 (9 1 9 1.0 A)
 (10 1 10 1.0 A)
 (11 1 11 1.0 A))
|#

(defun blended-modality-table (within-scale
                               modality-category-threshold
                               sequence-weighting-pair
                               &rest sequence-weighting-pairs)
  (let* ((mct modality-category-threshold)
         (seq-wt-list (cons sequence-weighting-pair
                            sequence-weighting-pairs))
         (sum-of-weights (apply #'+ (mapcar #'second seq-wt-list)))
         (individual-modality-tables
          (let ((imts nil))
            (dolist (seq-wt-pair seq-wt-list imts)
              (setf imts 
                    (append imts
                            (list (modality-table (first seq-wt-pair)
                                                  modality-category-threshold)))))))
         (weighted-individual-modality-tables
          (mapcar #'(lambda (mod-table seq-wt-pair)
                      (let* ((wt (second seq-wt-pair)))
                        (mapcar #'(lambda (interval)
                                    (list (first interval)
                                          (* wt (second interval))
                                          (third interval)))
                                mod-table)))
                  individual-modality-tables
                  seq-wt-list))
         (blended-modality-table
          (let ((bmt (first weighted-individual-modality-tables)))
            (dolist (wtd-mod-tbl (cdr weighted-individual-modality-tables) bmt)
              (setf bmt (mapcar #'(lambda (interval1 interval2)
                                    (list (first interval1)
                                          (+ (second interval1) (second interval2))
                                          (third interval1)))
                                bmt
                                wtd-mod-tbl)))))
         (blended-modality-table
          (mapcar #'(lambda (interval)
                      (list (first interval)
                            (/ (second interval) sum-of-weights)
                            (third interval)))
                  blended-modality-table))
         (blended-modality-table
          (mapcar #'(lambda (interval)
                      (append interval
                              (let ((f (second interval)))
                                (list (* 1.0 f)
                                      (cond ((<= (- 1 mct) f 1) 'A)
                                            ((< mct f (- 1 mct)) 'B)
                                            ((and (< 0 f) (<= f mct)) 'C)
                                            ((zerop f) 'D))))))
                  blended-modality-table)))
    (if within-scale
      (remove-if #'zerop
                 blended-modality-table
                 :key #'second)
      blended-modality-table)))

(defvar *modality-table*)
(setf *modality-table*
      (blended-modality-table nil .25
                        (list (list 2 2 1 2 2 2 1) 4);major
                        (list (list 2 1 2 2 1 2 2) 1);natural minor
                        (list (list 2 1 2 2 1 2 2) 1);descending melodic minor
                        (list (list 2 1 2 2 2 2 1) 1);ascending melodic minor
                        (list (list 2 1 2 2 1 3 1) 2);harmonic minor
                        ))

#|

(pprint (blended-modality-table t .25 
                        (list (list 2 2 1 2 2 2 1) 12)
                            (list (list 2 1 2 2 1 3 1) 5)
                            (list (list 2 1 2 2 2 2 1) 3)))
[[2,2,1,2,2,2,1],12],[[2,1,2,2,1,3,1],5],[[2,1,2,2,2,2,1],3]

(pprint (blended-modality-table t .25
                        (list (list 2 2 1 2 2 2 1) 4);major
                        (list (list 2 1 2 2 1 2 2) 1);natural minor
                        (list (list 2 1 2 2 1 2 2) 1);descending melodic minor
                        (list (list 2 1 2 2 2 2 1) 1);ascending melodic minor
                        (list (list 2 1 2 2 1 3 1) 2);harmonic minor
                        ))

Note that Emilios includes natural minor and descending melodic minor which is the same!

(setf modality-category-threshold .25)
(setf sequence-weighting-pair (list (list 2 2 1 2 2 2 1) 12))
(setf sequence-weighting-pairs (list (list (list 2 1 2 2 1 3 1) 5)
                            (list (list 2 1 2 2 2 2 1) 3)))
|#

(defun pitch-interval-name-gpir-interval (pin &optional 
                                              (morphetic-octave t)
                                              (morphetic-direction t))
  (let* ((modality-table
          (blended-modality-table nil .25
                        (list (list 2 2 1 2 2 2 1) 4);major
                        (list (list 2 1 2 2 1 2 2) 1);natural minor
                        (list (list 2 1 2 2 1 2 2) 1);descending melodic minor
                        (list (list 2 1 2 2 2 2 1) 1);ascending melodic minor
                        (list (list 2 1 2 2 1 3 1) 2);harmonic minor
                        ))
         (i (pin-pi pin))
         (dir (if morphetic-direction
                (cond ((< 0 (second i)) '+)
                      ((> 0 (second i)) '-)
                      ((zerop (second i)) '=))
                (cond ((< 0 (first i)) '+)
                      ((> 0 (first i)) '-)
                      ((zerop (first i)) '=))))
         (morphetic-interval (if (eq dir '-)
                               (- (second i))
                               (second i)))
         (chromatic-interval (if (eq dir '-)
                               (- (first i))
                               (first i)))
         (pci (mod chromatic-interval 12))
         (nci (mod morphetic-interval 7))
         (oct (floor (if morphetic-octave 
                       morphetic-interval
                       chromatic-interval) 7))
         (mdl (fifth (find (list nci pci)
                           modality-table
                           :key #'(lambda (x)
                                    (list (first x)
                                          (third x)))
                           :test #'equalp))))
    (list dir nci mdl pci oct)))

(defun pin-gpir (pin &optional 
                     (morphetic-octave t)
                     (morphetic-direction t))
  (pitch-interval-name-gpir-interval pin
                                     morphetic-octave
                                     morphetic-direction))

#|
This demonstrates ambiguity in system:
? (pin-gpir 'aaaaaa1)
(= 0 D 6 0)
? (pin-gpir 'dddddd1)
(= 0 D 6 0)
? (pin-gpir 'ra4)
(+ 3 C 6 0)
? (pin-gpir 'rddddddddddd4)
(+ 3 C 6 0)
? 

With morphetic-octave/morphetic-direction
? (pin-gpir 'aaaaaa1 t nil)
(+ 0 D 6 0)
? (pin-gpir 'aaaaaa1 t t)
(= 0 D 6 0)
? (pin-gpir 'aaaaaa1 nil t)
(= 0 D 6 0)
? (pin-gpir 'aaaaaa1 nil nil)
(+ 0 D 6 0)
(pin-gpir 'rp4)
(+ 3 A 5 0)
? (pin-gpir 'rdddddddddddd4)
(+ 3 A 5 0)

|#



(defun pin-pi (pitch-interval-name)
  (let* ((pin (if (stringp pitch-interval-name)
                (string-upcase pitch-interval-name)
                (string-upcase (string pitch-interval-name))))
         (d (char pin 0))
         (d (if (member d '(#\F #\R) :test #'equalp) (string d) ""))
         (ty (do* ((ty "")
                   (x (if (equalp d "") 0 1))
                   (j (string (elt pin x)) (string (elt pin x)))
                   (ty (concatenate 'string ty j) (concatenate 'string ty j))
                   (x (+ 1 x) (+ 1 x)))
                  ((or (>= x (length pin))
                       (member (elt pin x) '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))
                   ty)))
         (ty-error (not (well-formed-interval-type-p ty)))
         (s (if (not ty-error)
              (do* ((y (length ty))
                    (x (if (equalp d "") y (+ y 1)))
                    (s (string (elt pin x)))
                    (x (+ 1 x) (+ 1 x))
                    (j (if (< x (length pin))
                         (string (elt pin x))
                         "")
                       (if (< x (length pin))
                         (string (elt pin x))
                         ""))
                    (s (if (equalp j "") s
                           (concatenate 'string s j))
                       (if (equalp j "") s
                           (concatenate 'string s j))))
                   ((equalp j "")
                    s))))
         (s-error (if (not ty-error) (not (well-formed-number-string-p s))))
         (s-dash (if (or s-error ty-error) nil (read-from-string s)))
         (pmintvar (if (or s-error ty-error) nil (if (equalp d "f") (- 1 s-dash) (- s-dash 1))))
         (mint-dash (if (or s-error ty-error) nil (mod (abs pmintvar) mum)))
         (cint-dash (if (or s-error ty-error) nil (elt '(0 2 4 5 7 9 11) mint-dash)))
         (pcintone (if (or s-error ty-error) nil (+ cint-dash
                                                    (* muc
                                                       (floor (abs pmintvar)
                                                            mum)))))
         (t-dash (if (or s-error ty-error) nil (elt '("p" "ma" "ma" "p" "p" "ma" "ma") mint-dash)))
         (e (if (or s-error ty-error) nil
                (cond ((and (equalp ty "p") (equalp t-dash "p")) 0)
                      ((and (equalp t-dash "p") (equalp (char ty 0) #\D)) (* (- 1) (length ty)))
                      ((and (equalp t-dash "p") (equalp (char ty 0) #\A)) (length ty))
                      ((and (equalp ty "ma") (equalp t-dash "ma")) 0)
                      ((and (equalp t-dash "ma") (equalp ty "mi")) (- 1))
                      ((and (equalp t-dash "ma") (equalp (char ty 0) #\D)) (* (- 1)
                                                                             (+ (length ty) 1)))
                      ((and (equalp t-dash "ma") (equalp (char ty 0) #\A)) (length ty)))))
         (pcintvar (if (or s-error ty-error) nil
                       (if (< pmintvar 0) (* (- 1) (+ e pcintone)) (+ e pcintone)))))
    (list pcintvar pmintvar)))



(defun well-formed-interval-type-p (ty)
  (or (member ty '("MA" "MI" "P") :test #'equalp)
      (let ((wf t))
        (dotimes (j (length ty) wf)
          (if (not (equalp (char ty j) #\D))
            (setf wf nil))))
      (let ((wf t))
        (dotimes (j (length ty) wf)
          (if (not (equalp (char ty j) #\A))
            (setf wf nil))))))

;*************************
; PITCH SPELLING ALGORITHM
;*************************

#|
Algorithm takes a sequence of MIDI note numbers as input
(\citealp[p.~58]{Cambouropoulos1998};\citealp{p.~2}{Cambouropoulos2002}).

In this implementation, the input will be an OPND file from which
a sequence of chromatic pitches will be extracted. This OPND file
is then used at the end of the algorithm to compare with the computed
pitch-spelling.

The algorithm actually only uses chroma information, not chromatic pitch
information. So the first step is to extract a sequence of chromas from
the input OPND file.

*******************

Then we make a table of possible chromamorphs, *q-table*.
The first step in constructing *q-table*

Then for each chroma c in this chroma sequence, c-seq,
we find all possible chromamorphs, q, and compute the inflection (my term) for
each q. This gives a list of seven ordered pairs
(qi-set c) = ((q1 i1) (q2 i2) ... (q7 i7))
where the morph of qn is n-1, the chroma of each qn is c and in is the inflection
of chromamorph qn. This is a generalization of Emilios's system because it allows
for inflections greater than 2 and less than -2 (i.e. triple sharps, triple flats etc).

Next we find the element of (qi-set c) for which the absolute inflection is least. The least
absolute inflection will be either 0 or 1. Let (m-min (qi-set c)) be the morph of the
 
Next we delete all elements of (qi-set c) in which the inflection is greater than 2. This
leaves at most 3

|#



(defun batch-camb-pitch-spell (&optional (print-pitch-names nil)
                                         (window-length 9)
                                         (enharmonic-penalty 2)
                                         (modality-C-penalty 1)
                                         (modality-D-penalty 2))
  (let* ((dir (directory (concatenate 'string
                                      (pathname-directory-to-string (pathname-directory (choose-directory-dialog)))
                                      "*.opnd-m")))
         (total-number-of-errors 0)
         (total-number-of-notes 0)
         (number-of-errors 0)
         (number-of-notes 0))
    (mapcar #'(lambda (filename)
                (multiple-value-setq (number-of-errors
                                      number-of-notes)
                  (camb-pitch-spell print-pitch-names
                                    filename
                                    window-length
                                    enharmonic-penalty
                                    modality-C-penalty
                                    modality-D-penalty))
                (setf total-number-of-errors (+ total-number-of-errors number-of-errors)
                      total-number-of-notes (+ total-number-of-notes number-of-notes))) 
            dir)
    (format t "~%Total number of notes = ~d~%" total-number-of-notes)
    (format t "Total number of errors = ~d~%" total-number-of-errors)
    (format t "Percentage correct = ~,2f%~%" (* 100 (- 1 (/ total-number-of-errors total-number-of-notes))))))

(defun camb-pitch-spell (&optional (print-pitch-names nil)
                                   (opnd-filename (choose-file-dialog))
                                   (window-length 9)
                                   (enharmonic-penalty 2)
                                   (modality-C-penalty 1)
                                   (modality-D-penalty 2)
                                   )
  (let* ((opnd (sort (with-open-file (opnd-file-stream
                                      opnd-filename)
                       (read opnd-file-stream))
                     #'<
                     :key #'first)
                     )
         (chromatic-pitch-sequence (mapcar #'(lambda (opnd-datapoint)
                                               (first (pn-p (second opnd-datapoint))))
                                           opnd))
         
         ;Now we have the input as a sequence of MIDI pitch numbers (- 21) in the order they appear
         ;in the midi file.

         (chroma-sequence (mapcar #'(lambda (chromatic-pitch)
                                      (mod chromatic-pitch 12))
                                  chromatic-pitch-sequence))
         (list-of-possible-chromamorphs (construct-list-of-possible-chromamorphs))
         (chromamorph-sequence (mapcar #'(lambda (chroma)
                                           (elt list-of-possible-chromamorphs
                                                chroma))
                                       chroma-sequence))
         (input-length (list-length chromamorph-sequence))
         (chromamorph-sequence (do* ((new-chromamorph-sequence nil)
                                     (i 0 (+ i (/ window-length 3))))
                                    ((>= (+ i (/ window-length 3)) input-length)
                                     (mapcar #'caar new-chromamorph-sequence))
                                 (let* ((window (append (if (null new-chromamorph-sequence)
                                                          nil
                                                          (subseq new-chromamorph-sequence
                                                                  i
                                                                  (+ i (/ window-length 3))))
                                                        (subseq chromamorph-sequence
                                                                (if (null new-chromamorph-sequence)
                                                                  i
                                                                  (+ i (/ window-length 3)))
                                                                (min input-length (+ i window-length)))))
                                        (all-window-spellings (compute-all-window-spellings window))
                                        (window-spelling-scores (mapcar #'(lambda (window-spelling)
                                                                            (compute-window-spelling-score window-spelling
                                                                                                           enharmonic-penalty
                                                                                                           modality-C-penalty
                                                                                                           modality-D-penalty))
                                                                        all-window-spellings))
                                        (min-score (apply #'min window-spelling-scores))
                                        (lowest-scoring-spellings (remove-if #'null
                                                                             (mapcar #'(lambda (spelling score)
                                                                                         (if (= score min-score)
                                                                                           spelling))
                                                                                     all-window-spellings
                                                                                     window-spelling-scores)))
                                        (best-spelling (compute-best-spelling lowest-scoring-spellings))
                                        (best-spelling (mapcar #'list best-spelling)))
                                   (setf new-chromamorph-sequence
                                         (append new-chromamorph-sequence
                                                 (if (zerop i)
                                                   (subseq best-spelling 0 (min (list-length best-spelling)
                                                                                (* 2 (/ window-length 3))))
                                                   (subseq best-spelling 
                                                           (/ window-length 3)
                                                           (min (list-length best-spelling)
                                                                (* 2 (/ window-length 3))))))))))
         (morph-sequence (mapcar #'second chromamorph-sequence))
         (morphetic-pitch-sequence (mapcar #'best-morphetic-pitch
                                           chromatic-pitch-sequence
                                           morph-sequence))
         (pitch-name-sequence (mapcar #'p-pn
                                      (mapcar #'list 
                                              chromatic-pitch-sequence
                                              morphetic-pitch-sequence)))
         ;``Output of the algorithm is a sequence of ÔcorrectlyÕ
         ;spelled pitches." 
         ; \citep[p.~2]{Cambouropoulos2002}

         ;But no indication of how the pitch-names are computed from the chromamorph representation.

         (error-list (remove-if #'null
                                (mapcar #'(lambda (pitch-name opnd-datapoint)
                                            (if (not (pitch-name-equal-p pitch-name (second opnd-datapoint)))
                                              (list opnd-datapoint pitch-name)))
                                        pitch-name-sequence
                                        opnd)))
         (number-of-errors (list-length error-list))
         ;;;;;;;;NOW FOR PITCH SEQUENCE TRANSPOSED BY RD2
         (pitch-name-sequence-transposed-rd2 (mapcar #'(lambda (pitch-name)
                                                         (pn-tran pitch-name 'rd2))
                                                     pitch-name-sequence))
         (rd2-error-list (remove-if #'null
                                (mapcar #'(lambda (pitch-name opnd-datapoint)
                                            (if (not (pitch-name-equal-p pitch-name (second opnd-datapoint)))
                                              (list opnd-datapoint pitch-name)))
                                        pitch-name-sequence-transposed-rd2
                                        opnd)))
         (rd2-number-of-errors (list-length rd2-error-list))

         ;;;;;;;;NOW FOR PITCH SEQUENCE TRANSPOSED BY FD2
         (pitch-name-sequence-transposed-fd2 (mapcar #'(lambda (pitch-name)
                                                         (pn-tran pitch-name 'fd2))
                                                     pitch-name-sequence))
         (fd2-error-list (remove-if #'null
                                (mapcar #'(lambda (pitch-name opnd-datapoint)
                                            (if (not (pitch-name-equal-p pitch-name (second opnd-datapoint)))
                                              (list opnd-datapoint pitch-name)))
                                        pitch-name-sequence-transposed-fd2
                                        opnd)))
         (fd2-number-of-errors (list-length fd2-error-list))

         ;;;NOW DETERMINE BEST SPELLING OF THE THREE POSSIBILITIES
         (best-spelling (position (min number-of-errors
                                       rd2-number-of-errors
                                       fd2-number-of-errors)
                                  (list number-of-errors
                                        rd2-number-of-errors
                                        fd2-number-of-errors)))
         (pitch-name-sequence (elt (list pitch-name-sequence
                                         pitch-name-sequence-transposed-rd2
                                         pitch-name-sequence-transposed-fd2)
                                   best-spelling))
         (computed-spelling (mapcar #'(lambda (pitch-name opnd-datapoint)
                                        (append (list (first opnd-datapoint))
                                                (list pitch-name)
                                                (cddr opnd-datapoint)))
                                    pitch-name-sequence opnd))
         (error-list (elt (list error-list
                                rd2-error-list
                                fd2-error-list)
                          best-spelling))
         (number-of-errors (elt (list number-of-errors
                                      rd2-number-of-errors
                                      fd2-number-of-errors)
                                best-spelling))
         (percentage-correct (* 100 (- 1 (/ number-of-errors (list-length opnd))))))
    (with-open-file (computed-spelling-file
                     (make-pathname :directory (pathname-directory opnd-filename)
                                    :name (pathname-name opnd-filename)
                                    :type "camb")
                     :direction :output
                     :if-exists :rename-and-delete)
      (pprint computed-spelling
              computed-spelling-file))
    (format t "~%~%PROGRAM: Cambouropoulos~%")
    (format t "FILE: ~s~%" (pathname-name opnd-filename))
    (format t "Number of errors = ~d~%" number-of-errors)
    (format t "Number of notes = ~d~%" (list-length opnd))
    (format t "Percentage correct = ~,2f%~%" percentage-correct)
    (format t "Best spelling obtained when computed spelling transposed by ~a.~%"
            (cond ((= best-spelling 0) 'p1)
                  ((= best-spelling 1) 'rd2)
                  ((= best-spelling 2) 'fd2)))
    (format t "ERROR LIST:")
    (pprint error-list)
    (if print-pitch-names 
      (progn (format t "~%COMPUTED PITCH NAME SEQUENCE:")
             (pprint (mapcar #'string-downcase pitch-name-sequence))
             (format t "~%ORIGINAL PITCH NAME SEQUENCE:")
             (pprint (mapcar #'string-downcase (mapcar #'second opnd)))))
    (values number-of-errors
            (list-length opnd))))

#|
Cambouropoulos's papers on pitch-spelling:

\citep[pp.~242--246]{Cambouropoulos1996}
\citep[pp.~58--62]{Cambouropoulos1998}
\citep[p.~5]{Cambouropoulos2000}
\citep{Cambouropoulos2001}
\citep{Cambouropoulos2002}

\citet{Cambouropoulos2002}
==========================

``Input to the proposed algorithm is merely a sequence of MIDI pitch numbers in the order they appear in a MIDI file."
\citep[p.~2]{Cambouropoulos2002}


``Output of the algorithm is a sequence of ÔcorrectlyÕ
spelled pitches."
\citep[p.~2]{Cambouropoulos2002}

In fact, isn't the output just a sequence of chromamorphs?

REFS:

@article{Cambouropoulos1996,
	author="Emilios Cambouropoulos",
	title="A general pitch interval representation: {T}heory
			and Applications",
	journal="Journal of New Music Research",
	year=1996,
	volume=25,
	pages={231--251}}

@phdthesis{Cambouropoulos1998,
	author="Emilios Cambouropoulos",
	title="Towards a General Computational Theory of
			Musical Structure",
	school="University of Edinburgh",
	year=1998,
	month=feb}

@inproceedings{Cambouropoulos2000,
	author = "Emilios Cambouropoulos",
	title = "From {MIDI} to Traditional Musical Notation",
	booktitle = "Proceedings of the AAAI 2000 Workshop on Artificial Intelligence and Music, 
	17th National Conference on Artificial Intelligence (AAAI'2000), 30 July--3 August",
	address = "Austin, TX.",
	year = 2000,
	note = "Available online at \hbox{\ttfamily ftp://ftp.ai.univie.ac.at/papers/oefai-tr-2000-15.pdf}"}

@inproceedings{Cambouropoulos2001,
	author="Emilios Cambouropoulos",
	title = "Automatic Pitch Spelling: From Numbers to Sharps and Flats",
	booktitle="VIII Brazilian Symposium on Computer Music (SBC{\&}M 2001)",
	year = 2001,
	address = "Fortaleza, Brazil",
	note = "Available online at \hbox{\ttfamily ftp://ftp.ai.univie.ac.at/papers/oefai-tr-2001-12.pdf}"
	}

@article{Cambouropoulos2002,
	author = "Emilios Cambouropoulos",
	year = 2002,
	title = "Pitch Spelling: A Computational Model",
	journal = "Music Perception",
	note = "To appear"
}

|#

(defun pn-tran (pitch-name pitch-interval-name)
  (p-pn (p-tran (pn-p pitch-name) (pin-pi pitch-interval-name))))

(defun p-tran (p i)
  (mapcar #'+ p i))

(defun compute-best-spelling (lowest-scoring-spellings)
  (let* ((best-spelling (first lowest-scoring-spellings)))
    (dolist (spelling (cdr lowest-scoring-spellings) best-spelling)
      (if (is-better-spelling-p spelling best-spelling)
        (setf best-spelling spelling)))))

(defun is-better-spelling-p (spelling1 spelling2)
  (do* ((score1 0)
        (score2 0)
        (spelling-length (list-length spelling1))
        (last-i (* 2 (/ spelling-length 3)))
        (i (/ spelling-length 3)
           (1+ i)))
       ((= i last-i)
        (> score1 score2))
    (if (not (equalp (elt spelling1 i) (elt spelling2 i)))
      (let* (;(q11 (first (elt spelling1 (1- i))))
             (q12 (first (elt spelling1 i)))
             (q13 (first (elt spelling1 (1+ i))))
             ;(q21 (first (elt spelling2 (1- i))))
             (q22 (first (elt spelling2 i)))
             (q23 (first (elt spelling2 (1+ i))))
             ;(modality11 (position (compute-modality-category (chromamorph-interval q11 q12)) '(a b c d)))
             (modality12 (position (compute-modality-category (chromamorph-interval q12 q13)) '(a b c d)))
             ;(modality21 (position (compute-modality-category (chromamorph-interval q21 q22)) '(a b c d)))
             (modality22 (position (compute-modality-category (chromamorph-interval q22 q23)) '(a b c d))))
        (cond ((< modality12 modality22) (setf score1 (1+ score1)))
              ((< modality22 modality12) (setf score2 (1+ score2))))))))

(defun chromamorph-interval (q1 q2)
  (let* ((c1 (first q1))
         (c2 (first q2))
         (m1 (second q1))
         (m2 (second q2)))
    (list (mod (- c2 c1) 12)
          (mod (- m2 m1) 7))))

(defun pitch-name-equal-p (pn1 pn2)
  (equalp (pn-p pn1) (pn-p pn2)))

(defun best-morphetic-pitch (chromatic-pitch morph)
  (let* ((morphetic-octave1 (floor chromatic-pitch 12))
         (morphetic-octave2 (+ 1 morphetic-octave1))
         (morphetic-octave3 (- morphetic-octave1 1))
         (mp1 (+ morphetic-octave1 (/ morph 7)))
         (mp2 (+ morphetic-octave2 (/ morph 7)))
         (mp3 (+ morphetic-octave3 (/ morph 7)))
         (chroma (mod chromatic-pitch 12))
         (cp (+ morphetic-octave1 (/ chroma 12)))
         (difference-list (list (abs (- cp mp1))
                                (abs (- cp mp2))
                                (abs (- cp mp3))))
         (morphetic-octave-list (list morphetic-octave1
                                      morphetic-octave2
                                      morphetic-octave3))
         (best-morphetic-octave (elt morphetic-octave-list
                                     (position (apply #'min difference-list)
                                               difference-list))))
    (+ (* 7 best-morphetic-octave) morph)))

(defun compute-window-spelling-score (window-spelling
                                      &optional
                                      (enharmonic-penalty 2)
                                      (modality-C-penalty 1)
                                      (modality-D-penalty 2))
  (let* ((interval-list (compute-interval-list window-spelling))
         (score-list (mapcar #'(lambda (interval)
                                 (score-interval interval
                                                 enharmonic-penalty
                                                 modality-C-penalty
                                                 modality-D-penalty)) 
                             interval-list)))
    (apply #'+ score-list)))

(defun compute-interval-list (window-spelling)
  (do* ((window-length (list-length window-spelling))
        (interval-list nil)
        (i 0 (1+ i)))
       ((= i (1- window-length))
        interval-list)
    (do* ((j (1+ i) (1+ j)))
         ((= j window-length))
      (setf interval-list
            (cons (list (elt window-spelling i)
                        (elt window-spelling j))
                  interval-list)))))

(defvar *blended-modality-table*)
(setf *blended-modality-table*
      (blended-modality-table nil .25
                              (list (list 2 2 1 2 2 2 1) 4);major
                              (list (list 2 1 2 2 1 2 2) 1);natural minor
                              (list (list 2 1 2 2 1 2 2) 1);descending melodic minor
                              (list (list 2 1 2 2 2 2 1) 1);ascending melodic minor
                              (list (list 2 1 2 2 1 3 1) 2);harmonic minor
                              ))



(defun score-interval (interval
                       enharmonic-penalty
                       modality-C-penalty
                       modality-D-penalty)
  (let* ((c1 (first (first (first interval))))
         (c2 (first (first (second interval))))
         (m1 (second (first (first interval))))
         (m2 (second (first (second interval))))
         (i (list (mod (- c2 c1) 12)
                  (mod (- m2 m1) 7)))
         (modality-category (compute-modality-category i))
         (modality-penalty (case modality-category
                             (A 0)
                             (B 0)
                             (C modality-C-penalty)
                             (D modality-D-penalty)))
         (enh-penalty (if (is-enharmonic-p (first interval))
                        enharmonic-penalty
                        0))
         (enh-penalty (if (is-enharmonic-p (second interval))
                        (+ enh-penalty enharmonic-penalty)
                        enh-penalty)))
    (+ modality-penalty enh-penalty)))

(defun is-enharmonic-p (qi)
  (eq (third qi) 'enh))

(defun compute-modality-category (i)
  (fifth (find i *blended-modality-table*
               :key #'(lambda (x)
                        (list (third x)
                              (first x)))
               :test #'equalp)))

(defun compute-all-window-spellings (window)
  (let* ((flat-window (mapcar #'(lambda (qs-for-this-note)
                                  (if (= 3 (list-length qs-for-this-note))
                                    (butlast qs-for-this-note)
                                    qs-for-this-note)
                                  )
                              window))
         (sharp-window (mapcar #'(lambda (qs-for-this-note)
                                   (if (= 3 (list-length qs-for-this-note))
                                    (rest qs-for-this-note)
                                    qs-for-this-note)
                                  )
                               window))
         (sharp-window-spellings (compute-all-window-spellings-2 sharp-window))
         (flat-window-spellings (compute-all-window-spellings-2 flat-window)))
    (remove-duplicates (append sharp-window-spellings
                               flat-window-spellings)
                       :test #'equalp)))

(defun compute-all-window-spellings-2 (window)
  (let* ((spellings nil)
         (window-length (list-length window))
         (window-length-string (format nil "~d" window-length)))
    (dotimes (x (expt 2 window-length) (remove-duplicates spellings :test #'equalp))
      (setf spellings
            (append spellings
                    (list (map 'list #'(lambda (qs-for-this-note c)
                                         (if (= 1 (list-length qs-for-this-note))
                                           (first qs-for-this-note)
                                           (elt qs-for-this-note
                                              (read-from-string (string c)))))
                               window
                               (format nil 
                                       (concatenate 'string "~"
                                                    window-length-string
                                                    ",'0,b")
                                       x))))))))

#|
(compute-all-window-spellings-2 '((0 1) (0 1) (0 1) (0 1)))
(map 'list #'(lambda (x c)
               (elt x (read-from-string (string c))))
     '((a b) (c d) (e f))
     "010")

(compute-all-window-spellings (setf window (construct-list-of-possible-chromamorphs)))
(setf qs-for-this-note (first window))

(format nil "~10,'0,b" 4)

(dotimes (x 4)
  (format t 
          (concatenate 'string "~"
                       "4"
                       ",'0,b~%")
          x))
|#

#|
Assume chromamorph of A natural is (0 0) so that we can mips2asa procedures.
|#

(defun construct-all-q-table ()
  (do* ((q-table nil)
        (c 0 (1+ c)))
       ((= c 12)
        q-table)
    (do* ((q-set-for-this-chroma nil)
          (m 0 (1+ m)))
         ((= m 7)
          (setf q-table
                (append q-table
                        (list q-set-for-this-chroma))))
      (setf q-set-for-this-chroma
            (cons (list (list c m)
                        (q-inf (list c m)))
                  q-set-for-this-chroma)))))

#|
c	m	c'	c-c'
0	0	0	0
0	2	3	-3	+9
11	2	3	8	-4
10	2	3	7	-5
2	1	2	0	0
3	1	2	1	-11
1	1	2	-1	11
7	4	7	0	0
3	4	7	-4	8
|#


(defun q-inf (q)
  ;See table above. For each chromamorph there are two possible
  ;inflections. This function returns the one whose absolute value
  ;is least.
  (let* ((c (first q))
         (m (second q))
         (c-dash (elt (list 0 2 3 5 7 8 10) m))
         (i1 (- c c-dash))
         (i2 (if (zerop i1)
               0
               (* -1 (signum i1) (- 12 (abs i1)))))
         (i (if (< (abs i1) (abs i2))
              i1
              i2)))
    i))

(defun construct-list-of-possible-chromamorphs ()
  (let* ((all-q-table (construct-all-q-table))
         )
    (mapcar #'(lambda (q-set-for-this-chroma)
                (let* ((white-note (zerop (apply #'min (mapcar #'(lambda (qi)
                                                                   (abs (second qi))) 
                                                               q-set-for-this-chroma))))
                       (pos-of-0-or-1-inf (position (if white-note 0 1)
                                                    q-set-for-this-chroma
                                                    :key #'second
                                                    :test #'=)))
                  (if white-note
                    (list (append (elt q-set-for-this-chroma (mod (1- pos-of-0-or-1-inf) 7))
                                  (list 'enh))
                          (elt q-set-for-this-chroma pos-of-0-or-1-inf)
                          (append (elt q-set-for-this-chroma (mod (1+ pos-of-0-or-1-inf) 7))
                                  (list 'enh)))
                    (list (elt q-set-for-this-chroma (mod (1- pos-of-0-or-1-inf) 7))
                          (elt q-set-for-this-chroma pos-of-0-or-1-inf)))))
            all-q-table)))

(defvar *list-of-possible-chromamorphs*)
(setf *list-of-possible-chromamorphs*
      (construct-list-of-possible-chromamorphs))





(defun p-pn (p)
  (let* ((m (p-m p))
         (l (elt '("A" "B" "C" "D" "E" "F" "G") m))
         (gc (p-gc p))
         (cdash (elt '(0 2 3 5 7 8 10) m))
         (e (- gc cdash))
         (i "")
         (i (cond ((< e 0) (dotimes (j (- e) i) (setf i (concatenate 'string i "f"))))
                  ((> e 0) (dotimes (j e i) (setf i (concatenate 'string i "s"))))
                  ((= e 0) "n")))
         (om (p-om p))
         (oasa (if (or (= m 0) (= m 1))
                 om
                 (+ 1 om)))
         (o (format nil "~D" oasa)))
    (concatenate 'string l i o)))

(defun p-m (p)
  (mod (p-pm p) mum))

(defun p-pm (p)
  (second p))



(defun p-gc (p)
  (- (p-pc p)
     (* muc (p-om p))))

(defun p-pc (p)
  (first p))

(defun p-om (p)
  (div (p-pm p) mum))

(defun div (x y)
  (floor x y))

#|
event(0,[f,#,4],0,2,[]).	% sharps #, flats b, natural = or n
event(1,[g,=,4],2,1,[]).
event(2,[a,=,4],3,1,[]).
event(3,[a,=,4],4,1,[]).
event(4,[g,=,4],5,1,[]).
event(5,[f,#,4],6,1,[]).
event(6,[e,=,4],7,1,[]).
event(7,[d,=,4],8,2,[]).

|#

(defun batch-opnd2prolog (&optional (opnd-directory (choose-directory-dialog)))
  (let* ((opnd-dir (directory (concatenate 'string
                                           (pathname-directory-to-string (pathname-directory opnd-directory))
                                           "*.opnd*"))))
    (dolist (opnd-filename opnd-dir)
      (opnd2prolog opnd-filename))))

(defun opnd2prolog (&optional (opnd-filename (choose-file-dialog)))
  (let* ((id -1)
         (triple nil)
         (opnd (sort (with-open-file (opnd-filestream
                                      opnd-filename)
                       (read opnd-filestream))
                     #'(lambda (dp1 dp2)
                         (or (< (first dp1) (first dp2))
                             (and (= (first dp1) (first dp2))
                                  (pitch-name-chromatic-pitch-< (second dp1)
                                                                (second dp2)))))))
         (prolog-filename (pathname (concatenate 'string
                                                 (pathname-directory-to-string (pathname-directory opnd-filename))
                                                 (pathname-name opnd-filename) 
                                                 ".camb"))))
    (with-open-file (prolog-filestream
                     prolog-filename
                     :direction :output
                     :if-exists :rename-and-delete)
      (dolist (datapoint opnd)
        (setf id (1+ id))
        (setf triple (pn-triple (second datapoint)))
        (format prolog-filestream "~%event(~d,[~a,~a,~a],~d,~d,[])."
                id
                (first triple)
                (second triple)
                (third triple)
                (first datapoint)
                (third datapoint))))
    ))

(defun pitch-name-chromatic-pitch-< (pn1 pn2)
  (< (first (pn-p pn1))
     (first (pn-p pn2))))

(defun pn-triple (pn-as-input)
  (let* ((n (if (stringp pn-as-input)
              (string-upcase pn-as-input)
              (string-upcase (string pn-as-input))))
         (n (if (and (>= (length n) 2)
                     (member (elt n 1) '(#\- #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))
              (concatenate 'string 
                           (string (elt n 0))
                           "N"
                           (subseq n 1))
              n))
         (n (if (and (>= (length n) 3)
                     (eq (elt n 1) #\#))
              (concatenate 'string 
                           (string (elt n 0))
                           "S"
                           (subseq n 2))
              n))
         (l (string (elt n 0)))
         (i (do* ((i "")
                  (x 2)
                  (j (string (elt n (- x 1))) (string (elt n (- x 1))))
                  (i (concatenate 'string i j) (concatenate 'string i j))
                  (x (+ 1 x) (+ 1 x)))
                 ((or (>= x (length n))
                      (member (elt n (- x 1)) '(#\- #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))
                  i)))
         (o (do* ((y (length i))
                  (x (+ y 2))
                  (o (string (elt n (- x 1))))
                  (x (+ 1 x) (+ 1 x))
                  (j (if (<= x (length n))
                       (string (elt n (- x 1)))
                       "")
                     (if (<= x (length n))
                       (string (elt n (- x 1)))
                       ""))
                  (o (if (equalp j "") o
                         (concatenate 'string o j))
                     (if (equalp j "") o
                         (concatenate 'string o j))))
                 ((equalp j "")
                  o)))
         (oasa (read-from-string o))
         )
    (list (string-downcase l) 
          (substitute #\b #\f 
                      (substitute #\= #\n 
                                  (substitute  #\# #\s 
                                               (string-downcase i))))
          oasa)))

(defun test-compute-pitch-name-table ()
  (mapcar #'(lambda (pn-list)
              (mapcar #'butlast
                      (mapcar #'pitch-name-gpir-pitch
                              pn-list)))
          (list (list 'bs3 'cn4 'dff4)
                (list 'cs4 'df4)
                (list 'css4 'dn4 'eff4)
                (list 'ds4 'ef4)
                (list 'dss4 'en4 'ff4)
                (list 'es4 'fn4 'gff4)
                (list 'fs4 'gf4)
                (list 'fss4 'gn4 'aff4)
                (list 'gs4 'af4)
                (list 'gss4 'an4 'bff4)
                (list 'as4 'bf4)
                (list 'ass4 'bn4 'cf5))))
