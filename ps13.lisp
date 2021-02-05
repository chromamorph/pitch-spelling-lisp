 #|
ps13 Pitch spelling algorithm as described in

@inproceedings{MeredithPS13ESCOM5,
	author = "David Meredith",
	year = 2003,
	title = "Pitch Spelling Algorithms",
	booktitle = "Proceedings of the Fifth Triennial ESCOM Conference (ESCOM5) (8-13 September 2003)",
	address = "Hanover University of Music and Drama, Hanover, Germany.",
	pages = "pp.~204--207",
	editor = "R. Kopiez and A. C. Lehmann and I. Wolther and C. Wolf",
	isbn = "3-931852-67-9",
	issn = "1617-6847"
}

Copyright © 2003-2003 by David Meredith. All rights reserved.
|#

#||
;;; I don't think these are needed, and they only make sense in CCL
(setf *save-local-symbols* t)
(setf *verbose-eval-selection* t)
||#

(defvar mum 7)
(setf mum 7)
(defvar muc 12)
(setf muc 12)

(defun ps13-test-from-file-list (file-list &optional (pre-context 33) (post-context 23) (supertest nil))
  (let* ((total-number-of-errors 0)
         (total-number-of-notes 0))
    (dolist (file-name file-list)
      (multiple-value-bind (number-of-errors number-of-notes)
                           (ps13  file-name nil pre-context post-context supertest)
        (setf total-number-of-errors (+ total-number-of-errors number-of-errors)
              total-number-of-notes (+ total-number-of-notes number-of-notes))))
    (format t 
                "
Total number of errors = ~d.
Total number of notes = ~d.
Percentage correct = ~,2f%

"
                total-number-of-errors
                total-number-of-notes
                (* 100.0 (/ (- total-number-of-notes total-number-of-errors) total-number-of-notes))
                )))

(defun ps13-test (&optional (directory (choose-directory-dialog)) (pre-context 33) (post-context 23) (supertest nil))
  (let* ((file-list (directory (pathname (concatenate 'string
                                                      (pathname-directory-to-string (pathname-directory directory))
                                                      "*.opnd*"))))
         
         (total-number-of-errors 0)
         (total-number-of-notes 0))
    (dolist (file-name file-list)
      (multiple-value-bind (number-of-errors number-of-notes)
                           (ps13  file-name nil pre-context post-context supertest)
        (setf total-number-of-errors (+ total-number-of-errors number-of-errors)
              total-number-of-notes (+ total-number-of-notes number-of-notes))))
    (format t 
                "
Total number of errors = ~d.
Total number of notes = ~d.
Percentage correct = ~,2f%

"
                total-number-of-errors
                total-number-of-notes
                (* 100.0 (/ (- total-number-of-notes total-number-of-errors) total-number-of-notes))
                )))

(defun ps13-supertest (&optional (start-pre-context 1) (start-post-context 1) (end-pre-context 25) (end-post-context 25))
  (do* ((file-list (directory (pathname (concatenate 'string
                                                      (pathname-directory-to-string (pathname-directory (choose-directory-dialog)))
                                                      "*.opnd*"))))
        (best-pre-context start-pre-context)
        (best-post-context start-post-context)
        (best-percentage 0.0)
        (pre-context start-pre-context (1+ pre-context)))
       ((= pre-context end-pre-context)
        (format t 
                "
Best pre-context = ~d
Best post-context = ~d
Best score = ~,2f%
"
                best-pre-context
                best-post-context
                best-percentage))
    (do* ((this-percentage 0)
          (post-context start-post-context (1+ post-context)))
         ((= post-context end-post-context))
      (setf this-percentage (ps13-test-for-supertest file-list pre-context post-context))
      (pprint (list pre-context post-context this-percentage))
      (if (> this-percentage best-percentage)
        (setf best-percentage this-percentage
              best-pre-context pre-context
              best-post-context post-context)))))

(defun ps13-test-for-supertest (file-list pre-context post-context)
  (let* ((total-number-of-errors 0)
         (total-number-of-notes 0))
    (dolist (file-name file-list)
      (multiple-value-bind (number-of-errors number-of-notes)
                           (ps13 file-name nil pre-context post-context t)
        (setf total-number-of-errors (+ total-number-of-errors number-of-errors)
              total-number-of-notes (+ total-number-of-notes number-of-notes))))
    (* 100.0 (/ (- total-number-of-notes total-number-of-errors) total-number-of-notes))))



(defun ps13 (&optional (opnd-file-name (choose-file-dialog))
                       (init-morph nil)
                       (pre-context 33)
                       (post-context 23)
                       (supertest nil))
  (let* ((chromamorph-table (list 0 1 1 2 2 3 3 4 5 5 6 6))
         (opnd-file-contents (with-open-file (opnd-file
                                              opnd-file-name)
                               (read opnd-file)))
         (opcpm-test (sort (mapcar #'(lambda (opnd-datapoint)
                                       (let* ((p (pn-p (second opnd-datapoint))))
                                         (list (first opnd-datapoint)
                                               (first p)
                                               (second p))))
                                   opnd-file-contents) #'vector-less-than))
         (onset-list (mapcar #'first opcpm-test))
         (chromatic-pitch-list (mapcar #'second opcpm-test))
         (chroma-list (mapcar #'chromatic-pitch-chroma chromatic-pitch-list))
         (n (list-length chroma-list))
         (chroma-vector-list (do* ((cvl nil)
                                   (i 0 (1+ i)))
                                  ((= i n)
                                   cvl)
                               (setf cvl
                                     (append cvl
                                             (list (do* ((context (subseq chroma-list
                                                                          (max 0 (- i pre-context))
                                                                          (min n (+ i post-context))))
                                                         (cv (list 0 0 0 0 0 0 0 0 0 0 0 0))
                                                         (c 0 (+ 1 c)))
                                                        ((= c 12)
                                                         cv)
                                                     (setf (elt cv c)
                                                           (count c context))))))))
         (spelling-table (do* ((first-morph nil nil)
                               (spelling nil nil)
                               (spelling2 nil nil)
                               (st nil)
                               (c 0 (1+ c)))
                              ((= c 12)
                               st)
                           (setf spelling (mapcar #'(lambda (chroma-in-chroma-list)
                                                      (elt chromamorph-table
                                                           (mod (- chroma-in-chroma-list c) 12)))
                                                  chroma-list))
                           (setf first-morph (first spelling))
                           (setf spelling2 (mapcar #'(lambda (morph-in-spelling)
                                                       (mod (- morph-in-spelling first-morph) 7))
                                                   spelling))
                           (setf st (append st (list spelling2)))))
         
         (relative-morph-list (do ((morph-vector (list 0 0 0 0 0 0 0) 
                                                 (list 0 0 0 0 0 0 0))
                                   (rml nil)
                                   (i 0 (1+ i))
                                   (morphs-for-this-chroma nil 
                                                           nil)
                                   )
                                  ((= i n)
                                   rml)
                                (setf morphs-for-this-chroma
                                      (mapcar #'(lambda (spelling)
                                                  (elt spelling i))
                                              spelling-table))
                                (setf rml (do ((prev-score nil nil)
                                               (j 0 (1+ j)))
                                              ((= j 12)
                                               ;(pprint morph-vector)
                                               (append rml (list (position (apply #'max morph-vector)
                                                                           morph-vector))))
                                            (setf prev-score (elt morph-vector (elt morphs-for-this-chroma j)))
                                            (setf (elt morph-vector (elt morphs-for-this-chroma j))
                                                  (+ prev-score (elt (elt chroma-vector-list i) j)))))))
         (initial-morph (if init-morph init-morph (elt '(0 1 1 2 2 3 4 4 5 5 6 6) (mod (first chromatic-pitch-list) 12))))
         (morph-list (mapcar #'(lambda (relative-morph)
                                 (mod (+ relative-morph initial-morph) 7))
                             relative-morph-list))
         (ocm (mapcar #'list onset-list chroma-list morph-list))
         (ocm-chord-list (do* ((cl (list (list (first ocm))))
                               (i 1 (1+ i)))
                              ((= i n)
                               cl)
                           (if (= (first (elt ocm i))
                                  (first (elt ocm (1- i))))
                             (setf (first (last cl))
                                   (append (first (last cl))
                                           (list (elt ocm i))))
                             (setf cl
                                   (append cl
                                           (list (list (elt ocm i))))))))
         (number-of-chords (list-length ocm-chord-list))
         ;neighbour notes
         (ocm-chord-list (do* ((i 0 (1+ i)))
                              ((= i (- number-of-chords 2))
                               ocm-chord-list)
                           (dolist (note1 (elt ocm-chord-list i))
                             (if (member (cdr note1)
                                         (mapcar #'cdr (elt ocm-chord-list (+ i 2)))
                                         :test #'equalp)
                               (dolist (note2 (elt ocm-chord-list (1+ i)))
                                 (if (= (third note2)
                                        (third note1))
                                   (progn (if (member (mod (- (second note2) (second note1)) 12)
                                                      '(1 2))
                                            (setf (third note2)
                                                  (mod (+ 1 (third note2)) 7)))
                                          (if (member (mod (- (second note1) (second note2)) 12)
                                                      '(1 2))
                                            (setf (third note2)
                                                  (mod (- (third note2) 1) 7))))))))))
         ;downward passing notes
         (ocm-chord-list (do* ((i 0 (1+ i)))
                              ((= i (- number-of-chords 2))
                               ocm-chord-list)
                           (dolist (note1 (elt ocm-chord-list i))
                             (dolist (note3 (elt ocm-chord-list (+ i 2)))
                               (if (= (third note3) (mod (- (third note1) 2) 7))
                                 (dolist (note2 (elt ocm-chord-list (1+ i)))
                                   (if (and (or (= (third note2)
                                                   (third note1))
                                                (= (third note2)
                                                   (third note3)))
                                            (< 0
                                               (mod (- (second note1) (second note2)) 12)
                                               (mod (- (second note1) (second note3)) 12)))
                                     (unless (remove-if #'null
                                                        (mapcar #'(lambda (note)
                                                                    (/= (second note)
                                                                        (second note2)))
                                                                (remove-if #'null
                                                                           (mapcar #'(lambda (note)
                                                                                       (if (= (third note)
                                                                                              (mod (- (third note1) 1) 7))
                                                                                         note))
                                                                                   (elt ocm-chord-list (1+ i)))))) 
                                       (setf (third note2)
                                           (mod (- (third note1) 1) 7))))))))))
         ;upward passing notes
         (ocm-chord-list (do* ((i 0 (1+ i)))
                              ((= i (- number-of-chords 2))
                               ocm-chord-list)
                           (dolist (note1 (elt ocm-chord-list i))
                             (dolist (note3 (elt ocm-chord-list (+ i 2)))
                               (if (= (third note3) (mod (+ (third note1) 2) 7))
                                 (dolist (note2 (elt ocm-chord-list (1+ i)))
                                   (if (and (or (= (third note2)
                                                   (third note1))
                                                (= (third note2)
                                                   (third note3)))
                                            (< 0
                                               (mod (- (second note3) (second note2)) 12)
                                               (mod (- (second note3) (second note1)) 12)))
                                     (unless (remove-if #'null
                                                        (mapcar #'(lambda (note)
                                                                    (/= (second note)
                                                                        (second note2)))
                                                                (remove-if #'null
                                                                           (mapcar #'(lambda (note)
                                                                                       (if (= (third note)
                                                                                              (mod (+ (third note1) 1) 7))
                                                                                         note))
                                                                                   (elt ocm-chord-list (1+ i)))))) 
                                       (setf (third note2)
                                           (mod (+ (third note1) 1) 7))))))))))
         
         (morph-list (mapcar #'third (apply #'append ocm-chord-list)))
         (morphetic-pitch-list (mapcar #'(lambda (chromatic-pitch morph)
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
                                       chromatic-pitch-list
                                       morph-list))
         (opcpm (mapcar #'(lambda (opcpm-datapoint morphetic-pitch)
                            (list (first opcpm-datapoint)
                                  (second opcpm-datapoint)
                                  morphetic-pitch))
                        opcpm-test
                        morphetic-pitch-list))
         (opcpm-rd2 (mapcar #'(lambda (opcpm-datapoint)
                                (list (first opcpm-datapoint)
                                      (second opcpm-datapoint)
                                      (1+ (third opcpm-datapoint))))
                            opcpm))
         (opcpm-fd2 (mapcar #'(lambda (opcpm-datapoint)
                                (list (first opcpm-datapoint)
                                      (second opcpm-datapoint)
                                      (1- (third opcpm-datapoint))))
                            opcpm))
         (error-list (remove-if #'null (mapcar #'(lambda (a b)
                                          (if (not (equalp a b))
                                            (list b a)))
                                      opcpm
                                      opcpm-test)))
         (number-of-errors (list-length error-list))
         (transposition 'p1)
         (error-list-rd2 (remove-if #'null (mapcar #'(lambda (a b)
                                                       (if (not (equalp a b))
                                                         (list b a)))
                                                   opcpm-rd2
                                                   opcpm-test)))
         (number-of-errors-rd2 (list-length error-list-rd2))
         (number-of-errors (if (< number-of-errors-rd2 number-of-errors)
                             (setf error-list error-list-rd2
                                   transposition 'rd2
                                   number-of-errors number-of-errors-rd2
                                   )
                             number-of-errors))
         (error-list-fd2 (remove-if #'null (mapcar #'(lambda (a b)
                                                       (if (not (equalp a b))
                                                         (list b a)))
                                                   opcpm-fd2
                                                   opcpm-test)))
         (number-of-errors-fd2 (list-length error-list-fd2))
         (number-of-errors (if (< number-of-errors-fd2 number-of-errors)
                             (setf error-list error-list-fd2
                                   transposition 'fd2
                                   number-of-errors number-of-errors-fd2
                                   )
                             number-of-errors))
	 (error-list (mapcar #'(lambda (error-pair)
                        (list (list (first (first error-pair))
                                    (p-pn (list (second (first error-pair))
                                                (third (first error-pair)))))
                              (list (first (second error-pair))
                                    (p-pn (list (second (second error-pair))
                                                (third (second error-pair)))))))
                    error-list))
         (percentage-correct (* 100.0 (/ (- n number-of-errors) n)))
         )
    
    (if supertest nil
(format t 
        "
File: ~a
Number of errors = ~d.
Number of datapoints = ~d.
Percentage correct = ~,2f%
Transposition = ~a
Errors: ~a

"
        (pathname-name opnd-file-name)
        number-of-errors
        n
        percentage-correct
        transposition
        error-list
        ;error-prediction-list
        ;number-of-incorrectly-predicted-errors
        ;number-of-unpredicted-errors
        ;(if (> number-of-errors 0) 
        ;  (* 100.0 (/ number-of-correctly-predicted-errors number-of-errors))
        ;  100.0)
        ))
    (values number-of-errors
            n)))

(defun ps13-for-supertest (&optional (input-file-name nil) (init-morph nil) pre-context post-context)
  (let* ((chromamorph-table (list 0 1 1 2 2 3 3 4 5 5 6 6))
         (file-name (if input-file-name input-file-name (choose-file-dialog)))
         (opcpm-test (remove-duplicates (sort (mapcar #'(lambda (opc-datapoint)
                                                          (list (first opc-datapoint)
                                                                (second opc-datapoint)
                                                                (third opc-datapoint)))
                                                      (with-open-file (opc-file
                                                                       file-name)
                                                        (read opc-file))) #'vector-less-than) :test #'equalp))
         (onset-list (mapcar #'first opcpm-test))
         (chromatic-pitch-list (mapcar #'second opcpm-test))
         (chroma-list (mapcar #'chromatic-pitch-chroma chromatic-pitch-list))
         (n (list-length chroma-list))
         (chroma-vector-list (do* ((cvl nil)
                                   (i 0 (1+ i)))
                                  ((= i n)
                                   cvl)
                               (setf cvl
                                     (append cvl
                                             (list (do* ((context (subseq chroma-list
                                                                          (max 0 (- i pre-context))
                                                                          (min n (+ i post-context))))
                                                         (cv (list 0 0 0 0 0 0 0 0 0 0 0 0))
                                                         (c 0 (+ 1 c)))
                                                        ((= c 12)
                                                         cv)
                                                     (setf (elt cv c)
                                                           (count c context))))))))
         (spelling-table (do* ((first-morph nil nil)
                               (spelling nil nil)
                               (spelling2 nil nil)
                               (st nil)
                               (c 0 (1+ c)))
                              ((= c 12)
                               st)
                           (setf spelling (mapcar #'(lambda (chroma-in-chroma-list)
                                                      (elt chromamorph-table
                                                           (mod (- chroma-in-chroma-list c) 12)))
                                                  chroma-list))
                           (setf first-morph (first spelling))
                           (setf spelling2 (mapcar #'(lambda (morph-in-spelling)
                                                       (mod (- morph-in-spelling first-morph) 7))
                                                   spelling))
                           (setf st (append st (list spelling2)))))
         
         (relative-morph-list (do ((morph-vector (list 0 0 0 0 0 0 0) 
                                                 (list 0 0 0 0 0 0 0))
                                   (rml nil)
                                   (i 0 (1+ i))
                                   (morphs-for-this-chroma nil 
                                                           nil)
                                   )
                                  ((= i n)
                                   rml)
                                (setf morphs-for-this-chroma
                                      (mapcar #'(lambda (spelling)
                                                  (elt spelling i))
                                              spelling-table))
                                (setf rml (do ((prev-score nil nil)
                                               (j 0 (1+ j)))
                                              ((= j 12)
                                               ;(pprint morph-vector)
                                               (append rml (list (position (apply #'max morph-vector)
                                                                           morph-vector))))
                                            (setf prev-score (elt morph-vector (elt morphs-for-this-chroma j)))
                                            (setf (elt morph-vector (elt morphs-for-this-chroma j))
                                                  (+ prev-score (elt (elt chroma-vector-list i) j)))))))
         (initial-morph (if init-morph init-morph (elt '(0 1 1 2 2 3 4 4 5 5 6 6) (mod (first chromatic-pitch-list) 12))))
         (morph-list (mapcar #'(lambda (relative-morph)
                                 (mod (+ relative-morph initial-morph) 7))
                             relative-morph-list))
         (ocm (mapcar #'list onset-list chroma-list morph-list))
         (ocm-chord-list (do* ((cl (list (list (first ocm))))
                               (i 1 (1+ i)))
                              ((= i n)
                               cl)
                           (if (= (first (elt ocm i))
                                  (first (elt ocm (1- i))))
                             (setf (first (last cl))
                                   (append (first (last cl))
                                           (list (elt ocm i))))
                             (setf cl
                                   (append cl
                                           (list (list (elt ocm i))))))))
         (number-of-chords (list-length ocm-chord-list))
         ;neighbour notes
         (ocm-chord-list (do* ((i 0 (1+ i)))
                              ((= i (- number-of-chords 2))
                               ocm-chord-list)
                           (dolist (note1 (elt ocm-chord-list i))
                             (if (member (cdr note1)
                                         (mapcar #'cdr (elt ocm-chord-list (+ i 2)))
                                         :test #'equalp)
                               (dolist (note2 (elt ocm-chord-list (1+ i)))
                                 (if (= (third note2)
                                        (third note1))
                                   (progn (if (member (mod (- (second note2) (second note1)) 12)
                                                      '(1 2))
                                            (setf (third note2)
                                                  (mod (+ 1 (third note2)) 7)))
                                          (if (member (mod (- (second note1) (second note2)) 12)
                                                      '(1 2))
                                            (setf (third note2)
                                                  (mod (- (third note2) 1) 7))))))))))
         ;downward passing notes
         (ocm-chord-list (do* ((i 0 (1+ i)))
                              ((= i (- number-of-chords 2))
                               ocm-chord-list)
                           (dolist (note1 (elt ocm-chord-list i))
                             (dolist (note3 (elt ocm-chord-list (+ i 2)))
                               (if (= (third note3) (mod (- (third note1) 2) 7))
                                 (dolist (note2 (elt ocm-chord-list (1+ i)))
                                   (if (and (or (= (third note2)
                                                   (third note1))
                                                (= (third note2)
                                                   (third note3)))
                                            (< 0
                                               (mod (- (second note1) (second note2)) 12)
                                               (mod (- (second note1) (second note3)) 12)))
                                     (unless (remove-if #'null
                                                        (mapcar #'(lambda (note)
                                                                    (/= (second note)
                                                                        (second note2)))
                                                                (remove-if #'null
                                                                           (mapcar #'(lambda (note)
                                                                                       (if (= (third note)
                                                                                              (mod (- (third note1) 1) 7))
                                                                                         note))
                                                                                   (elt ocm-chord-list (1+ i)))))) 
                                       (setf (third note2)
                                           (mod (- (third note1) 1) 7))))))))))
         ;upward passing notes
         (ocm-chord-list (do* ((i 0 (1+ i)))
                              ((= i (- number-of-chords 2))
                               ocm-chord-list)
                           (dolist (note1 (elt ocm-chord-list i))
                             (dolist (note3 (elt ocm-chord-list (+ i 2)))
                               (if (= (third note3) (mod (+ (third note1) 2) 7))
                                 (dolist (note2 (elt ocm-chord-list (1+ i)))
                                   (if (and (or (= (third note2)
                                                   (third note1))
                                                (= (third note2)
                                                   (third note3)))
                                            (< 0
                                               (mod (- (second note3) (second note2)) 12)
                                               (mod (- (second note3) (second note1)) 12)))
                                     (unless (remove-if #'null
                                                        (mapcar #'(lambda (note)
                                                                    (/= (second note)
                                                                        (second note2)))
                                                                (remove-if #'null
                                                                           (mapcar #'(lambda (note)
                                                                                       (if (= (third note)
                                                                                              (mod (+ (third note1) 1) 7))
                                                                                         note))
                                                                                   (elt ocm-chord-list (1+ i)))))) 
                                       (setf (third note2)
                                           (mod (+ (third note1) 1) 7))))))))))
         #|
(ocm-chord-list (do* ((complete-chromamorph-table (old-compute-chromamorph-table))
                               (i 0 (1+ i)))
                                ((= i (- number-of-chords 1))
                                 ocm-chord-list)
                             (dolist (this-note (elt ocm-chord-list i))
                               (let* ((this-note-chromamorph (cdr this-note))
                                      (this-note-chromamorph-raised (list (first this-note-chromamorph)
                                                                          (mod (1+ (second this-note-chromamorph)) 7)))
                                      (this-note-chromamorph-lowered (list (first this-note-chromamorph)
                                                                           (mod (1- (second this-note-chromamorph)) 7)))
                                      (context (mapcar #'cdr (remove this-note
                                                                     (apply #'append (subseq ocm-chord-list
                                                                                             (max 0 (- i 3))
                                                                                             (min number-of-chords (+ i 1))))
                                                                     :test #'equalp)))
                                      (context-length (list-length context))
                                      (list-of-scores-for-this-note (append (list 1)
                                                                            (mapcar #'(lambda (q-int)
                                                                                        (q-int-hd q-int complete-chromamorph-table))
                                                                                    (mapcar #'(lambda (chromamorph)
                                                                                                (q-q-int this-note-chromamorph chromamorph))
                                                                                            context))))
                                      ;(dummy (pprint list-of-scores-for-this-note))
                                      (max-score-for-this-note (apply #'max list-of-scores-for-this-note))
                                      (score-for-this-note (/ (apply #'+ list-of-scores-for-this-note)
                                                              (+ 1 context-length)))
                                      (score-for-this-note-raised (/ (apply #'+ (mapcar #'(lambda (q-int)
                                                                                           (q-int-hd q-int complete-chromamorph-table))
                                                                                       (mapcar #'(lambda (chromamorph)
                                                                                                   (q-q-int this-note-chromamorph-raised
                                                                                                            chromamorph))
                                                                                               context)))
                                                                     (+ 1 context-length)))
                                      (score-for-this-note-lowered (/ (apply #'+ (mapcar #'(lambda (q-int)
                                                                                            (q-int-hd q-int complete-chromamorph-table))
                                                                                        (mapcar #'(lambda (chromamorph)
                                                                                                    (q-q-int this-note-chromamorph-lowered
                                                                                                             chromamorph))
                                                                                                context)))
                                                                      (+ 1 context-length)))
                                      (score-list-for-this-note (list score-for-this-note
                                                                      score-for-this-note-raised
                                                                      score-for-this-note-lowered))
                                      (morph-list-for-this-note (mapcar #'second (list this-note-chromamorph
                                                                                       this-note-chromamorph-raised
                                                                                       this-note-chromamorph-lowered)))
                                      (best-morph-for-this-note (elt morph-list-for-this-note 
                                                                     (position (apply #'min score-list-for-this-note)
                                                                               score-list-for-this-note))))
                                 ;dummy
                                 (if (> max-score-for-this-note 4)
                                   (setf (third this-note) best-morph-for-this-note))))))
|#
         (morph-list (mapcar #'third (apply #'append ocm-chord-list)))
         (morphetic-pitch-list (mapcar #'(lambda (chromatic-pitch morph)
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
                                       chromatic-pitch-list
                                       morph-list))
         (opcpm (mapcar #'(lambda (opcpm-datapoint morphetic-pitch)
                            (list (first opcpm-datapoint)
                                  (second opcpm-datapoint)
                                  morphetic-pitch))
                        opcpm-test
                        morphetic-pitch-list))
         (error-list (remove-if #'null (mapcar #'(lambda (a b)
                                                   (if (not (equalp a b))
                                                     (list b a)))
                                               opcpm
                                               opcpm-test)))
         (error-list (mapcar #'(lambda (error-pair)
                                 (list (list (first (first error-pair))
                                             (p-pn (list (second (first error-pair))
                                                         (third (first error-pair)))))
                                       (list (first (second error-pair))
                                             (p-pn (list (second (second error-pair))
                                                         (third (second error-pair)))))))
                             error-list))
         (number-of-errors (list-length error-list))
         #|
(number-of-correctly-predicted-errors (list-length (intersection error-prediction-list
                                                                          (mapcar #'second error-list)
                                                                          :test #'equalp)))
         (number-of-incorrectly-predicted-errors (- (list-length error-prediction-list)
                                                    number-of-correctly-predicted-errors))
         (number-of-unpredicted-errors (- number-of-errors
                                          number-of-correctly-predicted-errors))
|#
         ;(percentage-correct (* 100.0 (/ (- n number-of-errors) n)))
         )
    
    
    (values number-of-errors
            n)))

(defun q-q-int (q1 q2)
  (list (mod (- (first q2) (first q1)) muc)
        (mod (- (second q2) (second q1)) mum)))

(defun compute-scores-for-note (chromamorph-interval-list 
                                context
                                complete-chromamorph-table)
  (mapcar #'(lambda (chromamorph-interval time-distance)
              (/ (second (first (member chromamorph-interval
                                        complete-chromamorph-table
                                        :test #'(lambda (cmi cte)
                                                  (equalp cmi
                                                          (list (first cte)
                                                                (third cte)))))))
                 time-distance))
          chromamorph-interval-list
          (mapcar #'second context)))

(defun chromatic-pitch-chroma (chromatic-pitch)
  (mod chromatic-pitch 12))

(defun pathname-directory-to-string (pn)
  (let ((pns (concatenate 'string (second pn) ":")))
    (dolist (pnelt (cddr pn) pns)
      (setf pns (concatenate 'string
                             pns 
                             pnelt
                             ":")))))

(defun vector-minus (v1 v2)
  (mapcar #'- v1 v2))

(defun vector-less-than (v1 v2)
  (cond ((null v2) nil)
        ((null v1) t)
        ((< (first v1) (first v2)) t)
        ((> (first v1) (first v2)) nil)
        (t (vector-less-than (cdr v1) (cdr v2)))))

#|
Algorithms for converting between A.S.A. pitch names and MIPS pitches.
|#

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
  (bmod (p-pm p) mum))

(defun bmod (x y)
  (- x
     (* y
        (int (/ x y)))))

(defun p-pm (p)
  (second p))

(defun int (x)
  (values (floor x)))

(defun p-gc (p)
  (- (p-pc p)
     (* muc (p-om p))))

(defun p-pc (p)
  (first p))

(defun p-om (p)
  (div (p-pm p) mum))

(defun div (x y)
  (int (/ x y)))

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
         (oasa (if is-good-i (string-to-number o)))
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

(defun string-to-number (s)
  (if (well-formed-number-string-p s)
    (if (string-is-negative-p s)
      (let ((n 0))
        (dotimes (i (- (length s) 1) (* -1 n))
          (setf n (+ (* 10 n)
                     (- (char-code (elt s (+ 1 i)))
                        (char-code #\0))))))
      (let ((n 0))
        (dotimes (i (length s) n)
          (setf n (+ (* 10 n)
                     (- (char-code (elt s i))
                        (char-code #\0)))))))))

(defun string-is-negative-p (s)
  (equalp #\- (char s 0)))

;(string-is-negative-p "23")

(defun well-formed-number-string-p (s)
  (let ((wf t))
    (dotimes (i (length s) wf)
      (if (not (or (<= (char-code #\0) (char-code (char s i)) (char-code #\9))
                   (and (= i 0) 
                        (equalp (char s i) #\-))))
        (setf wf nil)))))

#|
(well-formed-number-string-p "23")
|#

(defun well-formed-inflection-p (i)
  (or (equalp i "N")
      (let ((wf t))
        (dotimes (j (length i) wf)
          (if (not (equalp (char i j) #\F))
            (setf wf nil))))
      (let ((wf t))
        (dotimes (j (length i) wf)
          (if (not (member (char i j) '(#\S #\#)))
            (setf wf nil))))))

#|
TESTS FOR p-pn and pn-p

(mapcar #'p-pn
        '((0 0) (-1 0) (0 -1) (-9 -5) (-10 -5) (-9 -6) (39 23) (52 27) (52 34) (39 22) (38 23)))

(mapcar #'pn-p
        '("An0" "Af0" "Gss0" "Cn0" "Cf0" "Bs-1" "Cn4" "Gssssss4" "Gffffff5" "Bs3" "Cf4"))
|#

(defun pi-pin (pint)
  (let* ((pmint (p-int-pm-int pint))
         (d (cond ((= 0 pmint) "")
                  ((> pmint 0) "r")
                  ((< pmint 0) "f")))
         (sdash (+ 1 (abs pmint)))
         (s (format nil "~D" sdash))
         (mintdash (bmod (abs pmint) mum))
         (cintdash (elt '(0 2 4 5 7 9 11) mintdash))
         (tdash (elt '("p" "ma" "ma" "p" "p" "ma" "ma") mintdash))
         (e (if (>= pmint 0) (- (p-int-gc-int pint) cintdash) (- (p-int-gc-int (invp pint)) cintdash)))
         (ty (cond ((and (equalp tdash "p") (= e 0))
                   "p")
                  ((and (equalp tdash "p") (> e 0))
                   (let ((x "")) (dotimes (i e x) (setf x (concatenate 'string x "a")))))
                  ((and (equalp tdash "p") (< e 0))
                   (let ((x "")) (dotimes (i (- e) x) (setf x (concatenate 'string x "d")))))
                  ((and (equalp tdash "ma") (= e 0))
                   "ma")
                  ((and (equalp tdash "ma") (= e -1))
                   "mi")
                  ((and (equalp tdash "ma") (< e -1))
                   (let ((x "")) (dotimes (i (- (- e) 1) x) (setf x (concatenate 'string x "d")))))
                  ((and (equalp tdash "ma") (> e 0))
                   (let ((x "")) (dotimes (i e x) (setf x (concatenate 'string x "a"))))))))
    (concatenate 'string d ty s)))

(defun p-int-pm-int (pint)
  (second pint))

(defun p-int-gc-int (pint)
  (- (p-int-pc-int pint)
     (* muc 
        (div (p-int-pm-int pint)
             mum))))

(defun p-int-pc-int (pint)
  (first pint))

(defun invp (pint)
  (list (- (p-int-pc-int pint))
        (- (p-int-pm-int pint))))

#|
Tests for pi-pin and pin-pi

(mapcar #'pi-pin
        '((0 0) (2 1) (1 1) (3 1) (0 1) (-1 1) (4 1) (-7 -4) 
          (-6 -4) (-5 -4) (-17 -10) (0 7) (-1 0) (1 0)))
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
         (s-dash (if (or s-error ty-error) nil (string-to-number s)))
         (pmintvar (if (or s-error ty-error) nil (if (equalp d "f") (- 1 s-dash) (- s-dash 1))))
         (mint-dash (if (or s-error ty-error) nil (bmod (abs pmintvar) mum)))
         (cint-dash (if (or s-error ty-error) nil (elt '(0 2 4 5 7 9 11) mint-dash)))
         (pcintone (if (or s-error ty-error) nil (+ cint-dash
                                                    (* muc
                                                       (div (abs pmintvar)
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

#|
(mapcar #'pin-pi
        '(rma2 ra2 rd2 rdd2 fp5 fd5 fp11 rdddddddddddd8 d1 a1))
(pin-pi 'd1)
(setf pitch-interval-name 'd1)
|#

#|
Functions for transposing pitches and pitch names by intervals and interval names.
|#

(defun p-tran (p pint)
  (list (+ (p-pc p) (p-int-pc-int pint))
        (+ (p-pm p) (p-int-pm-int pint))))

(defun pn-tran (pitch-name pitch-interval-name)
  (p-pn (p-tran (pn-p pitch-name) (pin-pi pitch-interval-name))))

#|
Test of pitch name transposition function:

Construction of seconds relation pitch name digraph:
|#

(defun two-dim-pitch-name-digraph (initial-pitch-name 
                                   x-pitch-interval-name
                                   y-pitch-interval-name
                                   x-int-span
                                   y-int-span)
  (let* (pn
         (row (list (setf pn (pn-tran initial-pitch-name 'p1))))
         (row (dotimes (x (- x-int-span 1) row) 
                (setf row (append row
                                  (list (setf pn (pn-tran pn x-pitch-interval-name)))))))
         (digraph (list row)))
    (dotimes (y (- y-int-span 1) digraph)
      (setf digraph (cons (setf row (mapcar #'(lambda (pitch-name)
                                                (pn-tran pitch-name y-pitch-interval-name))
                                            row))
                          digraph)))))

(defun find-all-genus-occurrences (genus)
  (let* ((file-name (choose-file-dialog))
         (dataset (with-open-file (dataset-stream
                                   file-name)
                    (read dataset-stream))))
    (pprint (remove-if #'null (mapcar #'(lambda (datapoint)
                                          (if (equalp (list (p-gc (list (second datapoint)
                                                                        (third datapoint)))
                                                            (p-m (list (second datapoint)
                                                                       (third datapoint))))
                                                      genus)
                                            (list (first datapoint)
                                                  (p-pn (list (second datapoint)
                                                              (third datapoint))))))
                                      dataset)))))

(defun find-chroma-vector ()
  (let* ((file-name (choose-file-dialog))
         (dataset (with-open-file (dataset-stream
                                   file-name)
                    (read dataset-stream)))
         (chromatic-pitch-list (mapcar #'second dataset))
         (chroma-list (mapcar #'chromatic-pitch-chroma chromatic-pitch-list))
         (chroma-vector (do* ((cv '(0 0 0 0 0 0 0 0 0 0 0 0))
                              (c 0 (+ 1 c)))
                             ((= c 12)
                              cv)
                          (setf (elt cv c)
                                (count c chroma-list)))))
    chroma-vector))

(defun old-compute-chromamorph-table (&optional (table-size 9))
  (remove-duplicates  (sort (do* ((cmt nil)
                                  (x (- table-size) (+ 1 x)))
                                 ((= x table-size)
                                  cmt)
                              (do* ((cmt-for-this-x nil)
                                    (y (- table-size) (+ 1 y)))
                                   ((= y table-size)
                                    (setf cmt
                                          (append cmt-for-this-x cmt)))
                                (setf cmt-for-this-x
                                      (cons (list (mod (+ (* x 4) (* y 7)) 12)
                                                  (+ (abs x) (abs y))
                                                  (mod (+ (* x 2) (* y 4)) 7))
                                            cmt-for-this-x))))
                            #'vector-less-than)
                      :from-end t
                      :test #'(lambda (a b)
                                (and (equalp (first a) (first b))
                                     (equalp (third a) (third b))))))


(defun pitch-interval-chromamorph-interval (pitch-interval)
  (list (chromatic-pitch-interval-chroma-interval (pitch-interval-chromatic-pitch-interval pitch-interval))
        (morphetic-pitch-interval-morph-interval (pitch-interval-morphetic-pitch-interval pitch-interval))))

(defun chromatic-pitch-interval-chroma-interval (chromatic-pitch-interval)
  (mod chromatic-pitch-interval muc))

(defun morphetic-pitch-interval-morph-interval (morphetic-pitch-interval)
  (mod morphetic-pitch-interval mum))

(defun pitch-interval-chromatic-pitch-interval (pitch-interval)
  (first pitch-interval))

(defun pitch-interval-morphetic-pitch-interval (pitch-interval)
  (second pitch-interval))
         
(defun test-chromamorph-table (table-size)
  (let ((chromamorph-table (old-compute-chromamorph-table table-size))
        (absent-interval-list nil))
    (do* ((c 0 (1+ c)))
         ((= c 12)
          absent-interval-list)
      (do* ((m 0 (1+ m)))
           ((= m 7)
            )
        (if (member (list c m)
                    chromamorph-table
                    :test #'(lambda (cmi cte)
                              (equalp cmi
                                      (list (first cte)
                                            (third cte)))))
          nil
          (setf absent-interval-list
                (append absent-interval-list
                        (list (list c m)))))))))

(defun pin-hd (pitch-interval-name)
  (second (first (member (pin-pi pitch-interval-name)
                         (old-compute-chromamorph-table)
                         :test #'(lambda (a b) (equalp a (list (first b) (third b))))))))

(defun q-int-hd (chromamorph-interval &optional (complete-chromamorph-table nil))
  (second (first (member chromamorph-interval
                         (if complete-chromamorph-table complete-chromamorph-table (old-compute-chromamorph-table))
                         :test #'(lambda (a b) (equalp a (list (first b) (third b))))))))
(defun opd-opn (&optional (voice nil))
  (let* ((opd (with-open-file (opd-file
                               (choose-file-dialog))
                (read opd-file)))
         (opn (mapcar #'(lambda (opd-datapoint)
                          (append (list (first opd-datapoint)
                                        (p-pn (list (second opd-datapoint)
                                                    (third opd-datapoint))))
                                  (last opd-datapoint 2)))
                      opd))
         (opn (if voice
                (remove-if #'(lambda (datapoint)
                               (not (= voice (first (last datapoint)))))
                           opn)
                opn)))
    opn))

;(ps13-test)

#|
The function compute-chromamorph-table computes a list like
(list 0 1 1 2 2 3 3 4 5 5 6 6) as used in ps13.

The function takes as its argument a list of pitch intervals
((ic-1 im-1) (ic-2 im-2)...(ic-n im-n))

Then, for each chroma interval, 0 <= c <= 11, it finds the vector

v = (v-1 v-2 ... v-n)

that satisfies

C1. SUM_{k=1}^n (v-k * ic-k) mod 12 = c

C2. SUM_{k=1}^n v-k is a minimum

C3. v-k >= 0 for all 1 <= k <= n 

If there is more than one vector v satisfying C1-C3...


|#

#|
The following function may have chromamorph ambiguity effects.
|#

(defun compute-chromamorph-table (interval-list)
  (do* ((il (remove-duplicates interval-list :test #'equalp))
        (table-size 1 (1+ table-size))
        (number-of-intervals (list-length il))
        (max-v-number (expt (1+ table-size) number-of-intervals)
                      (expt (1+ table-size) number-of-intervals))
        (all-chromas (list 0 1 2 3 4 5 6 7 8 9 10 11))
        (table nil)
        (chromas-in-table nil))
       ((equalp all-chromas chromas-in-table)
        (let* ((chromamorph-table nil))
          (format t "~%Table size = ~d~%" table-size)
          (mapcar #'(lambda (morphs-for-this-chroma)
                      (let* ((least-distance (fifth (first morphs-for-this-chroma))))
                        (remove-if #'(lambda (this-morph)
                                       (> (fifth this-morph)
                                          least-distance))
                                   morphs-for-this-chroma)))
                  (dotimes (c 12 (reverse chromamorph-table))
                    (setf chromamorph-table
                          (cons (sort (remove-if-not #'(lambda (table-element)
                                                         (= (third table-element)
                                                            c))
                                                     table)
                                      #'<
                                      :key #'fifth)
                                chromamorph-table))))))
    (setf table (let* ((tab nil))
                  (dotimes (v-number max-v-number tab)
                    (let* ((v (base-n-list v-number (1+ table-size) number-of-intervals))
                           (minus-v (mapcar #'- v))
                           (distance (apply #'+ v))
                           (chroma-up (mod (apply #'+ (mapcar #'*
                                                           v
                                                           (mapcar #'first il)))
                                        12))
                           (morph-up (mod (apply #'+ (mapcar #'*
                                                          v
                                                          (mapcar #'second il)))
                                       7))
                           (chroma-down (mod (apply #'+ (mapcar #'*
                                                           minus-v
                                                           (mapcar #'first il)))
                                        12))
                           (morph-down (mod (apply #'+ (mapcar #'*
                                                          minus-v
                                                          (mapcar #'second il)))
                                       7)))
                      (setf tab
                            (cons
                             (list (- v-number)
                                   minus-v
                                   chroma-down
                                   morph-down
                                   distance)
                             (cons 
                              (list v-number
                                    v
                                    chroma-up
                                    morph-up
                                    distance)
                              tab))))))
          chromas-in-table (sort (remove-duplicates (mapcar #'third table)) #'<))))

(defun base-n-list (k base number-of-digits)
  (let* ((base-n-string (format nil (concatenate 'string
                                                 "~"
                                                 (format nil "~d" base)
                                                 "R")
                                k))
         (length-of-string (length base-n-string))
         (output-list nil)
         (digit-list (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
         (this-digit-value nil))
    (dotimes (i (max length-of-string number-of-digits) (reverse output-list))
      (if (< i length-of-string)
        (setf output-list
              (cons (if (setf this-digit-value 
                              (position (elt base-n-string i) digit-list))
                      this-digit-value
                      (+ 10 (- (char-code (elt base-n-string i)) (char-code #\A))))
                    output-list))
        (setf output-list
              (append output-list (list 0)))))))

#|
(dotimes (i 100)
  (pprint (base-n-list i 15)))
|#

#|
(compute-chromamorph-table '((7 4)))

Table size = 7
(((0 (0) 0 0 0) (0 (0) 0 0 0))
 ((5 (5) 1 1 5))
 ((2 (2) 2 1 2))
 ((3 (3) 3 2 3))
 ((4 (4) 4 2 4))
 ((1 (1) 5 3 1))
 ((6 (6) 6 4 6) (6 (6) 6 3 6))
 ((1 (1) 7 4 1))
 ((4 (4) 8 5 4))
 ((3 (3) 9 5 3))
 ((2 (2) 10 6 2))
 ((5 (5) 11 6 5)))

(compute-chromamorph-table '((4 2) (3 2)))

Table size = 4
(((0 (0) 0 0 0) (0 (0) 0 0 0) (-12 (-3 0) 0 1 3) (12 (3 0) 0 6 3) (-3 (-3) 0 1 3) (3 (3) 0 6 3))
 ((-9 (-2 -1) 1 1 3) (7 (1 3) 1 1 4))
 ((-6 (-1 -2) 2 1 3) (10 (2 2) 2 1 4))
 ((13 (3 1) 3 1 4) (-15 (-3 -3) 3 2 6))
 ((4 (1 0) 4 2 1) (1 (1) 4 2 1) (-8 (-2 0) 4 3 2) (-2 (-2) 4 3 2))
 ((-5 (-1 -1) 5 3 2) (11 (2 3) 5 3 5))
 ((-14 (-3 -2) 6 4 5) (14 (3 2) 6 3 5))
 ((5 (1 1) 7 4 2) (-11 (-2 -3) 7 4 5))
 ((-4 (-1 0) 8 5 1) (-1 (-1) 8 5 1) (8 (2 0) 8 4 2) (2 (2) 8 4 2))
 ((-13 (-3 -1) 9 6 4) (15 (3 3) 9 5 6))
 ((6 (1 2) 10 6 3) (-10 (-2 -2) 10 6 4))
 ((9 (2 1) 11 6 3) (-7 (-1 -3) 11 6 4)))


(compute-chromamorph-table '((4 2) (3 2)))

Table size = 4
(((0 (0 0) 0 0 0) (0 (0 0) 0 0 0) (-12 (-3 0) 0 1 3) (12 (3 0) 0 6 3) (-3 (-3 0) 0 1 3) (3 (3 0) 0 6 3))
 ((-9 (-2 -1) 1 1 3) (7 (1 3) 1 1 4))
 ((-6 (-1 -2) 2 1 3) (10 (2 2) 2 1 4))
 ((13 (3 1) 3 1 4) (-15 (-3 -3) 3 2 6))
 ((4 (1 0) 4 2 1) (1 (1 0) 4 2 1) (-8 (-2 0) 4 3 2) (-2 (-2 0) 4 3 2))
 ((-5 (-1 -1) 5 3 2) (11 (2 3) 5 3 5))
 ((-14 (-3 -2) 6 4 5) (14 (3 2) 6 3 5))
 ((5 (1 1) 7 4 2) (-11 (-2 -3) 7 4 5))
 ((-4 (-1 0) 8 5 1) (-1 (-1 0) 8 5 1) (8 (2 0) 8 4 2) (2 (2 0) 8 4 2))
 ((-13 (-3 -1) 9 6 4) (15 (3 3) 9 5 6))
 ((6 (1 2) 10 6 3) (-10 (-2 -2) 10 6 4))
 ((9 (2 1) 11 6 3) (-7 (-1 -3) 11 6 4)))

(compute-chromamorph-table '((4 2) (3 2)))

Table size = 3
(((0 (0 0) 0 0 0) (0 (0 0) 0 0 0))
 ((-7 (-2 -1) 1 1 3))
 ((-5 (-1 -2) 2 1 3) (8 (2 2) 2 1 4))
 ((1 (0 1) 3 2 1))
 ((3 (1 0) 4 2 1) (-6 (-2 0) 4 3 2))
 ((-4 (-1 -1) 5 3 2))
 ((-2 (0 -2) 6 3 2) (2 (0 2) 6 4 2))
 ((4 (1 1) 7 4 2))
 ((-3 (-1 0) 8 5 1) (6 (2 0) 8 4 2))
 ((-1 (0 -1) 9 5 1))
 ((5 (1 2) 10 6 3) (-8 (-2 -2) 10 6 4))
 ((7 (2 1) 11 6 3)))

(compute-chromamorph-table '((4 2) (3 2)))

Table size = 3
(((0 (0 0) 0 0 0) (0 (0 0) 0 0 0))
 ((-7 (-2 -1) 1 1 3))
 ((-5 (-1 -2) 2 1 3))
 ((1 (0 1) 3 2 1))
 ((3 (1 0) 4 2 1))
 ((-4 (-1 -1) 5 3 2))
 ((-2 (0 -2) 6 3 2) (2 (0 2) 6 4 2))
 ((4 (1 1) 7 4 2))
 ((-3 (-1 0) 8 5 1))
 ((-1 (0 -1) 9 5 1))
 ((5 (1 2) 10 6 3))
 ((7 (2 1) 11 6 3)))

|#
