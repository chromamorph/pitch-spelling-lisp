;; This file contains functions for sorting opnd datasets.(load "hd:2007-04-23:pitch.lisp")(load "hd:2007-04-23:vector-compare.lisp")(defun sort-by-onset (opndv-dataset)  (sort (mapcar #'copy-list opndv-dataset)        #'sort-dps-by-onset))(defun sort-dps-by-onset (opndv-datapoint1 opndv-datapoint2)  (let* ((onset1 (first opndv-datapoint1))         (onset2 (first opndv-datapoint2))         (pitch1 (pn2p (second opndv-datapoint1)))         (pitch2 (pn2p (second opndv-datapoint2)))         (chromatic-pitch1 (first pitch1))         (chromatic-pitch2 (first pitch2))         (morphetic-pitch1 (second pitch1))         (morphetic-pitch2 (second pitch2))         (duration1 (third opndv-datapoint1))         (duration2 (third opndv-datapoint2))         (voice1 (fourth opndv-datapoint1))         (voice2 (fourth opndv-datapoint2)))    (vector-less-than-p (list onset1                               chromatic-pitch1                              duration1                              voice1                              morphetic-pitch1)                        (list onset2                              chromatic-pitch2                              duration2                              voice2                              morphetic-pitch2))))(defun sort-by-voice (opndv-dataset)  (sort (mapcar #'copy-list opndv-dataset)        #'(lambda (opndv-datapoint1 opndv-datapoint2)            (let* ((onset1 (first opndv-datapoint1))                   (onset2 (first opndv-datapoint2))                   (pitch1 (pn2p (second opndv-datapoint1)))                   (pitch2 (pn2p (second opndv-datapoint2)))                   (chromatic-pitch1 (first pitch1))                   (chromatic-pitch2 (first pitch2))                   (morphetic-pitch1 (second pitch1))                   (morphetic-pitch2 (second pitch2))                   (duration1 (third opndv-datapoint1))                   (duration2 (third opndv-datapoint2))                   (voice1 (fourth opndv-datapoint1))                   (voice2 (fourth opndv-datapoint2)))              (vector-less-than-p (list voice1                                        onset1                                         chromatic-pitch1                                        duration1                                        morphetic-pitch1)                                  (list voice2                                        onset2                                        chromatic-pitch2                                        duration2                                        morphetic-pitch2))))))