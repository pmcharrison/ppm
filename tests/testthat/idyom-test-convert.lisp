(in-package :ppm-star)

;; Note! Escape method AX is selected with :X, not with :AX
(defun test-model (sequences alphabet &key (ps nil) (escape :c) update-exclusion)
   (let ((model (ppm:make-ppm alphabet :escape escape :mixtures t
                              :update-exclusion update-exclusion :order-bound nil)))
     (prog1 
       (ppm:model-dataset model sequences :construct? t :predict? t)
       (when ps (ppm:write-model-to-postscript model ps)))))

(defun to-R (file x)
  (with-open-file (s file :direction :output :if-exists :supersede)
    (format s "~%x <- list(~%")
    (loop
       for seq in x
       for g from 1
       with M = (length x)
       do (progn
	    (format s "list(~%")
	    (loop
	       for dist in (cdr seq)
	       for h from 1
	       with N = (length (cdr seq))
	       do (progn
		    (format s "c(~%")
		    (loop
		       with n = (length (second dist))
		       for el in (second dist) ;; el = a letter
		       for i from 1
		       do (progn
			    (format s "~A = ~D" (string-downcase (car el)) (second el))
			    (if (equal i n)
				(format s ")")
				(format s ",~%"))))
		    (format s (if (equal h N) ")" ",~%"))))
	    (format s (if (equal g M) ")" ",~%")))))
  x)

(to-r "/home/peter/Dropbox/Academic/projects/harrison-peter/ppm/tests/testthat/data/escape-b-update-excluded.R" 
      (test-model '((a b r a c a d a b r a)) '(a b c d r) :escape :b :ps "/home/peter/Downloads/temp.ps"))

(to-r "/home/peter/Dropbox/Academic/projects/harrison-peter/ppm/tests/testthat/data/escape-v2-a.R" 
      (test-model '((a b r a c a d a b r a)
   	             (l e t l e t t e r t e l e)
   	             (a s s a n i s s i m a s s a)
   	             (m i s s i s s i p p i)
   	             (w o o l o o b o o l o o))
                '(a b c d e i l m n o p r s t w) 
                :escape :a
                :update-exclusion nil
                :ps "/home/peter/Downloads/temp.ps"))
