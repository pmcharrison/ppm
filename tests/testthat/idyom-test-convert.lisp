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
	    (format s (if (equal g M) ")" ",~%"))))))

(to-R "/Users/peter/Dropbox/Academic/projects/maria-chait/PPMdecay/tests/testthat/data/ppm-mix-1.R"
