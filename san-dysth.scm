
#!
 San-Dysth V0.1.1

  Kjetil Matheussen, 2008.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

!#


;; ./snd -noglob -noinit -e '(define *san-dysth-run-standalone* #t)' -b san-dysth.scm


(define *san-dysth-run-standalone* #t)

(if (not (defined? 'snd-ls-version))
    (begin
      (display "Your version of snd-ls is too old. Please upgrade and reinstall san-dysth.")
      (newline)
      (exit)))

(define-macro (run-standalone)
  `(begin
     (define srfi-loaded #f)
     (define *eval-c-cache-dir* (string-append (getenv "HOME") "/.san_dysth/eval-c-cache"))
     (define *rt-jackname-prefix* "san-dysth")
     (define *rt-midi-alsaname* "san-dysth")))


(if (defined? '*san-dysth-run-standalone*)
    (primitive-eval '(run-standalone)))

(if (not (provided? 'snd-rt-compiler.scm))
    (load-from-path "rt-compiler.scm"))

(if (not (provided? 'snd-gui.scm))
    (load-from-path "gui.scm"))

(if (not (provided? 'snd-ladspa.scm))
    (primitive-eval '(begin 
		       (load-from-path "ladspa-help.scm")
		       (define ladspa-help-assoclist '())
		       
		       (define (insert-ladspa-help alist)
			 (if (not (null? alist))
			     (begin
			       (set! ladspa-help-assoclist 
				     (cons (list (string-append (car alist) (cadr alist))
						 (caddr alist) (cadddr alist))
					   ladspa-help-assoclist))
			       (insert-ladspa-help (cddddr alist)))))
		       
		       (insert-ladspa-help ladspa-help-texts))))

(if (defined? '*san-dysth-run-standalone*)
    (set! (rt-safety) 0))



(definstrument (san-dysth :key
				(middlenote 60)
				(polyphony 32)
				(check-overruns #t)

				(das-vol 0.344)
				(pitch 0.794)
				(octave 6)
				(period 920)
				(maximum-drunk-change (/ 8.226 10000))
				(unrandom 0.0)
				(maximum-add-val (/ 1.545 100))
				(a1 0.698)
				(a2 0)
				(a3 0)
				(b1 0.474)
				(b2 0.723)
				(b3 0.426)								
				(attack 0.093)
				(attack-peak 3.699)
				(sustain 0.05)
				(release 0.05)
				(src-width 20)
				(regular #f)
				(reverb-val 0.922))
  
  (define* (make-reverb-old)
    (let ((l (make-ladspa "cmt" "freeverb3")))
      (let ((plugin l))
	(ladspa-set! plugin 0 0.0 )
	(ladspa-set! plugin 1 0.814999997615814 )
	(ladspa-set! plugin 2 0.617999970912933 )
	(ladspa-set! plugin 3 1.0 )
	(ladspa-set! plugin 4 0.0 )
	(ladspa-set! plugin 5 0.7 )
	)
      l))
  (define* (make-reverb)
    (let ((l (make-ladspa "g2reverb.so" "G2reverb")))
      (let ((plugin l))
	(ladspa-set! plugin 0 52.4360008239746 )
	(ladspa-set! plugin 1 2.45300006866455 )
	(ladspa-set! plugin 2 0.225999996066093 )
	(ladspa-set! plugin 3 0.566999971866608 )
	(ladspa-set! plugin 4 -80.0 )
	(ladspa-set! plugin 5 -33.5200004577637 )
	(ladspa-set! plugin 6 -43.8779983520508 )
	)
      l))
  
  (define (filename-without-path path)
    (let ((chars (reverse! (string->list path)))
	  (result '()))
      (while (and (not (null? chars))
		  (not (char=? (car chars) #\/)))
	     (set! result (cons (car chars) result))
	     (set! chars (cdr chars)))
      (list->string result)))

  (define presetpath (string-append (getenv "HOME") "/.san_dysth/presets/"))
  
  (system (string-append "mkdir -p " presetpath " >/dev/null 2>/dev/null"))
  (system (string-append "mkdir -p " presetpath "/backup >/dev/null 2>/dev/null"))
  (system (string-append "mkdir -p " presetpath "/tmp >/dev/null 2>/dev/null"))
  (system (string-append "mkdir -p " presetpath "/examples >/dev/null 2>/dev/null"))
  (if (defined? '*san-dysth-example-presets*)
      (system (string-append "cp -u " *san-dysth-example-presets* "/* " presetpath "/examples/ >/dev/null 2>/dev/null")))

  (primitive-eval '(define synth-load-save-parameters '(polyphony
							attack attack-peak sustain release
							das-vol pitch octave period maximum-drunk-change unrandom maximum-add-val
							a1 a2 a3 b1 b2 b3
							src-width reverb-val
							min-release)))
  (letrec* (

	    (filename (<-> presetpath "tmp/preset.scm"))
		      
	    (num-synths polyphony)
	    (num-playing 0)
	    (all-isplaying (make-vct 16))

	    (reverb-bus (make-bus 2))
	    (reverb (make-reverb))

	    (min-release 0.000001)
	    
	    (reverb-glide-vol (make-glide-var 1.0 0.001))

	    (autopan (make-var 1))
	    
	    (notes (make-vct (1+ num-synths)))
	    (volumes (make-vct (1+ num-synths)))

	    (a4 (make-oscil :frequency 440))

	    (get-attack-inc (lambda (attack-peak attack)
			      (/ (* attack-peak 256)
				 (* (mus-srate) attack))))
	    (get-sustain-dec (lambda (attack-peak sustain)
			       (/ (- attack-peak 1)
				  (* (mus-srate) sustain))))
	    (get-release-mul (lambda (release)
			       (expt min-release (/ 1 (* (mus-srate) release)))))

	    (make-synths (lambda ()
			   (let ((ins (map
			    (lambda (synth-num)
			      (let* (
				     (note-vol 0)
				     (note 0)
				     (pan_left 1)
				     (pan_right 1)

				     (sr (make-src :srate 0 :width src-width))
				     
				     ;;(attack (make-env  `(0 0    0.7 1.9    1.0 1.0) #:duration attack))

				     (attack-val 0)
				     (attack-inc (get-attack-inc attack-peak attack))
				     
				     (sustain-val attack-peak)
				     (sustain-dec (get-sustain-dec attack-peak sustain))
				     
				     (release-val 1.0)
				     (release-mul (get-release-mul release))
				     ;;(release-dec (/ 256 (* (mus-srate) release)))

				     (is-playing #f)
				     (src-val 0)			       
				     (is-attacking #t)
				     (is-sustaining #f)
				     (is-releasing #f)
				     
				     (val 0)
				     (addval 0)
				     (period period)
				     (periodcounter period)
				     (inc-addval #f)
				     (maximum-add-val maximum-add-val)
				     (maximum-drunk-change maximum-drunk-change)

				     (last-time 0)
				     (regular-periods regular)
				     
				     (volume (make-glide-var das-vol 0.01))
				     (rate (* 0.1 (expt 2 (+ octave pitch))))

				     (das-filter (make-filter 4 (vct 1 b1 b2 b3) (vct 1 (- a1) (- a2) (- a3))))
				     (das-delay (make-delay 500))
;;				     (das-osc (make-oscil))

				     (instrument 
				      (<rt> 
						 (lambda ()
						   (declare (<double> release-val release-mul)
							    (<int> last-time synth-num note is-playing is-releasing is-attacking))
						   (define (scale x x1 x2 y1 y2)
						     (+ y1
							(/ (* (- x x1)
							      (- y2 y1))
							   (- x2 x1))))
						   (define (get-pan-vals pan)
						     (let ((scale ,(- 2 (* 2 (sqrt 2))))
							   (x (scale pan -1 1 0 1)))
						       (vct (* (- 1 x)
							       (+ (* scale (- 1 x))
								  (- 1 scale)))
							    (* x (+ (* scale x)
								    (- 1 scale))))))

						   (define (synthesize)
						     (declare (<double> val addval maximum-drunk-change))
						     (declare (<int> period periodcounter))
						     (let ((synth (lambda (direction)
								    ;;(oscil das-osc))))
								    (cond ((<= val -1)
									   (set! inc-addval #t))
									  ((>= val 1)
									   (set! inc-addval #f))
									  ((> addval maximum-add-val)
									   (set! periodcounter period)
									   (set! inc-addval #f))
									  ((< addval (- maximum-add-val))
									   (set! periodcounter period)
									   (set! inc-addval #t))
									  ((not regular-periods)
									   (set! periodcounter (1- periodcounter))
									   (if (= periodcounter 0)
									       (begin
										 (set! periodcounter period)
										 (set! inc-addval (if inc-addval #f #t))))))
								    
								    (set! addval (filter das-filter (if inc-addval
													(+ unrandom (random maximum-drunk-change))
													(- (+ unrandom (random maximum-drunk-change))))))
								    (set! val (+ val addval))
								    val)))
						       (* (read-glide-var volume)
							  (src sr (* src-val rate) synth))))

						   (define (not-playing-func)
						     (if (not (= (rt-get-time) last-time)) ;; realtime objects are executed in blocks, so its only necessary to check
							 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; one time.
							 (let ((notes-note (the <int> (vct-ref notes synth-num))))
							   (set! last-time (rt-get-time))
							   (if (> notes-note 0)
							       (begin
								 (set! is-playing #t)
								 (set! note (vct-ref notes synth-num))
								 (let* ((mi 20)
									(ma 107)
									(val (max mi (min ma note)))
									(vals (get-pan-vals (scale val mi ma -1 1))))
								   (set! pan_left (* ,(sqrt 2) (vct-ref vals 0)))
								   (set! pan_right (* ,(sqrt 2) (vct-ref vals 1))))
								 (set! is-attacking #t)
								 (set! is-releasing #f)
								 (set! is-sustaining #f)
								 
								 (mus-reset das-delay)
								 ;;(mus-reset attack)
								 ;;(mus-reset release)
								 
								 (set! attack-val 0.0)
								 (set! sustain-val attack-peak)
								 (set! release-val 1.0)
								 
								 (mus-reset sr)
								 
								 (set! src-val (let ((middlenote (midi-to-freq middlenote))
										     (srcval (midi-to-freq note))) 
										 (set! srcval (- srcval middlenote))
										 (set! srcval (/ srcval middlenote))
										 (1+ srcval)))
								 (set! note-vol (vct-ref volumes synth-num))
								 (set! num-playing (1+ num-playing))
								 ;;(printf "aiai %x %x %x %f %f\\n" control data1 data2 src-val vol)
								 
								 )
							       (if (< notes-note 0) ;; Did not get time to play.
								   (vct-set! notes synth-num 0))))))

						   (define (is-playing-func)
						     (if (not is-releasing)
							 (if (not (= (rt-get-time) last-time))
							     (begin
							       (set! last-time (rt-get-time))
							       (if (< (vct-ref notes synth-num) 1)
								   (begin
								     ;;(printf "Starting to release %d/%d\\n" synth-num note)
								     (set! is-releasing #t))))))
						     
						     (let ((outval (* 0.2 note-vol (synthesize)))
							   (my-out (lambda (a b)
								     (declare (<double> a b))
								     (if (> (read-var autopan) 0)
									 (begin
									   (set! a (* a pan_left))
									   (set! b (* b pan_right))))
								     (out (vct a b)))))
						       (declare (<double> outval))
						       (cond (is-attacking
							      (set! outval (* outval (/ attack-val 256)))
							      (my-out outval
								      (delay das-delay outval))
							      (set! attack-val (+ attack-val attack-inc))
							      (if (>= attack-val (* attack-peak 256))
								  (begin
								    (set! is-attacking #f)
								    (set! is-sustaining #t))))
							     
							     (is-sustaining
							      (set! outval (* outval sustain-val))
							      (my-out outval
								      (delay das-delay outval))
							      (set! sustain-val (- sustain-val sustain-dec))
							      (if (<= sustain-val 1.0)
								  (set! is-sustaining #f)))
							     
							     (is-releasing
							      
							      (my-out (* release-val outval)
								      (* release-val (delay das-delay outval)))
							      (set! release-val (* release-val release-mul))
							      ;;(set! release-val (- release-val release-dec))
							      (if (<= release-val min-release) ;; check for (min-relase * reverb-val) as well?
								  (begin
								    (set! is-playing #f)
								    (vct-set! notes synth-num 0)
								    (set! num-playing (1- num-playing)))))
							     (else
							      (my-out outval
								      (delay das-delay outval))))))

						   (if (not is-playing)
						       (not-playing-func))
						   (if is-playing
						       (is-playing-func))))))
				
				instrument))
			    (iota (1+ num-synths)))))
			     (for-each (lambda (instr)
					 (-> instr play #:position 'last))
				       (c-butlast ins))
			     ins)))
	    
	    (make-midi-input (lambda ()
			       (<rt-play> #:position 'first
					  (lambda ()
					    (receive-midi (lambda (control data1 data2)
							    (set! control (logand #xf0 control))
							    (if (and (< num-playing num-synths)
								     (= control #x90)
								     (> data2 0))
								(let loop ((synth-num 0))
								  (declare (<int> synth-num))
								  
								  (if (= (vct-ref notes synth-num) 0)
								      (begin
									;;(printf "synth-num %d %d %f\\n" synth-num data1 (vct-ref notes synth-num))
									(vct-set! volumes synth-num (/ data2 128.0))
									(vct-set! notes synth-num data1))
								      (if (< synth-num (1- num-synths))
									  (loop (1+ synth-num)))))
									
								(if (or (= control #x80)
									(and (= control #x90)
									     (= data2 0)))
								    (let loop ((synth-num 0))
								      (if (= (vct-ref notes synth-num) data1)
									  (vct-set! notes synth-num (- data1))
									  (if (< synth-num (1- num-synths))
									      (loop (1+ synth-num)))))))))))))
	    
	    (make-reverb-instrument (lambda ()
				      (set! reverb (make-reverb))
				      (<rt-play> #:position 'last
						 (lambda ()
						   (out (vct-scale! (ladspa-run reverb (read-bus *out-bus*))
								    (* reverb-val 40 (read-glide-var reverb-glide-vol))))))))
	    
	    
	    (synths (make-synths))
	    (midi-input (make-midi-input))

	    (reverb-instrument (make-reverb-instrument))
	     
	    (440-instrument (<rt> (lambda ()
				   (out (* 0.1 (oscil a4))))))

	    (440-playing #f)
	    (a4-playing #f)
	    
	    (a4-stop (lambda ()
		       (if a4-playing
			   (begin
			     (-> (last synths) stop)
			     (vct-set! notes num-synths -57)
			     (set! a4-playing #f)))))
	    (a4-start (lambda ()
			(if (not a4-playing)
			    (begin
			      (-> (last synths) play #:position 'last)
			      (vct-set! volumes num-synths 0.9)
			      (vct-set! notes num-synths 57)
			      (set! a4-playing #t)))))
	    (440-stop (lambda ()
			(if 440-playing
			    (begin
			      (-> 440-instrument stop)
			      (set! 440-playing #f)))))
	    (440-start (lambda ()
			 (if (not 440-playing)
			     (begin
			       (-> 440-instrument play)
			       (set! 440-playing #t)))))
	    
	    (stop-all (lambda (restfunc)
			(for-each (lambda (instrument)
				    (write-glide-var (-> instrument volume) 0.0))
				  (c-butlast synths))
			(write-glide-var reverb-glide-vol 0.0)
			(in 10
			    (lambda ()
			      (for-each (lambda (instrument) (-> instrument stop)) (c-butlast synths))
			      (440-stop)
			      (a4-stop)
			      (set! notes (make-vct (1+ num-synths)))
			      (-> reverb-instrument stop)
			      (-> midi-input stop)
			      (if restfunc
				  (restfunc))))))
	    
	    (start-all (lambda (stop?)
			 (let ((doit (lambda ()
				       (set! synths  (make-synths))
				       (write-glide-var reverb-glide-vol 1.0)
				       (set! midi-input (make-midi-input))
				       (set! reverb-instrument (make-reverb-instrument)))))
			   (if stop?
			       (stop-all doit)
			       (doit)))))
	    
	    (get-filename (lambda (func)
			    (let ((dialog (GTK_FILE_CHOOSER_DIALOG (gtk_file_chooser_dialog_new
								    "aiai"
								    #f
								    GTK_FILE_CHOOSER_ACTION_OPEN
								    (list GTK_STOCK_CANCEL GTK_RESPONSE_REJECT
									  GTK_STOCK_OK GTK_RESPONSE_ACCEPT)))))
			      (gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER dialog) presetpath)
			      (if (and (= GTK_RESPONSE_ACCEPT (gtk_dialog_run (GTK_DIALOG dialog)))
				       func)
				  (func  (gtk_file_chooser_get_filename (GTK_FILE_CHOOSER dialog))))
			      (gtk_widget_hide (GTK_WIDGET dialog)))))
	    (save-preset (begin
			   (primitive-eval '(define-macro (make-varlist-macro)
					      `(list ,@(map (lambda (var)
							      `(list ',var ,var))
							    synth-load-save-parameters))))
			   (lambda (preset-filename)			   
			     (let ((fd (open-file preset-filename "w")))
			       (pretty-print `(define synth-preset-temp ',(make-varlist-macro))
					     fd)
			       (close fd)))))
	    
	    (load-preset (begin
			   (primitive-eval '(define-macro (read-varlist-macro)
					      `(begin ,@(map (lambda (var)
							       `(let ((a (assoc ',var synth-preset-temp)))
								  (if a
								      (set! ,var (cadr a)))))
							     synth-load-save-parameters))))
			   
			   (lambda (preset-filename)
			     (stop-all (lambda ()
					 (gtk_window_set_title (GTK_WINDOW (-> d dialog)) preset-filename)
					 (for-each (lambda (slider)
						     (-> slider delete!))
						   sliders)
					 (load preset-filename)
					 (set! a2 0)(set! a3 0)
					 (read-varlist-macro)
					 (set! sliders (make-sliders))
					 (start-all #f))))))

	    (save-backup (lambda (backup-filename)
			   (system (<-> "cp -f \"" backup-filename "\" \"" presetpath "/backup/"
					(filename-without-path backup-filename) "-"(string-append (string #\`)) "date -Iseconds" (string-append (string #\`)) "\""
					" >/dev/null 2>/dev/null"))))

	    (preset-A (<-> presetpath "tmp/synth-preset-A"))
	    (preset-B (<-> presetpath "tmp/synth-preset-B"))

	    (load-here (lambda ()
			 (get-filename (lambda (preset-filename)
					 (save-backup filename)
					 (load-preset preset-filename)
					 (save-preset preset-A)
					 (save-preset preset-B)
					 (gtk_window_set_title (GTK_WINDOW (-> d dialog)) preset-filename)
					 (set! filename preset-filename)))))
	    
	    (save (lambda ()
		    (save-backup filename)
		    (save-preset filename)))
	    
	    (save-as (lambda ()
		       (get-filename (lambda (preset-filename)
				       (save-backup preset-filename)
				       (save-preset preset-filename)
				       (gtk_window_set_title (GTK_WINDOW (-> d dialog)) preset-filename)
				       (set! filename preset-filename)))))
	    
	    (exit-synth (lambda ()
		    (stop-all #f)
		    (-> d hide)
		    (set! check-overruns #f)
		    (exit)
		    ))
	    	    
	    (d (<dialog> "SAN DYSTH" exit-synth
			 "A" (lambda () (save-preset preset-B) (load-preset preset-A))
			 "B" (lambda () (save-preset preset-A) (load-preset preset-B))
			 "-->A/B" (lambda () (save-preset preset-A) (save-preset preset-B))
			 "Save As" save-as
			 "Save" save
			 "Load" load-here
			 "Reverb GUI" (lambda () (make-ladspa-gui reverb))
			 "Close" exit-synth))

	    (for-all-instruments (lambda (func)
				   (lambda (val)
				     (for-each (lambda (instrument)
						 (func instrument val))
					       synths))))
	    (<checkbutton2> (lambda args
			      (let ((ret (apply <checkbutton> args)))
				(-> ret add-method 'delete! (<- ret remove))
				ret)))

	    (make-sliders (lambda ()
;			    (map (lambda (args)
;				   (apply (lambda (name minval var maxval resolution func)
;					    (<slider> d name 0 var 2.0 (for-all-instruments func) resolution))
;				 `(("Amplitude" 0 ,das-vol 2.0 1000 ,(lambda (instrument val)
;								      (write-glide-var (-> instrument volume) (* (if (> unrandom 0) 0.01 1) das-vol))))
;				   ("Pitch" ...)))

			    (list
			     (<slider> d "Amplitude" 0 das-vol 2.0 (for-all-instruments (lambda (instrument val)
											  (set! das-vol val)
											  (write-glide-var (-> instrument volume) (* (if (> unrandom 0) 0.01 1) das-vol))))
				       1000)
			     (<slider> d "Pitch" 0 pitch 1 (for-all-instruments (lambda (instrument val)
										  (set! pitch val)
										  (set! (-> instrument rate) (* 0.1 (expt 2 (+ octave pitch))))))
				       1000)
			     (<slider> d "Octave" 0 octave 8 (for-all-instruments (lambda (instrument val)
										    (set! octave val)
										    (set! (-> instrument rate) (* 0.1 (expt 2 (+ octave pitch))))))
				       1)
			     (<slider> d "src-width" 2 src-width 40 (lambda (val)
								      (set! src-width val)
								      (start-all #t))
				       1)
			     (<slider> d "Period" 2 period 2000 (for-all-instruments (lambda (instrument val)
										       (set! period val)
										       (set! (-> instrument period) val)))
				       1)
			     (<slider> d "Max change" 0.1 (* 10000 maximum-drunk-change) 40 (for-all-instruments (lambda (instrument val)
														   (set! maximum-drunk-change (/ val 10000))
														   (set! (-> instrument maximum-drunk-change) (sin (/ val 10000)))))
				       1000)
			     
			     ;; Unrandom can be very cool, but also very dangerous for ears and loudspeakers to play with.
			     ;;    (<slider> d "Unrandom" 0.0 0 2.0 (for-all-instruments (lambda (instrument val)
			     ;;							    (if (or (and (> val 0)
			     ;;									 (= 0 (-> instrument unrandom)))
			     ;;								    (and (= val 0)
			     ;;									 (> (-> instrument unrandom) 0)))
			     ;;								(begin
			     ;;								  (write-glide-var (-> instrument volume) (* (if (> val 0) 0.01 1) das-vol))
			     ;;								  (set! (-> instrument rate) (* (if (> val 990) 0.001 0.1) (expt 2 (+ octave pitch))))))
			     ;;							    (set! unrandom val)
			     ;;							    (set! (-> instrument unrandom) val)))
			     ;;	      1000)
			     (<slider> d "Max add" 0.1 (* 100 maximum-add-val) 10 (for-all-instruments (lambda (instrument val)
													 (set! maximum-add-val (/ val 100))
													 (set! (-> instrument maximum-add-val) (/ val 100))))
				       1000)
			     (<slider> d "-a1" -1 a1 1.0 (lambda (val)
							   (if (> (+ val a2 a3) 1)
							       (begin
								 (set! val (- 1 a2 a3))
								  (-> (list-ref sliders 7) set! val)))
							   (set! a1 val)
							   ;;(c-display (+ (abs a1) (abs a2) (abs a3)))
							   (for-each (lambda (instrument)
									(vct-set! (mus-ycoeffs (-> instrument das-filter)) 1 (- val)))
								     synths))
				       1000)
			     (<slider> d "-a2" -1 a2 1.0 (lambda (val)
							   (if (> (+ val a1 a3) 1)
							       (begin
								 (set! val (- 1 a1 a3))
								 (-> (list-ref sliders 8) set! val)))
							   (set! a2 val)
							   ;;(c-display (+ (abs a1) (abs a2) (abs a3)))
							   (for-each (lambda (instrument)
								       (vct-set! (mus-ycoeffs (-> instrument das-filter)) 2 (- val)))
								     synths))
				       1000)
			     (<slider> d "-a3" -1 a3 1.0 (lambda (val)
							   (if (> (+ val a1 a2) 1)
							       (begin
								  (set! val (- 1 a1 a2))
								  (-> (list-ref sliders 9) set! val)))
							   (set! a3 val)
							   ;;(c-display (+ (abs a1) (abs a2) (abs a3)))
							   (for-each (lambda (instrument)
								       (vct-set! (mus-ycoeffs (-> instrument das-filter)) 3 (- val)))
								     synths))
				       1000)
			     (<slider> d "b1" -1 b1 1.0 (for-all-instruments (lambda (instrument val)
									       (set! b1 val)
									       (vct-set! (mus-xcoeffs (-> instrument das-filter)) 1 val)))
				       1000)
			     (<slider> d "b2" -1 b2 1.0 (for-all-instruments (lambda (instrument val)
									       (set! b2 val)
									       (vct-set! (mus-xcoeffs (-> instrument das-filter)) 2 val)))
				       1000)
			     (<slider> d "b3" -1 b3 1.0 (for-all-instruments (lambda (instrument val)
									       (set! b3 val)
									       (vct-set! (mus-xcoeffs (-> instrument das-filter)) 3 val)))
				       1000)
			     (<slider> d "attack" 0.005 attack 2.0 (for-all-instruments (lambda (instrument val)
											  (set! attack val)
											  (set! (-> instrument attack-inc)
												(get-attack-inc (-> instrument attack-peak) attack))))
				       1000)
			     (<slider> d "attack-peak" 1.00 attack-peak 10.0 (for-all-instruments (lambda (instrument val)
												    (set! attack-peak val)
												    (set! (-> instrument attack-peak) val)
												    (set! (-> instrument attack-inc) (get-attack-inc val attack))
												    (set! (-> instrument sustain-dec) (get-sustain-dec val sustain))))
				       1000)
			     (<slider> d "sustain" 0.005 sustain 2.0 (for-all-instruments (lambda (instrument val)
											    (set! sustain val)
											    (set! (-> instrument sustain-dec) (get-sustain-dec (-> instrument attack-peak)
																	       val))))
				       1000)
			     (<slider> d "release" 0.005 release 2.0 (for-all-instruments (lambda (instrument val)
											    (set! release val)
											    (set! (-> instrument release-mul) (get-release-mul val))))
				       1000)
			     (<checkbutton2> d "On/Off" (lambda (val) (if val 
									  (start-all #t)
									  (stop-all #f)))
					     #t)
			     (<checkbutton2> d "Autopan" (lambda (val) (if val 
									   (write-var autopan 1)
									   (write-var autopan 0)))
					     #t)
			     (<checkbutton2> d "Regular" (for-all-instruments (lambda (instrument val)
									       (set! regular val)
									       (set! (-> instrument regular-periods) (if val 1 0))))
					    regular)
			     (<checkbutton2> d "Play A4" (lambda (val) ((if val a4-start a4-stop))))
			     (<checkbutton2> d "Play 440Hz" (lambda (val) ((if val 440-start 440-stop))))
			     (<slider> d "reverb" 0 reverb-val 2.0 (for-all-instruments (lambda (instrument val)
											  (set! reverb-val val)
											  (set! (-> reverb-instrument reverb-val) val)))
				       ;;(set! (-> instrument reverb-val) val)))
				       1000))))
	    (sliders (make-sliders)))
    
    (save-preset preset-A)
    (save-preset preset-B)
    
    (-> d show)
    
    (letrec ((check-overrun (let ((lastval (last (rte-info))))
			      (lambda ()
				(let ((newval (last (rte-info))))
				  (if (> newval lastval)
				      (begin
					(c-display "Using too much CPU. Skipping. (" newval "skips since start (try lowering period+pitch/octave, src-width and/or polyphony))")
					(set! lastval newval))))
				(if check-overruns
				    (in 1000 check-overrun))))))
      (check-overrun))
    
    
    midi-input))


(san-dysth)




