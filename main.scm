;; Charles Jackson
;; HealthCheckin - A way to track down symptom triggers

(define seconds-in-hour 3600.0)
(define seconds-in-day (* seconds-in-hour 24))
(define background-color (color-rgb 30 30 30))
(define topbar-height 60)
(define button-height 40)
(define margin 20)
(define padding 7)

(define frequency-selector (make-table))
(define frequency-options
  (vector (map (lambda (str) (dropdown-callback str))
               (map number->string (list-nums 0 24)))
          (map (lambda (str) (dropdown-callback str))
               (map number->string (list-nums 0 365)))))
(define unit-selector (make-table))
(define time-selectors (list))
(define start-time-selector (make-table))
(define end-time-selector (make-table))
(define time-day-selector (make-table))
(define ratings (list))

(define (list-nums first last)
  ;; first and last are both inclusive
  (let loop ((nums (list)) (num last))
    (if (< num first)
        nums
        (loop (cons num nums) (- num 1)))))

(define (glgui-clear-all)
  (map (lambda (wgt)
         (glgui-widget-delete gui wgt))
       (table-ref gui 'widget-list (list)))
  (glgui-box gui 0 0 (glgui-width-get) (glgui-height-get)
    background-color))

(define (glgui-show-index lst index)
  (map (lambda (wgt i)
         (glgui-widget-set! gui wgt 'hidden (not (= i index))))
       lst (list-nums 0 (- (length lst) 1))))

(define (glgui-title title)
  (let ((x (inexact->exact (floor (/ (glgui-width-get) 4.5)))))
    (glgui-widget-set! gui
        (glgui-label gui x (- (glgui-height-get) topbar-height -10) (* x 2.5) button-height
          title dejavubold_46.fnt White)
        'align GUI_ALIGNCENTER)))

(define (dropdown-callback str)
  (lambda (lg lw x y w h s)
    (if s (glgui:draw-box x y w h Grey))
    (glgui:draw-text-left (+ x 5) y (- w 10) h str dejavu_25.fnt Black)))

(define (checkin-page)
  (glgui-clear-all)
  ;; topbar
  (glgui-menubar gui 0 (- (glgui-height-get) topbar-height) (glgui-width-get) topbar-height)
  (glgui-title "Check-In")
  (glgui-button-arrow-string gui 10 (- (glgui-height-get) topbar-height -12)
                             100 button-height
    #t "Settings" dejavu_18.fnt (lambda (g w t x y) (settings-page)))
  (glgui-button-arrow-string gui (- (glgui-width-get) 110) (- (glgui-height-get) topbar-height -12)
                             100 button-height
    #f "Export" dejavu_18.fnt (lambda (g w t x y) (export-page)))
  ;;; GAD
  ;; messages
  (let ((height (- (glgui-height-get) topbar-height margin 20)))
    (map (lambda (label)
           (glgui-widget-set! gui
               (glgui-label gui 0 height (glgui-width-get) 20
                 label dejavu_25.fnt White)
               'align GUI_ALIGNCENTER)
           (set! height (- height 90)))
         (list "Worry" "Anxious" "Restless" "Racing thoughts" "Irritable"
               "Not able to relax" "Afraid of an awful event")))
  ;; sliders
  (let ((height (- (glgui-height-get) topbar-height (* margin 2) 50))
        (slider-color (color-rgb #x1b #xb1 #x93)))
    (set! ratings
          (map (lambda (_)
                 (let ((slider (glgui-slider gui margin height (- (glgui-width-get) (* margin 2)) 40
                                 0 3 #f White slider-color Grey #f dejavu_18.fnt dejavu_18.fnt #f White)))
                   (glgui-widget-set! gui slider 'showlabels #f)
                   (glgui-widget-set! gui slider 'colorbarright White)
                   (set! height (- height 90))
                   slider))
               (list-nums 0 6))))
  ;; finish button
  (glgui-button-string gui 50 20 (- (glgui-width-get) 100) button-height
    "Finish Check-In" dejavu_25.fnt (lambda (g w t x y)
                                      (save-checkin)
                                      (pop-up "Check-in Complete." checkin-page))))

(define (save-checkin)
  (sqlite-query db
    (string-append
     "INSERT INTO checkin VALUES ("
     (number->string ##now)
     (foldr (lambda (value sum)
              (string-append sum "," (number->string value)))
            "" (reverse (map (lambda (slider)
                               (table-ref slider 'value 0))
                             ratings))) ")" ))
  (set-n-notifications))

(define (pop-up message action)
  (let* ((x (/ (glgui-width-get) 7))
         (y (* (glgui-height-get) 2/5))
         (w (* x 5))
         (h (* y 2/3))
         (shadow 5)
         (container (glgui-container gui 0 0 (glgui-width-get) (- (glgui-height-get) topbar-height))))
    (glgui-box container 0 0 (glgui-width-get) (- (glgui-height-get) topbar-height)
      (color-rgba 0 0 0 0)) ;; block user from clicking other elements
    (glgui-widget-set! container (glgui-box container (+ x shadow) (- y shadow) w h
                                   (color-shade DarkGrey .4)) 'rounded #t)
    (glgui-widget-set! container (glgui-box container x y w h DarkGrey) 'rounded #t)

    (glgui-button-string container (+ x (* margin 2)) (+ y margin) (- w (* margin 4)) button-height
      "OK" dejavu_25.fnt (lambda (g w t x y) (action)))
    (table->list (glgui-label-wrapped container (+ x margin) (+ y margin button-height) (- w (* 2 margin)) 140
                   message dejavu_25.fnt Black))))

(define (export-page)
  (glgui-clear-all)
  ;; topbar
  (glgui-menubar gui 0 (- (glgui-height-get) topbar-height) (glgui-width-get) topbar-height)
  (glgui-title "Export")
  (glgui-button-arrow-string gui 10 (- (glgui-height-get) topbar-height -12)
                             100 button-height
    #t "Check-In" dejavu_18.fnt (lambda (g w t x y) (checkin-page)))
  (glgui-button-arrow-string gui (- (glgui-width-get) 110) (- (glgui-height-get) topbar-height -12)
                             100 button-height
    #f "Settings" dejavu_18.fnt (lambda (g w t x y) (settings-page)))
  ;; export button
  (glgui-button-string gui (/ (- (glgui-width-get) 300) 2) (/ (glgui-height-get) 2) 300 button-height
    "To Spreadsheet" dejavu_25.fnt
    (lambda (g w t x y)
      (pop-up (string-append "Your spreadsheet is in " (export-csv) ".")
              export-page))))

(define (export-csv)
  (let ((csv (string-append (system-directory) (system-pathseparator) "checkin.csv")))
    (csv-write csv
      (cons (list "Year" "Month" "Day" "Time" "DOW" "Worry" "Anxious"
                  "Restless" "Racing" "Irritable" "NRelax" "Afraid" "Sum")
            (map (lambda (row)
                   (append (string-split
                            (seconds->string (car row)
                                             "~Y ~m ~d ~H:~M ~a") #\ )
                           (cdr row)
                           (list (apply + (cdr row)))))
                 (sqlite-query db "SELECT * FROM checkin
                                   ORDER BY `time`"))))
    csv))

(define (settings-page)
  (glgui-clear-all)
  ;; topbar
  (glgui-menubar gui 0 (- (glgui-height-get) topbar-height) (glgui-width-get) topbar-height)
  (glgui-title "Settings")
  (glgui-button-arrow-string gui 10 (- (glgui-height-get) topbar-height -12)
                             100 button-height
    #t "Export" dejavu_18.fnt (lambda (g w t x y) (export-page)))
  (glgui-button-arrow-string gui (- (glgui-width-get) 110) (- (glgui-height-get) topbar-height -12)
                             100 button-height
    #f "Check-In" dejavu_18.fnt (lambda (g w t x y) (checkin-page)))
  ;; message
  (glgui-label gui margin (- (glgui-height-get) topbar-height button-height padding)
               (- (glgui-width-get) (* margin 2)) button-height
    "Reminder to check-in every " dejavu_25.fnt White)
  ;; frequency selection
  (set! frequency-selector
        (glgui-dropdownbox gui margin (- (glgui-height-get) topbar-height (* button-height 2) padding)
                           (* button-height 2.5) button-height
          (map (lambda (str) (dropdown-callback str))
               (map number->string (list-nums 0 24)))
          Black White Black))
  (let ((option (vector-ref frequency-options (settings-ref 'unit 0)))
        (frequency (settings-ref 'frequency 0)))
    (table-set! frequency-selector 'current
                (if (< frequency (length option))
                    frequency 0))
    (table-set! frequency-selector 'list option))
  ;; unit selection
  (set! unit-selector
        (glgui-dropdownbox gui (+ margin padding 100) (- (glgui-height-get) topbar-height (* button-height 2) padding)
                           130 button-height
          (map (lambda (str) (dropdown-callback str))
               (list "Hours" "Days"))
          Black White Black))
  (table-set! unit-selector 'callback
              (lambda (g w t x y)
                (let ((selected (table-ref unit-selector 'current)))
                  (glgui-show-index time-selectors selected)
                  (glgui-widget-set! gui frequency-selector 'list
                                     (vector-ref frequency-options selected)))))
  (table-set! unit-selector 'current (settings-ref 'unit 0))
  ;; time selection
  (set! time-selectors
        (let* ((w (- (glgui-width-get) (* margin 2))) (h 580)
               (x margin) (y (- (glgui-height-get) topbar-height 80 h padding))
               (time-color (color-shade Grey .7)))
          (list
           ;; 0: start-time, end-time
           (let ((container (glgui-container gui x y w h))
                 (start-time (settings-ref 'start-time #f))
                 (end-time (settings-ref 'end-time #f)))
             (glgui-label container 0 (- h 60) 110 20
               "Between" dejavu_25.fnt White)
             (set! start-time-selector
                   (glgui-timepicker container (+ 110 padding) (- h 100)
                                     120 (* button-height 2.5)
                     time-color (color-shade Grey 0.5) White #f dejavu_25.fnt))
             (glgui-label container (+ 110 120 (* padding 2)) (- h (* button-height 1.5))
                          50 20
               "and" dejavu_25.fnt White)
             (set! end-time-selector
                   (glgui-timepicker container (+ 110 120 50 (* padding 2)) (- h 100)
                                     120 (* button-height 2.5)
                     time-color (color-shade Grey 0.5) White #f dejavu_25.fnt))
             (map (lambda (wgt)
                    (glgui-widget-set! container wgt 'ampm #t)
                    (glgui-widget-set! container wgt 'button-normal-color time-color))
                  (list start-time-selector end-time-selector))
             (if start-time
                 (glgui-widget-set! container start-time-selector 'value start-time))
             (if end-time
                 (glgui-widget-set! container end-time-selector 'value end-time))
             container)
           ;; 1: time
           (let ((container (glgui-container gui x y w h))
                 (time-day (settings-ref 'time-day #f)))
             (glgui-label container 0 (- h 60) 130 20
               "Remind at" dejavu_25.fnt White)
             (set! time-day-selector
                   (glgui-timepicker container (+ 130 padding) (- h 100)
                                     120 (* button-height 2.5)
                     time-color (color-shade Grey 0.5) White #f dejavu_25.fnt))
             (glgui-widget-set! container time-day-selector 'ampm #t)
             (glgui-widget-set! container time-day-selector 'button-normal-color time-color)
             (if time-day (glgui-widget-set! container time-day-selector 'value time-day))
             container))))
  (glgui-show-index time-selectors (table-ref unit-selector 'current))
  (glgui-button-string gui 50 20 (- (glgui-width-get) 100) button-height
    "Save Settings" dejavu_25.fnt (lambda (g w t x y)
                                    (save-settings)
                                    (checkin-page))))

(define (save-settings)
  (let ((frequency (table-ref frequency-selector 'current))
        (unit (table-ref unit-selector 'current)))
    (settings-set! 'settings-set #t)
    (settings-set! 'frequency frequency)
    (settings-set! 'unit unit)
    (case unit
      ((0) (begin ;; hours
             (settings-set! 'start-time (table-ref start-time-selector 'value))
             (settings-set! 'end-time (table-ref end-time-selector 'value))))
      ((1) (begin ;; days
             (settings-set! 'time-day (table-ref time-day-selector 'value))))))
  (set-n-notifications))

(define (set-n-notifications #!optional (n 5))
  (localnotification-cancelall)
  (if (and (settings-ref 'settings-set #f) (> (settings-ref 'frequency) 0))
      (let* ((message "🙂 How are you feeling?")
             (now ##now)
             (today (current-date))
             (midnight (exact->inexact
                        (srfi19:time-second
                         (date->time-utc
                          (make-date 0 0 0 0 (date-day today)
                                     (date-month today)
                                     (date-year today)
                                     (date-zone-offset today)))))))
        (case (settings-ref 'unit)
          ;; hours
          ((0) (let ((space (fl* (exact->inexact (settings-ref 'frequency)) seconds-in-hour))
                     (start-time (fl+ (settings-ref 'start-time) midnight))
                     (end-time (fl+ (settings-ref 'end-time) midnight)))
                 (let loop ((n n) (start-time start-time)
                            (time (fl+ (if (< now start-time) start-time now) seconds-in-hour))
                            (end-time (if (> start-time end-time)
                                          (fl+ end-time seconds-in-day)
                                          end-time)))
                   (cond ((= n 0) #!void)
                         ((>= time end-time) ;; move to the next day
                          (loop n (fl+ start-time seconds-in-day) (fl+ start-time seconds-in-day)
                                (fl+ end-time seconds-in-day)))
                         (else ;; set the notification
                          (localnotification-schedule message time)
                          ;; move on to the next notification
                          (loop (- n 1) start-time (fl+ time space) end-time))))))
          ;; days
          ((1) (let ((space (* (exact->inexact (settings-ref 'frequency)) seconds-in-day)))
                 (let loop ((n n ) (time (fl+ (settings-ref 'time-day) midnight)))
                   (cond ((= n 0) #!void)
                         ((< time now) (loop n (fl+ time space)))
                         (else
                          (print time) (newline)
                          (localnotification-schedule message time)
                          (loop (- n 1) (fl+ time space)))))))))))

(define gui #f)
(define db #f)

(main
 ;; initialization
 (lambda (w h)
   (make-window 450 800)
   (glgui-orientation-set! GUI_PORTRAIT)
   (set! gui (make-glgui))
   (let ((sqlfile (string-append (system-directory) (system-pathseparator) "checkin.db")))
     (set! db (sqlite-open sqlfile))
     ;; create checkin-table
     (sqlite-query db "CREATE TABLE IF NOT EXISTS checkin (
                       `time` TIME,
                       worry FLOAT, anxious FLOAT, restless FLOAT, racing FLOAT,
                       irritable FLOAT, notRelax FLOAT, afraid FLOAT)"))
   ;; make sure a config directory exists
   (let ((configdirectory (string-append (system-directory) (system-pathseparator) "config")))
     (if (not (file-exists? configdirectory))
         (create-directory configdirectory)))
   (settings-init (list))
   (if (settings-ref 'settings-set)
       (checkin-page)
       (settings-page)))
 ;; events
 (lambda (t x y)
   (if (and (= t EVENT_KEYPRESS)
            (= x EVENT_KEYESCAPE))
       (terminate))
   (glgui-event gui t x y))
 ;; termination
 (lambda ()
   (if db (sqlite-close db))
   #t)
 ;; suspend
 (lambda () (glgui-suspend))
 ;; resume
 (lambda () (glgui-resume)))

;; eof
