(defun test-before-p ()
	(let ((plan '()) (start '()) (goal '()) (a '()) (b '()))
		(setf start 
			(make-operator
				:name 'start ;;named will be used n assoc lists
				:uniq (gensym) ;;differentiate two operators of same name i think
				:preconditions nil
				:effects '((t i-am-crazy) (nil i-am-frodo-baggins) (nil i-have-a-phd) (nil this-assignment-is-done) (t i-like-video-games))))
		(setf goal 
			(make-operator
				:name 'goal
				:uniq (gensym)
				:preconditions '((nil i-am-crazy) (t i-am-frodo-baggins) (t i-have-a-phd) (t this-assignment-is-done) (t i-like-video-games))
				:effects nil))
		(setf a
			(make-operator 
				:name 'a
				:uniq (gensym)
				:preconditions '((nil i-have-a-phd) (nil i-am-frodo-baggins))
				:effects '((t i-am-frodo-baggins) (nil i-am-crazy))))
		(setf b 
			(make-operator 
				:name 'b
				:uniq (gensym)
				:preconditions '((t i-am-frodo-baggins))
				:effects '((t i-have-a-phd))))
		(setf plan (make-plan
			:operators (list start goal a b)
			:orderings (list (cons 'start 'goal) (cons 'start 'a) (cons 'a 'b) (cons 'b 'goal))
			:links nil
			:start start
			:goal goal))
		(print "should be true")
		(print (before-p a b plan))
		(print "should be false")
		(print (before-p b a plan))
			
		
	)
)

(defun test-inconsistent-p ()
	;;CONSISTENT PLAN
	(let ((plan '()) (start '()) (goal '()) (a '()) (b '()))
		(setf start 
			(make-operator
				:name 'start ;;named will be used n assoc lists
				:uniq (gensym) ;;differentiate two operators of same name i think
				:preconditions nil
				:effects '((t i-am-crazy) (nil i-am-frodo-baggins) (nil i-have-a-phd) (nil this-assignment-is-done) (t i-like-video-games))))
		(setf goal 
			(make-operator
				:name 'goal
				:uniq (gensym)
				:preconditions '((nil i-am-crazy) (t i-am-frodo-baggins) (t i-have-a-phd) (t this-assignment-is-done) (t i-like-video-games))
				:effects nil))
		(setf a
			(make-operator 
				:name 'a
				:uniq (gensym)
				:preconditions '((nil i-have-a-phd) (nil i-am-frodo-baggins))
				:effects '((t i-am-frodo-baggins) (nil i-am-crazy))))
		(setf b2 
			(make-operator 
				:name 'b
				:uniq (gensym)
				:preconditions '((t i-am-frodo-baggins))
				:effects '((t i-have-a-phd))))
		(setf b 
			(make-operator 
				:name 'b
				:uniq (gensym)
				:preconditions '((t i-am-frodo-baggins))
				:effects '((t i-have-a-phd))))
		(setf plan (make-plan
			:operators (list start goal a b)
			:orderings (list (cons 'start 'goal) (cons 'start 'a) (cons 'b2 'a) (cons 'a 'b) (cons 'b 'goal))
			:links nil
			:start start
			:goal goal))
		(print "should be false, it is not inconsistent")
		(print (inconsistent-p plan))
		
	)
	(let ((plan '()) (start '()) (goal '()) (a '()) (b '()))
		(setf start 
			(make-operator
				:name 'start ;;named will be used n assoc lists
				:uniq (gensym) ;;differentiate two operators of same name i think
				:preconditions nil
				:effects '((t i-am-crazy) (nil i-am-frodo-baggins) (nil i-have-a-phd) (nil this-assignment-is-done) (t i-like-video-games))))
		(setf goal 
			(make-operator
				:name 'goal
				:uniq (gensym)
				:preconditions '((nil i-am-crazy) (t i-am-frodo-baggins) (t i-have-a-phd) (t this-assignment-is-done) (t i-like-video-games))
				:effects nil))
		(setf a
			(make-operator 
				:name 'a
				:uniq (gensym)
				:preconditions '((nil i-have-a-phd) (nil i-am-frodo-baggins))
				:effects '((t i-am-frodo-baggins) (nil i-am-crazy))))
		(setf b 
			(make-operator 
				:name 'b
				:uniq (gensym)
				:preconditions '((t i-am-frodo-baggins))
				:effects '((t i-have-a-phd))))
		(setf plan (make-plan
			:operators (list start goal a b)
			:orderings (list (cons 'start 'goal) (cons 'start 'a) (cons 'b 'a) (cons 'a 'b) (cons 'b 'goal))
			:links nil
			:start start
			:goal goal))
		(print "should be true, it is inconsistent")
		(print (inconsistent-p plan))
		
	))

(defun test-all-operators ()
	
	(build-operators-for-precond)
	(loop for some-op in *operators* do
  		(loop for some-effect in (operator-effects some-op) do
			(progn (print "these are all the opeerators that do") (print some-effect) 
			       (print (all-operators some-effect))))))

(defun test-all-operators-time-test ()
	
	(build-operators-for-precond)
        (dotimes (x 1000000)
	(if (= (random 2) 0)
	(loop for some-op in *operators* do
  		(loop for some-effect in (operator-effects some-op) do
			(progn 
			       (all-operators some-effect))))
	(loop for some-op in *operators* do
  		(loop for some-effect in (operator-effects some-op) do
			(progn 
			       (all-operators2 some-effect))))))

)

(defun test-all-effects ()
	(print "start effects are")
	(print   *start-effects*)
  (let* ((start (make-operator
		 :name 'start
		 :uniq (gensym)
		 :preconditions nil
		 :effects *start-effects*))
	 (goal (make-operator
		:name 'goal
		:uniq (gensym)
		:preconditions *goal-preconditions*
		:effects nil))
	 (plan (make-plan
		:operators (list start goal)
		:orderings (list (cons start goal))
		:links nil
		:start start
		:goal goal)))
		(loop for some-op in *operators* do
  		(loop for some-effect in (operator-effects some-op) do
			(progn 
			       (print "here's all the things that cause") (print some-effect) (print (all-effects some-effect plan)))))
	)
)
(defun operator-threatens-link-p-test ()
        (let ((plan '()) (start '()) (goal '()) (a '()) (b '()))
                (setf start
                        (make-operator
                                :name 'start ;;named will be used n assoc lists
                                :uniq (gensym) ;;differentiate two operators of same name i think
                                :preconditions nil
                                :effects '((t i-am-crazy) (nil i-am-frodo-baggins) (nil i-have-a-phd) (nil this-assignment-is-done) (t i-like-video-games))))
                (setf goal
                        (make-operator
                                :name 'goal
                                :uniq (gensym)
                                :preconditions '((nil i-am-crazy) (t i-am-frodo-baggins) (t i-have-a-phd) (t this-assignment-is-done) (t i-like-video-games))
                                :effects nil))
                (setf a
                        (make-operator
                                :name 'a
                                :uniq (gensym)
                                :preconditions '((nil i-have-a-phd) (nil i-am-frodo-baggins))
                                :effects '((t i-am-frodo-baggins) (nil i-am-crazy))))
                (setf b
                        (make-operator
                                :name 'b
                                :uniq (gensym)
                                :preconditions '((t i-am-frodo-baggins))
                                :effects '((t i-have-a-phd))))
                (setf plan (make-plan
                        :operators (list start goal a b)
                        :orderings (list (cons 'start 'goal) (cons 'start 'a) (cons 'a 'b) (cons 'b 'goal))
                        :links nil
                        :start start
                        :goal goal))

                        (print "should be nil in p-test:")
                        (print (operator-threatens-link-p start (make-link :from b :to goal :precond '(t i-have-a-phd)) plan))
        )

        (let ((plan '()) (start '()) (goal '()) (a '()) (b '()))
                (setf start
                        (make-operator
                                :name 'start ;;named will be used n assoc lists
                                :uniq (gensym) ;;differentiate two operators of same name i think
                                :preconditions nil
                                :effects '((t i-am-crazy) (nil i-am-frodo-baggins) (nil i-have-a-phd) (nil this-assignment-is-done) (t i-like-video-games))))
                (setf goal
                        (make-operator
                                :name 'goal
                                :uniq (gensym)
                                :preconditions '((nil i-am-crazy) (t i-am-frodo-baggins) (t i-have-a-phd) (t this-assignment-is-done) (t i-like-video-games))
                                :effects nil))
                (setf a
                        (make-operator
                                :name 'a
                                :uniq (gensym)
                                :preconditions '((nil i-have-a-phd) (nil i-am-frodo-baggins))
                                :effects '((t i-am-frodo-baggins) (nil i-am-crazy))))
                (setf b
                        (make-operator
                                :name 'b
                                :uniq (gensym)
                                :preconditions '((t i-am-frodo-baggins))
                                :effects '((t i-have-a-phd))))
                (setf plan (make-plan
                        :operators (list start goal a b)
                        :orderings (list (cons 'a 'b) (cons 'b 'goal))
                        :links nil
                        :start start
                        :goal goal))

                        (print "should be t in p-test:")
                        (print (operator-threatens-link-p start (make-link :from b :to goal :precond '(t i-have-a-phd)) plan))
        )

             
)

(defun threats-test ()
        (let ((plan '()) (start '()) (goal '()) (a '()) (b '()))
                (setf start
                        (make-operator
                                :name 'start ;;named will be used n assoc lists
                                :uniq (gensym) ;;differentiate two operators of same name i think
                                :preconditions nil
                                :effects '((t i-am-crazy) (nil i-am-frodo-baggins) (nil i-have-a-phd) (nil this-assignment-is-done) (t i-like-video-games))))
                (setf goal
                        (make-operator
                                :name 'goal
                                :uniq (gensym)
                                :preconditions '((nil i-am-crazy) (t i-am-frodo-baggins) (t i-have-a-phd) (t this-assignment-is-done) (t i-like-video-games))
                                :effects nil))
                (setf a
                        (make-operator
                                :name 'a
                                :uniq (gensym)
                                :preconditions '((nil i-have-a-phd) (nil i-am-frodo-baggins))
                                :effects '((t i-am-frodo-baggins) (nil i-am-crazy))))
                (setf b
                        (make-operator
                                :name 'b
                                :uniq (gensym)
                                :preconditions '((t i-am-frodo-baggins))
                                :effects '((t i-have-a-phd))))
                (setf plan (make-plan
                        :operators (list start goal a b)
                        :orderings (list (cons 'start 'goal) (cons 'start 'a) (cons 'a 'b) (cons 'b 'goal))
                        :links nil
                        :start start
                        :goal goal))

                        (print "should be nil in p-test:")
                        (print (threats plan start (make-link :from b :to goal :precond '(t i-have-a-phd))))
        )

        (let ((plan '()) (start '()) (goal '()) (a '()) (b '()))
                (setf start
                        (make-operator
                                :name 'start ;;named will be used n assoc lists
                                :uniq (gensym) ;;differentiate two operators of same name i think
                                :preconditions nil
                                :effects '((t i-am-crazy) (nil i-am-frodo-baggins) (nil i-have-a-phd) (nil this-assignment-is-done) (t i-like-video-games))))
                (setf goal
                        (make-operator
                                :name 'goal
                                :uniq (gensym)
                                :preconditions '((nil i-am-crazy) (t i-am-frodo-baggins) (t i-have-a-phd) (t this-assignment-is-done) (t i-like-video-games))
                                :effects nil))
                (setf a
                        (make-operator
                                :name 'a
                                :uniq (gensym)
                                :preconditions '((nil i-have-a-phd) (nil i-am-frodo-baggins))
                                :effects '((t i-am-frodo-baggins) (nil i-am-crazy))))
                (setf b
                        (make-operator
                                :name 'b
                                :uniq (gensym)
                                :preconditions '((t i-am-frodo-baggins))
                                :effects '((t i-have-a-phd))))
                (setf plan (make-plan
                        :operators (list start goal a b)
                        :orderings (list (cons 'a 'b) (cons 'b 'goal))
                        :links nil
                        :start start
                        :goal goal))

                        (print "should be t in p-test:")
                        (print (threats plan start (make-link :from b :to goal :precond '(t i-have-a-phd))))
        )

             
)
