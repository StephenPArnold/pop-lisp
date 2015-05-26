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
