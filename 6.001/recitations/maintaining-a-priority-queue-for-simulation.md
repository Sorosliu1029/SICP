Having introduced mutation of data structures, we looked in recitation at how to implement a useful data structure to keeping a set of elements ordered by some “priority” rank. This is used, for example, in building (discrete event) simulators, where one needs to keep track of tasks that must be done in the future, and the times at which they need to be started. Here is the priority queue representation and implementation we developed:

(define make-task cons)
(define task-time car)
(define task-proc cdr)

(define (make-task-list)
  ;; [] -> task-list
  (list ’task-list)
  )

(define (add-task! tl task)
  ;; [task-list, task] -> ???
  (define (helper trail l)
    (cond ((null? l) (set-cdr! trail (list task)))
   ((< (task-time task) (task-time (car l)))
    (set-cdr! trail (cons task l)))
   (else (helper l (cdr l)))))
  (helper tl (cdr tl))
  tl
  )

(define (task-list? obj)
  (and (pair? obj)
       (eq? (car obj) ’task-list)))

(define (empty-task-list? tl)
  ;; task-list -> boolean
  (and (task-list? tl)
       (null? (cdr tl)))
  )

(define (pop-task! tl)
  ;; task-list -> task
  (and (task-list? tl)
       (not (empty-task-list? tl))
       (let ((ans (cadr tl)))
  (set-cdr! tl (cddr tl))
  ans)))

A typical task is represented as a procedure of no arguments. The idea is that it will be called when it is time to perform the task. In a simulator, usually tasks will, as part of their execution, add other tasks to the task-list, to be done at later times.

(define (say msg)
  (lambda ()
    (newline)
    (display msg)))

With the above, we can implement a simple simulation system that will “ping” every five ticks of the clock and “pong” every seven ticks. Note that our simulator, run, does not actually have a ticking clock, but merely executes the next available task from the task list. The clock is, therefore, kept implicitly by the (future) times for which each task queues up other tasks.

(define (run tl)
  (cond ((empty-task-list? tl)
  ‘done)
 (else
  (let ((task (pop-task! tl)))
    ((task-proc task)))
  (run tl))))

(define the-task-list (make-task-list))

(define (ping initial-time msg)
  (add-task!
   the-task-list
   (make-task initial-time
       (lambda ()
  (newline)
  (display msg)
  (ping (+ 5 initial-time)
        (list ‘ping (+ 5 initial-time)))))))

(define (pong initial-time msg)
  (add-task!
   the-task-list
   (make-task initial-time
       (lambda ()
  (newline)
  (display msg)
  (pong (+ 7 initial-time)
        (list ‘pong (+ 7 initial-time)))))))

(ping 0 “PING Starting up”)
(pong 0 “PONG Starting up”)

(run the-task-list)

PING Starting up
PONG Starting up
(ping 5)
(pong 7)
(ping 10)
(pong 14)
(ping 15)
(ping 20)
(pong 21)
(ping 25)
(pong 28)
(ping 30)
(pong 35)
(ping 35)
(ping 40)
(pong 42)
(ping 45)
(pong 49)
(ping 50)
(ping 55)
(pong 56)
…