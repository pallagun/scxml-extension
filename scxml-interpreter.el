;;; scxml-interpreter --- instantiate and run state machines -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'eieio)
(require 'scxml-element)
(require 'scxml-elements)

;; (defclass scxml-interp-element (scxml-element)
;;   ())
;; (defclass scxml-interp-state (scxml-interp-element scxml-element-with-id)
;;   ;; TODO - this shouldn't even need the 'initial' attribute.
;;   ;; at this ponit it's done by an <initial> child.
;;   ())
;; (cl-defgeneric scxml--is-compound-state-p ((element scxml-pseudo-element))
;;   nil)
;; (cl-defmethod scxml--is-compound-state-p ((element scxml-pseudo-state))
;;   (and (scxml-children element) t))
;; (defclass scxml-pseudo-transition (scxml-pseudo-element scxml-transition)
;;   ())
;; (defclass scxml-pseudo-initial (scxml-pseudo-element)
;;   ())

(defclass scxml-interp-state (scxml-state-type scxml-element-with-child-initial)
  ())
(cl-defmethod scxml-print ((interp scxml-interp-state))
  (format "%s(id:%s, children: %d)"
          (eieio-object-class interp)
          (scxml-element-id interp)
          (length (scxml-children interp))))
(defclass scxml-interp-final (scxml-interp-state)
  ())
(defclass scxml-interp-parallel (scxml-parallel)
  ())
(defclass scxml-interp-scxml (scxml-interp-state)
  ())
(defclass scxml-event ()
  ((name :initarg :name
         :type string
         :accessor scxml-name)
   (data :initarg :data
         :initform nil
         :accessor scxml-data))
  :description "Scxml event")

(defun scxml--queue ()
  "real quick, build a fifo, stolen from http://irreal.org/blog/?p=40

Usage:
  (let ((queue (scxml--queue)))
    (funcall queue 'push 'my-data)
    (funcall queue 'push 'my-data2)
    (funcall queue 'pop)"
  (let ((front) (back))
    (lambda (cmd &optional data)
      (cond ((eq cmd 'push)
             (push data front))
            ((eq cmd 'pop)
             (when (and (not back) front)
               (setq back (nreverse front))
               (setq front nil))
             (prog1
                 (car back)
               (setq back (cdr back))))
            ((eq cmd 'is-empty)
             (not (or front back)))))))
(defclass scxml-instance ()
  ((_type :type scxml-interp-state
          :documentation "The scxml state machine type to run")
   ;; The rest of these slots are based off the
   ;; "Algorithm for SCXML Interpretation right from the docs.
   (_configuration :type list
                   :initform nil)
   (_states-to-invoke :type list
                      :initform nil)
   (_internal-queue :initform (scxml--queue))
   (_external-queue :initform (scxml--queue))
   (_history-value :type hash-table
                   :initform (make-hash-table))
   (_datamodel :initform nil)
   (_running ; :type boolean?
             :initform nil)
   (_binding :type symbol
             :initform 'early
             :documentation "Must be either 'early or 'late, specification defaults to early"))
  :documentation "An instance of a state machine.")
(cl-defgeneric scxml-build-instance ((type scxml-scxml))
  "Build an scxml-instance")
(cl-defmethod scxml-build-instance ((type scxml-scxml))
  ;; TODO - validate?
  (let ((instance (scxml-instance)))
    (oset instance _type (scxml--expand-type type))
    instance))
(cl-defmethod scxml-run-instance ((instance scxml-instance))
  "Begin scxml interpretation.

procedure interpret(doc):
    if not valid(doc): failWithError()
    expandScxmlSource(doc)
    configuration = new OrderedSet()
    statesToInvoke = new OrderedSet()
    internalQueue = new Queue()
    externalQueue = new BlockingQueue()
    historyValue = new HashTable()
    datamodel = new Datamodel(doc)
    if doc.binding == \"early\":
        initializeDatamodel(datamodel, doc)
    running = true
    executeGlobalScriptElement(doc)
    enterStates([doc.initial.transition])
    mainEventLoop()"
  (oset instance _running t)
  (let ((initial-transitions (scxml--interp-get-initial-transitions (oref instance _type))))
    (scxml--interp-enter-states instance initial-transitions))
  (scxml--interp-main-event-loop instance))

(cl-defmethod scxml--interp-cast ((element scxml-element))
  (error "Implement for this type"))
(cl-defmethod scxml--interp-cast ((element scxml-scxml))
  (let ((scxml (scxml-interp-scxml :id (format "%s" (random))))
        (name (scxml-element-name element)))
    (when name
      (scxml-put-attrib scxml 'name name))
    scxml))
(cl-defmethod scxml--interp-cast ((element scxml-state))
  (scxml-interp-state :id (or (scxml-element-id element)
                              (format "%s" (random)))))
(cl-defmethod scxml--interp-cast ((element scxml-transition))
  (scxml-transition :target (scxml-target-id element)
                    :events (scxml-events element)
                    :cond (scxml-cond-expr element)))
(cl-defmethod scxml--expand-type ((type scxml-scxml))
  "Create a clone of TYPE for execution.

Expand 'initial' attributes and clone."
  (cl-labels ((place-initial
               (element target-id)
               (let ((transition (scxml-transition :target target-id))
                     (initial (scxml-initial)))
                 (scxml-add-child initial transition)
                 (scxml-add-child element initial)
                 element))
              (formalize-initial-attribute
               (element source-element)
               (let ((has-initial))
                 (when (object-of-class-p source-element 'scxml-element-with-initial)
                   (let ((initial-target (scxml-element-initial source-element)))
                     (when initial-target
                       (place-initial element initial-target)
                         (setq has-initial t))))
                 (when (and (not has-initial)
                            (object-of-class-p element 'scxml-interp-scxml))
                   ;; scxml must have an initial and it defaults to the first child.
                   (let ((first-child (first (scxml-children element))))
                     (place-initial element (scxml-element-id first-child))))))
              (expand
               (element)
               (let ((pseudo-element (scxml--interp-cast element))
                     (new-children (mapcar #'expand (scxml-children element))))
                 (when new-children
                   (mapc (lambda (child)
                           (scxml-add-child pseudo-element child))
                         (nreverse new-children)))
                 (formalize-initial-attribute pseudo-element element)
                 pseudo-element)))
    (expand type)))

(cl-defmethod scxml--interp-get-initial-transitions ((element scxml-interp-state))
  "Get the ELEMENT's intial child, then get that initial's transition set as a list."
  (let ((initial-child (find-if (lambda (child)
                                  (object-of-class-p child 'scxml-initial))
                                (scxml-children element))))
    (when initial-child
      (let ((transition (first (scxml-children initial-child))))
        (list transition)))))
(cl-defmethod scxml--interp-get-initial-targets ((element scxml-interp-state))
  "Get the ELEMENT's intial child, then get that initial's transition set as a list."
  (error "I suspect this is wrong")
  (let ((initial-child (find-if (lambda (child)
                                  (object-of-class-p child 'scxml-initial))
                                (scxml-children element))))
    (when initial-child
      (let ((transition (first (scxml-children initial-child))))
        (list (scxml-target transition))))))


(defun scxml--interp-enter-states (instance enabled-transitions)
  "Enter states indicated by transitions.

procedure enterStates(enabledTransitions):
    statesToEnter = new OrderedSet()
    statesForDefaultEntry = new OrderedSet()
    // initialize the temporary table for default content in history states
    defaultHistoryContent = new HashTable()
    computeEntrySet(enabledTransitions, statesToEnter, statesForDefaultEntry, defaultHistoryContent)
    for s in statesToEnter.toList().sort(entryOrder):
        configuration.add(s)
        statesToInvoke.add(s)
        if binding == \"late\" and s.isFirstEntry:
            initializeDataModel(datamodel.s,doc.s)
            s.isFirstEntry = false
        for content in s.onentry.sort(documentOrder):
            executeContent(content)
        if statesForDefaultEntry.isMember(s):
            executeContent(s.initial.transition)
        if defaultHistoryContent[s.id]:
            executeContent(defaultHistoryContent[s.id])
        if isFinalState(s):
            if isSCXMLElement(s.parent):
                running = false
            else:
                parent = s.parent
                grandparent = parent.parent
                internalQueue.enqueue(new Event(\"done.state.\" + parent.id, s.donedata))
                if isParallelState(grandparent):
                    if getChildStates(grandparent).every(isInFinalState):
                        internalQueue.enqueue(new Event(\"done.state.\" + grandparent.id))"
  (with-slots ((configuration _configuration)
               (states-to-invoke _states-to-invoke)
               (binding _binding)
               (root _type)
               (running _running)
               (internal-queue _internal-queue)) instance
    (let ((states-to-enter)
          (sorted-states-to-enter)        ;currently not sure if I need two variables but I'm being extra safe.
          (states-for-default-entry)
          (default-history-context (make-hash-table)))
      (let ((updates (scxml--interp-compute-entry-set enabled-transitions
                                                      states-to-enter
                                                      states-for-default-entry
                                                      default-history-context)))
        (setq states-to-enter (plist-get updates :states-to-enter))
        (setq states-for-default-entry (plist-get updates :states-for-default-entry)))

      (setq sorted-states-to-enter (copy-list states-to-enter))
      (sort sorted-states-to-enter #'scxml--interp-entry-order)
      (cl-loop for s in sorted-states-to-enter
               do (push s configuration)
               do (push s states-to-invoke)
               ;; TODO - Currently unable to handle late binding.
               when (eq binding 'late)
                 do (error "Late binding is not yet implemented")
               ;; TODO - Currently unable to handle executable content. in onentry
               ;; TODO - Currently unable to handle executable content when entering a state for default
               ;; TODO - Currently unable to handle executable content when looking at default-history-content.
               when (scxml--interp-is-final-state s)
               do (if (eq (scxml-parent s) root)
                      (setf running nil)
                    (let ((parent (scxml-parent s))
                          (grand-parent (scxml-parent parent)))
                      (funcall #'internal-queue 'push (scxml-event :name (format "done.state.%s" (scxml-element-id parent))
                                                                   :data "there sholud be state's done data here?"))
                      (when (and (scxml--interp-is-parallel-state grandparent)
                                 (every #'scxml--interp-is-in-final-state instance (scxml-children grandparent)))
                        (funcall #'internal-queue 'push (scxml-event :name (format "done.state.%s" (scxml-element-id grandparent)))))))))))
(defun scxml--interp-main-event-loop (instance)
  "Main event loop to be run after initialization & bootstapping.

procedure mainEventLoop():
    while running:
        enabledTransitions = null
        macrostepDone = false
        # Here we handle eventless transitions and transitions
        # triggered by internal events until macrostep is complete
        while running and not macrostepDone:
            enabledTransitions = selectEventlessTransitions()
            if enabledTransitions.isEmpty():
                if internalQueue.isEmpty():
                    macrostepDone = true
                else:
                    internalEvent = internalQueue.dequeue()
                    datamodel[\"_event\"] = internalEvent
                    enabledTransitions = selectTransitions(internalEvent)
            if not enabledTransitions.isEmpty():
                microstep(enabledTransitions.toList())
        # either we're in a final state, and we break out of the loop
        if not running:
            break
        # or we've completed a macrostep, so we start a new macrostep by waiting for an external event
        # Here we invoke whatever needs to be invoked. The implementation of 'invoke' is platform-specific
        for state in statesToInvoke.sort(entryOrder):
            for inv in state.invoke.sort(documentOrder):
                invoke(inv)
        statesToInvoke.clear()
        # Invoking may have raised internal error events and we iterate to handle them
        if not internalQueue.isEmpty():
            continue
        # A blocking wait for an external event.  Alternatively, if we have been invoked
        # our parent session also might cancel us.  The mechanism for this is platform specific,
        # but here we assume it’s a special event we receive
        externalEvent = externalQueue.dequeue()
        if isCancelEvent(externalEvent):
            running = false
            continue
        datamodel[\"_event\"] = externalEvent
        for state in configuration:
            for inv in state.invoke:
                if inv.invokeid == externalEvent.invokeid:
                    applyFinalize(inv, externalEvent)
                if inv.autoforward:
                    send(inv.id, externalEvent)
        enabledTransitions = selectTransitions(externalEvent)
        if not enabledTransitions.isEmpty():
            microstep(enabledTransitions.toList())
    # End of outer while running loop.  If we get here, we have reached a top-level final state or have been cancelled
    exitInterpreter()"
  (with-slots ((running _running)
               (internal-queue _internal_queue)) instance
    (while running
      (let ((enabled-transitions)
            (macrostep-done))
        (while (and running (not (macrostep-done)))
          (setq enabled-transitions
                (scxml--interp-select-eventless-transitions instance))
          (if (null enabled-transitions)
              (if (funcall internal-queue 'is-empty)
                  (setq macrostep-done t)
                (let ((internal-event (funcall internal-queue 'pop)))
                  ;; set datamodel [_event] to this event.
                  (setq enabled-transitions
                        (scxml--interp-select-transitions internal-event)))))
          (if (not (null enabled-transitions))
              (scxml--microstep enabled-transitions))


          ;; stopped
        )))))

(defun scxml--interp-microstep (instance enabled-transitions)
  "Reference:

procedure microstep(enabledTransitions):
    exitStates(enabledTransitions)
    executeTransitionContent(enabledTransitions)
    enterStates(enabledTransitions)"
  (scxml--interp-exit-states instance enabled-transitions)
  (scxml--interp-execute-transition-content enabled-transitions)
  (scxml--interp-enter-states instance enabled-transitions))

(defun scxml--interp-exit-states (instance enabled-transitions)
  "Reference:

procedure exitStates(enabledTransitions):
    statesToExit = computeExitSet(enabledTransitions)
    for s in statesToExit:
        statesToInvoke.delete(s)
    statesToExit = statesToExit.toList().sort(exitOrder)
    for s in statesToExit:
        for h in s.history:
            if h.type == \"deep\":
                f = lambda s0: isAtomicState(s0) and isDescendant(s0,s)
            else:
                f = lambda s0: s0.parent == s
            historyValue[h.id] = configuration.toList().filter(f)
    for s in statesToExit:
        for content in s.onexit.sort(documentOrder):
            executeContent(content)
        for inv in s.invoke:
            cancelInvoke(inv)
        configuration.delete(s)"
  (with-slots ((states-to-invoke _states-to-invoke)) instance
    (let ((states-to-exit (scxml--interp-compute-exit-set instance enabled-transition)))
      (cl-loop for s in states-to-exit
               do (remove s states-to-invoke))
      (setq states-to-exit (sort states-to-exit #'scxml--interp-exit-order))
      (cl-loop for s in states-to-exit
               do (cl-loop for h in nil ;; s.history - no history yet.
                           when (eq (scxml-type h) 'deep)
                           do)))))
                           HERE, right above.


(defun scxml--interp-compute-exit-set (instance transitions)
  "Reference:

function computeExitSet(transitions)
    statesToExit = new OrderedSet
    for t in transitions:
        if t.target:
            domain = getTransitionDomain(t)
            for s in configuration:
                if isDescendant(s,domain):
                    statesToExit.add(s)
    return statesToExit"
  (with-slots ((configuration _configuration)) instance
    (let ((states-to-exit))
      (cl-loop for tr in transitions
               when (scxml-target tr)
                 do (cl-loop with domain = (scxml--interp-get-transition-domain tr)
                             for s in configuration
                             when (scxml-is-descendant domain s)
                             do (push s states-to-exit)))
      states-to-exit)))

(defun scxml--interp-select-eventless-transitions (instance)
  "Grab all valid transitions with no event.

function selectEventlessTransitions():
    enabledTransitions = new OrderedSet()
    atomicStates = configuration.toList().filter(isAtomicState).sort(documentOrder)
    for state in atomicStates:
        loop: for s in [state].append(getProperAncestors(state, null)):
            for t in s.transition.sort(documentOrder):
                if not t.event and conditionMatch(t):
                    enabledTransitions.add(t)
                    break loop
    enabledTransitions = removeConflictingTransitions(enabledTransitions)
    return enabledTransitions"
  (with-slots ((configuration _configuration)) instance
  (let ((enabled-transitions)
        (atomic-states (sort (seq-filter #'scxml--interp-is-atomic-state
                                         configuration)
                             #'scxml-xml-document-order-predicate)))
    (cl-loop
     for state in atomic-states
     do (cl-loop
         named loop-enabled-transition
         for s in (cons state (scxml--interp-get-proper-ancestors state nil))

         do (cl-loop
             for tr in (sort (seq-filter (lambda (x)
                                           (object-of-class-p x 'scxml-transition))
                                         (scxml-children s))
                             #'scxml-xml-document-order-predicate)
             do (when (and (null (scxml-events tr))
                           (scxml--inter-condition-match instance tr))
                  (push tr enabled-transitions)
                  (cl-return-from loop-enabled-transition)))))
    (setq enabled-transitions
          (scxml--interp-remove-conflicting-transitions instance enabled-transitions))
    enabled-transitions)))

(defun scxml--interp-remove-conflicting-transitions (instance enabled-transitions)
  "Reference:

function removeConflictingTransitions(enabledTransitions):
    filteredTransitions = new OrderedSet()
    //toList sorts the transitions in the order of the states that selected them
    for t1 in enabledTransitions.toList():
        t1Preempted = false
        transitionsToRemove = new OrderedSet()
        for t2 in filteredTransitions.toList():
            if computeExitSet([t1]).hasIntersection(computeExitSet([t2])):
                if isDescendant(t1.source, t2.source):
                    transitionsToRemove.add(t2)
                else:
                    t1Preempted = true
                    break
        if not t1Preempted:
            for t3 in transitionsToRemove.toList():
                filteredTransitions.delete(t3)
            filteredTransitions.add(t1)

    return filteredTransitions"
  (let ((filtered-transitions))
    (cl-loop for t1 in enabled-transitions
             for t1-preempted = nil
             for transitions-to-remove = nil
             do (cl-loop for t2 in filtered-transitions
                         for exit-set-t1 = (scxml--interp-compute-exit-set t1)
                         for exit-set-t2 = (scxml--interp-compute-exit-set t2)
                         when (intersection exit-set-t1 exit-set-t2)
                         do (if (scxml-is-descendant (scxml-parent t2)
                                                     (scxml-parent t1))
                                (push t2 transitions-to-remove)
                              (setq t1-preempted t)
                              (cl-return)))
             when (not t1-preempted)
               do (progn
                    (cl-loop for t3 in transitions-to-remove
                             do (setq filtered-transitions
                                      (remove t2 filtered-transitions)))
                    (push t1 filtered-transitions)))
    filtered-transitions))

(defun scxml--interp-condition-match (instance transition)
  "Return non-nil if the transition's condition evaluates to true."
  ;; TODO - this.
  nil)

 ;; done?
(defun scxml--interp-is-state (any)
  "Reference:

state
    An element of type <state>, <parallel>, or <final>."
  (and (object-of-class-p any 'scxml-interp-state)
       ;; TODO - debatably scxml shouldn't even be a state then...
       (not (object-of-class-p any 'scxml-interp-scxml))))
(defun scxml--interp-is-history-state (any)
  "TODO - this predicate"
  nil)
(defun scxml--interp-is-compound-state (any)
  "Reference:

compound state
    A state of type <state> with at least one child state."
  (and (scxml-interp-state-p any)
       (find #'scxml--interp-is-state (scxml-children any))))
(defun scxml--interp-is-parallel-state (any)
  (and (object-of-class-p any 'scxml-interp-parallel) t))
(defun scxml--interp-is-final-state (any)
  (and (object-of-class-p any 'scxml-interp-final) t))
(defun scxml--interp-is-atomic-state (any)
  "Reference:

atomic state
    A state of type <state> with no child states, or a state of type <final>."
  (or (and (scxml-interp-state-p any)
           (not (some #'scxml--interp-is-state (scxml-children any))))
      (scxml-interp-final-p any)))
(defun scxml--interp-get-proper-ancestors (state1 state2)
  ;; From the specs:
  ;;
  ;; If state2 is null, returns the set of all ancestors of state1 in
  ;; ancestry order (state1's parent followed by the parent's parent,
  ;; etc. up to an including the <scxml> element).
  ;;
  ;; If state2 is non-null, returns in ancestry order the set of all
  ;; ancestors of state1, up to but not including state2. (A "proper
  ;; ancestor" of a state is its parent, or the parent's parent, or
  ;; the parent's parent's parent, etc.))If state2 is state1's parent,
  ;; or equal to state1, or a descendant of state1, this returns the
  ;; empty set.

  ;; TODO - pretty inefficient, but it passes tests, so there's that.
  (if state2
      (if (eq state1 state2)
          nil           ; empty - quick exit case.
        ;; return all of state1's parents up to state2
        (let ((state2-is-child-of-state1))
          (cl-block ancestors
            (scxml-visit-parents state2
                                 (lambda (parent)
                                   (when (eq parent state1)
                                     (setq state2-is-child-of-state1 t)
                                     (cl-return-from ancestors)))))
          (if state2-is-child-of-state1
              nil
            (let ((parents-reversed))
              (cl-block ancestors
                (scxml-visit-parents state1
                                     (lambda (parent)
                                       (when (eq parent state2)
                                         (cl-return-from ancestors))
                                       (push parent parents-reversed))))
              (nreverse parents-reversed)))))
    ;; return all of state1's parents.
    (let ((parents-reversed))
      (scxml-visit-parents state1
                           (lambda (parent)
                             (push parent parents-reversed)))
      (nreverse parents-reversed))))
(ert-deftest scmxl--interp-get-proper-ancestors ()
  (let ((root (scxml-interp-state :id "root"))
        (left (scxml-interp-state :id "left"))
        (left-left (scxml-interp-state :id "left-left"))
        (left-right (scxml-interp-state :id "left-right"))
        (right (scxml-interp-state :id "right")))
    (scxml-add-child root right)
    (scxml-add-child root left)
    (scxml-add-child left left-right)
    (scxml-add-child left left-left)

    ;; no limiting state.
    (should (equal (scxml--interp-get-proper-ancestors left-left nil)
                   (list left root)))
    (should (equal (scxml--interp-get-proper-ancestors left nil)
                   (list root)))
    (should (equal (scxml--interp-get-proper-ancestors root nil)
                   nil))

    ;; limiting state equal to input state
    (should-not (scxml--interp-get-proper-ancestors root root))
    (should-not (scxml--interp-get-proper-ancestors left left))
    (should-not (scxml--interp-get-proper-ancestors left-right left-right))

    ;; limiting state being direct parent of input state.
    (should-not (scxml--interp-get-proper-ancestors left-left left))
    (should-not (scxml--interp-get-proper-ancestors left-right left))
    (should-not (scxml--interp-get-proper-ancestors right root))
    (should-not (scxml--interp-get-proper-ancestors left root))

    ;; limiting state being child of input state.
    (should-not (scxml--interp-get-proper-ancestors left left-left))
    (should-not (scxml--interp-get-proper-ancestors left left-right))
    (should-not (scxml--interp-get-proper-ancestors root right))

    ;; limiting state being grandparent of input state.
    (should (equal (scxml--interp-get-proper-ancestors left-left root)
                   (list left)))
    (should (equal (scxml--interp-get-proper-ancestors left-right root)
                   (list left)))

    ;; limiting state being on a different branch of the tree.
    (should (equal (scxml--interp-get-proper-ancestors left-left right)
                   (list left root)))
    (should (equal (scxml--interp-get-proper-ancestors right left-right)
                   (list root)))))
(defun scxml--interp-entry-order (a b)
  "Sort predicate.

From the specification:
entryOrder // Ancestors precede descendants, with document order being used to break ties
    (Note:since ancestors precede descendants, this is equivalent to document order.)"

  (cond ((scxml-is-descendant a b)
         t)
        ((scxml-is-descendant b a)
         nil)
        (t
         (scxml-xml-document-order-predicate a b))))
(defun scxml--interp-exit-order (a b)
  "sort predicate.

exitOrder  // Descendants precede ancestors, with reverse document order being used to break ties
    (Note: since descendants follow ancestors, this is equivalent to reverse document order.)"
  (cond ((scxml-is-descendant b a)
         t)
        ((scxml-is-descendant a b)
         nil)
        (t
         (scxml-xml-document-order-predicate b a))))
(defun scxml--interp-is-in-final-state (instance s)
  "Return true if there's no way to get out of this state in it's current configuration.

function isInFinalState(s):
    if isCompoundState(s):
        return getChildStates(s).some(lambda s: isFinalState(s) and configuration.isMember(s))
    elif isParallelState(s):
        return getChildStates(s).every(isInFinalState)
    else:
        return false"
  (with-slots ((configuration _configuration)) instance
    (cond ((scxml--interp-is-compound-state s)
           (and (some #'scxml--interp-is-final-state (scxml-children s))
                (memq s configuration)))
          ((scxml--interp-is-parallel-state s)
           (every #'scxml--interp-is-in-final-state instance (scxml-children s)))
          (t
           nil))))
(defun scxml--interp-compute-entry-set (transitions states-to-enter states-for-default-entry default-history-content)
  "?
Specification pseudo-code:
procedure computeEntrySet(transitions, statesToEnter, statesForDefaultEntry, defaultHistoryContent)
    for t in transitions:
        for s in t.target:
            addDescendantStatesToEnter(s,statesToEnter,statesForDefaultEntry, defaultHistoryContent)
        ancestor = getTransitionDomain(t)
        for s in getEffectiveTargetStates(t)):
            addAncestorStatesToEnter(s, ancestor, statesToEnter, statesForDefaultEntry, defaultHistoryContent)"
  ;; procedure computeEntrySet(transitions, statesToEnter, statesForDefaultEntry, defaultHistoryContent)
  (cl-loop for transition in transitions
           do (cl-loop for s in (list (scxml-target transition)) ;list shim as I only support a single target now.
                       do (let ((updates (scxml--interp-add-descendant-states-to-enter s
                                                                                       states-to-enter
                                                                                       states-for-default-entry
                                                                                       default-history-content)))
                            (setq states-to-enter (plist-get updates :states-to-enter))
                            (setq states-for-default-entry (plist-get updates :states-for-default-entry))))
           do (let ((ancestor (scxml--interp-get-transition-domain transition)))
                (cl-loop for s in (scxml--interp-get-effective-target-states transition)
                         do (let ((updates (scxml--interp-add-ancestor-states-to-enter s
                                                                                       ancestor
                                                                                       states-to-enter
                                                                                       states-for-default-entry
                                                                                       default-history-content)))
                              (setq states-to-enter (plist-get updates :states-to-enter))
                              (setq states-for-default-entry (plist-get updates :states-for-default-entry))))))
  (list :states-to-enter states-to-enter
        :states-for-default-entry states-for-default-entry))
(defun scxml--interp-get-transition-domain (transition)
  "Not sure what this does just yet.

function getTransitionDomain(t)
    tstates = getEffectiveTargetStates(t)
    if not tstates:
        return null
    elif t.type == \"internal\" and isCompoundState(t.source) and tstates.every(lambda s: isDescendant(s,t.source)):
        return t.source
    else:
        return findLCCA([t.source].append(tstates))"
  (let ((tstates (scxml--interp-get-effective-target-states transition)))
    (cond ((null tstates)
           nil)
          ((and (eq (scxml-type transition) 'internal)
                (scxml--interp-is-compound-state (scxml-parent transition))
                (cl-every (lambda (s) (scxml-is-descendant (scxml-parent transition) s))
                          tstates))
           (scxml-parent-transition))
          (t
           (apply #'scxml-lcca (cons (scxml-parent transition) tstates))))))
(defun scxml--interp-get-effective-target-states (transition)
  "Not sure.

function getEffectiveTargetStates(transition)
    targets = new OrderedSet()
    for s in transition.target
        if isHistoryState(s):
            if historyValue[s.id]:
                targets.union(historyValue[s.id])
            else:
                targets.union(getEffectiveTargetStates(s.transition))
        else:
            targets.add(s)
    return targets"
  (cl-loop with targets = nil
           for s in (list (scxml-target transition)) ; wrap targets in a list as currently only a single target is supported.
           do (if (scxml--interp-is-history-state s)
                  ;; Handle history states.
                  (error "handle history states here")
                ;; no history, just push the target states.
                (push s targets))
           finally return targets))
(defun scxml--interp-add-descendant-states-to-enter (state states-to-enter states-for-default-entry default-history-content)
  "Will return (:states-to-enter states-to-enter :states-for-default-entry states-for-default-entry).

Specification pseudo code:
procedure addDescendantStatesToEnter(state,statesToEnter,statesForDefaultEntry, defaultHistoryContent):
    if isHistoryState(state):
        if historyValue[state.id]:
            for s in historyValue[state.id]:
                addDescendantStatesToEnter(s,statesToEnter,statesForDefaultEntry, defaultHistoryContent)
            for s in historyValue[state.id]:
                addAncestorStatesToEnter(s, state.parent, statesToEnter, statesForDefaultEntry, defaultHistoryContent)
        else:
            defaultHistoryContent[state.parent.id] = state.transition.content
            for s in state.transition.target:
                addDescendantStatesToEnter(s,statesToEnter,statesForDefaultEntry, defaultHistoryContent)
            for s in state.transition.target:
                addAncestorStatesToEnter(s, state.parent, statesToEnter, statesForDefaultEntry, defaultHistoryContent)
    else:
        statesToEnter.add(state)
        if isCompoundState(state):
            statesForDefaultEntry.add(state)
            for s in state.initial.transition.target:
                addDescendantStatesToEnter(s,statesToEnter,statesForDefaultEntry, defaultHistoryContent)
            for s in state.initial.transition.target:
                addAncestorStatesToEnter(s, state, statesToEnter, statesForDefaultEntry, defaultHistoryContent)
        else:
            if isParallelState(state):
                for child in getChildStates(state):
                    if not statesToEnter.some(lambda s: isDescendant(s,child)):
                        addDescendantStatesToEnter(child,statesToEnter,statesForDefaultEntry, defaultHistoryContent)"

  (if (scxml--interp-is-history-state state)
      (error "Implement this")

    ;; else: (not a history state)
    (push state states-to-enter) ;; statesToEnter.add(state)
    (cond ((scxml--interp-is-compound-state state) ;; if isCompoundState(state):
           (push state states-for-default-entry) ;; statesForDefaultEntry.add(state)
           ;;for s in state.initial.transition.target:
           ;;  addDescendantStatesToEnter(s,statesToEnter,statesForDefaultEntry, defaultHistoryContent)
           (cl-loop for target-state in (scxml--interp-get-initial-targets state)
                    do (let ((updates (scxml--interp-add-descendant-states-to-enter
                                       target-state
                                       states-to-enter
                                       states-for-default-entry
                                       default-history-content)))
                         (setq states-to-enter (plist-get updates :states-to-enter))
                         (setq states-for-default-entry (plist-get updates :states-for-default-entry))))
           ;;for s in state.initial.transition.target:
           ;;  addAncestorStatesToEnter(s, state, statesToEnter, statesForDefaultEntry, defaultHistoryContent)
           (cl-loop for target-state in (scxml--interp-get-initial-targets state)
                    do (let ((updates (scxml--interp-add-ancestor-states-to-enter s
                                                                                  state
                                                                                  states-to-enter
                                                                                  states-for-default-entry
                                                                                  default-history-content)))
                         (setq states-to-enter (plist-get updates :states-to-enter))
                         (setq states-for-default-entry (plist-get updates :states-for-default-entry)))))
          ((scxml--interp-is-parallel-state state) ;; else: if isParallelState(state):
           (cl-loop for child in (scxml-children state) ;; for child in getChildStates(state):
                    when (not (cl-find-if ;; if not statesToEnter.some(lambda s: isDescendant(s,child)):
                               (lambda (s) (scxml-is-descendant child s))
                               states-to-enter))
                    ;; addDescendantStatesToEnter(child,statesToEnter,statesForDefaultEntry, defaultHistoryContent)
                    do (let ((updates (scxml--interp-add-descendant-states-to-enter
                                       child
                                       states-to-enter
                                       states-for-default-entry
                                       default-history-content)))
                         (setq states-to-enter (plist-get updates :states-to-enter))
                         (setq states-for-default-entry (plist-get updates :states-for-default-entry)))))))
  (list :states-to-enter states-to-enter
        :states-for-default-entry states-for-default-entry))
(defun scxml--interp-add-ancestor-states-to-enter (state ancestor states-to-enter states-for-default-entry default-history-content)
  "Returns a plist of state-to-enter and statse-for-default-entry.

Specification pseudo-code:
procedure addAncestorStatesToEnter(state, ancestor, statesToEnter, statesForDefaultEntry, defaultHistoryContent)
    for anc in getProperAncestors(state,ancestor):
        statesToEnter.add(anc)
        if isParallelState(anc):
            for child in getChildStates(anc):
                if not statesToEnter.some(lambda s: isDescendant(s,child)):
                    addDescendantStatesToEnter(child,statesToEnter,statesForDefaultEntry, defaultHistoryContent)"
  ;; for anc in getProperAncestors(state,ancestor):
  (cl-loop for anc in (scxml--interp-get-proper-ancestors state ancestor)
           ;;     statesToEnter.add(anc)
           do (push anc state-to-enter)
           ;;     if isParallelState(anc):
           when (scxml--interp-is-parallel-state anc)
             ;;         for child in getChildStates(anc):
             do (cl-loop for child in (scxml-children anc)
                         ;; if not statesToEnter.some(lambda s: isDescendant(s,child)):
                         when (not (cl-find-if (lambda (s)
                                                 (scxml-is-descendant child s))
                                               states-to-enter))
                         ;; addDescendantStatesToEnter(child,statesToEnter,statesForDefaultEntry, defaultHistoryContent)
                         do (let ((updates (scxml--interp-add-descendant-state-to-enter child
                                                                                        states-to-enter
                                                                                        states-for-default-entry
                                                                                        default-history-content)))
                              (setq states-to-enter (plist-get updates :states-to-enter))
                              (setq states-for-default-entry (plist-get updates :states-for-default-entry)))))
  (list :states-to-enter states-to-enter
        :states-for-default-entry states-for-default-entry))




(provide 'scxml-interpreter)
;;; scxml-interpreter.el ends here
