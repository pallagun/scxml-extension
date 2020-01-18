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
               (setq back (cdr back))))))))
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
   (_binding :initform nil))
  :documentation "An instance of a state machine.")
(cl-defgeneric scxml-build-instance ((type scxml-scxml))
  "Build an scxml-instance")
(cl-defmethod scxml-build-instance ((type scxml-scxml))
  ;; TODO - validate?
  (let ((instance (scxml-instance)))
    (oset instance _type (scxml--expand-type type))
    instance))


;; (cl-defgeneric scxml--build-pseudo ((element scxml-element))
;;   (error "Create builder"))
;; (cl-defmethod scxml--build-pseudo ((element scxml-scxml))
;;   (let ((pseudo-state (scxml-pseudo-state :id (format "%s" (random)))))
;;     (scxml-put-attrib pseudo-state 'name (scxml-element-name element))
;;     pseudo-state))
;; (cl-defmethod scxml--build-pseudo ((element scxml-state))
;;   (scxml-pseudo-state :id (scxml-element-id element)))
;; (cl-defmethod scxml--build-pseudo ((element scxml-initial))
;;   (scxml-pseudo-initial))
;; (cl-defmethod scxml--build-pseudo ((element scxml-transition))
;;   (scxml-pseudo-transition :target (scxml-target-id element)
;;                            :events (scxml-events element)
;;                            :cond (scxml-cond-expr element)))

(cl-defmethod scxml--interp-cast ((element scxml-element))
  (error "Implement for this type"))
(cl-defmethod scxml--interp-cast ((element scxml-scxml))
  (let ((scxml (scxml-interp-state :id (format "%s" (random))))
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

(cl-defgeneric scxml--interp-get-initial-targets ((element scxml-element))
  nil)
(cl-defmethod scxml--interp-get-initial-targets ((element scxml-interp-state))
  (let ((initial-child (seq-filter (lambda (child) (object-of-class-p child scxml-initial))
                                   (scxml-children element))))
    (when initial-child
      (let ((transition (first (scxml-children initial-child))))
        (list (scxml-target transition))))))

(cl-defmethod scxml--expand-type ((type scxml-scxml))
  "Create a clone of TYPE for execution.

Expand 'initial' attributes and clone."
  (cl-labels ((formalize-initial-attribute
               (element source-element)
               (when (object-of-class-p source-element 'scxml-element-with-initial)
                 (let ((initial-target (scxml-element-initial source-element)))
                   (when initial-target
                     (let ((transition (scxml-transition :target initial-target))
                           (initial (scxml-initial)))
                       (scxml-add-child initial transition)
                       (scxml-add-child element initial))))))
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

(defun scxml--interp-is-history-state (any)
  "TODO - this predicate"
  nil)
(defun scxml--interp-is-compound-state (any)
  ;; TODO - might not need object-of-class here, may only need the strict class predicate.
  (and (object-of-class-p any 'scxml-interp-state)
       (seq-filter (lambda (child)
                     (object-of-class-p child 'scxml-interp-state))
                   (scxml-children any))
       t))
(defun scxml--interp-is-parallel-state (any)
  (and (object-of-class-p any 'scxml-parallel) t))

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

(defun scxml--interp-enter-states (instance states-list)
  ;; statesToEnter = new OrderedSet()
  (let ((states-to-enter)
        ;; statesForDefaultEntry = new OrderedSet()
        (states-for-default-entry)
        ;; // initialize the temporary table for default content in history states
        ;; defaultHistoryContent = new HashTable()
        (default-history-content (make-hash-table))
        STOPPED
    ;; computeEntrySet(enabledTransitions, statesToEnter, statesForDefaultEntry, defaultHistoryContent)
    ;; for s in statesToEnter.toList().sort(entryOrder):
    ;;     configuration.add(s)
    ;;     statesToInvoke.add(s)
    ;;     if binding == "late" and s.isFirstEntry:
    ;;         initializeDataModel(datamodel.s,doc.s)
    ;;         s.isFirstEntry = false
    ;;     for content in s.onentry.sort(documentOrder):
    ;;         executeContent(content)
    ;;     if statesForDefaultEntry.isMember(s):
    ;;         executeContent(s.initial.transition)
    ;;     if defaultHistoryContent[s.id]:
    ;;         executeContent(defaultHistoryContent[s.id])
    ;;     if isFinalState(s):
    ;;         if isSCXMLElement(s.parent):
    ;;             running = false
    ;;         else:
    ;;             parent = s.parent
    ;;             grandparent = parent.parent
    ;;             internalQueue.enqueue(new Event("done.state." + parent.id, s.donedata))
    ;;             if isParallelState(grandparent):
    ;;                 if getChildStates(grandparent).every(isInFinalState):
    ;;                     internalQueue.enqueue(new Event("done.state." + grandparent.id)
        )))

(defun scxml--interp-compute-entry-set (instance transitions states-to-enter states-for-default-entry default-history-content)
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
  (cl-loop for t in transitions
           (cl-loop for s in (list (scxml-target t)) ;list shim as I only support a single target now.
                    do (let ((updates (scxml--interp-add-descendant-states-to-enter s
                                                                                    states-to-enter
                                                                                    states-for-default-entry
                                                                                    default-history-content)))
                         (setq states-to-enter
                               (append (plist-get updates :states-to-enter)
                                       states-to-enter))
                         (setq states-for-default-entry
                               (append (plist-get updates :states-for-default-entry)
                                       states-for-default-entry))))
           (let ((ancestor ( get transition domain? wtf )

)))))

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
           (apply #'scxml--interp-find-lcca (cons (scxml-parent transition) tstates))))))
(defun scxml--interp-find-lcca (&rest elements)
  ;; find the lcca of all the elements.
  )
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
                         (setq states-to-enter
                               (append (plist-get updates :states-to-enter)
                                       states-to-enter))
                         (setq states-for-default-entry
                               (append (plist-get updates :states-for-default-entry)
                                       states-for-default-entry))))
           ;;for s in state.initial.transition.target:
           ;;  addAncestorStatesToEnter(s, state, statesToEnter, statesForDefaultEntry, defaultHistoryContent)
           (cl-loop for target-state in (scxml--interp-get-initial-targets state)
                    do (let ((updates (scxml--interp-add-ancestor-states-to-enter s
                                                                                  state
                                                                                  states-to-enter
                                                                                  states-for-default-entry
                                                                                  default-history-content)))
                         (setq states-to-enter
                               (append (plist-get updates :states-to-enter)
                                      states-to-enter))
                         (setq states-for-default-entry
                               (append (plist-get updates :states-for-default-entry)
                                       states-for-default-entry)))))
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
                         (setq states-to-enter
                               (append (plist-get updates :states-to-enter)
                                       states-to-enter))
                         (setq states-for-default-entry
                               (append (plist-get updates :states-for-default-entry)
                                       states-for-default-entry)))))))
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
                              (setq states-to-enter
                                    (append (plist-get updates :states-to-enter)
                                            states-to-enter))
                              (setq states-for-default-entry
                                    (append (plist-get updates :states-for-default-entry)
                                            states-for-default-entry)))))
  (list :states-to-enter states-to-enter
        :states-for-default-entry states-for-default-entry))

(defun scxml--interp-main-event-loop (instance scxml-instance)
  (with-slots ((running _running)) instance
  (when running
    (let ((enabled-transitions)
          (macrostep-done))
      (while (and running (not (macrostep-done)))
        (setq enabled-transitions
              (scxml--interpret-select-eventless-transitions instance))
        ;; stopped
        )))))



(provide 'scxml-interpreter)
;;; scxml-interpreter.el ends here
