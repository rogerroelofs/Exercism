//
// Delete and replace stub with your own implementation
//
// Inspired by "How it Works":
// https://indepth.dev/posts/1269/finding-fine-grained-reactive-programming#how-it-works
// https://levelup.gitconnected.com/finding-fine-grained-reactive-programming-89741994ddee?source=friends_link&sk=31c66a70c1dce7dd5f3f4229423ad127#4543
//
// and "Computations":
// https://github.com/ryansolid/solid/blob/master/documentation/reactivity.md#user-content-computations
//

/**
 * Type for the closure's value equality predicate.
 *
 * @typeParam T - Type of the values being compared for
 *              equality.
 *
 * @remarks
 * Conceptually this function should be equivalent
 * to: `lhs === rhs`
 *
 * @param lhs   - left hand side value
 * @param rhs   - right hand side value
 * @returns     - `true` if values are considered
 *                equal; `false` otherwise.
 */
type EqualFn<T> = (lhs: T, rhs: T) => boolean
type GetterFn<T> = () => T
type SetterFn<T> = (value: T) => T
type UnsubscribeFn = () => void
type UpdateFn<T> = (value?: T) => T

type InputPair<T> = [GetterFn<T>, SetterFn<T>]

type Options = {
  name: string // for debugging
}

type Observer<T> = {
  name?: string
  value?: T
  updateFn: UpdateFn<T>
}

type Subject<T> = {
  name?: string
  value: T
  observers: Set<Observer<unknown>>
  equalFn?: EqualFn<T>
}

type ComputedCell<T> = Observer<T> & Subject<T> & {
  hasChanged?: boolean
}

// module Context value
let activeObserver: Observer<unknown> | undefined
let isUpdating = false
let pendingObservers = new Set<Observer<unknown>>()

function createEqualityFn<T>(equal?: boolean | EqualFn<T>): EqualFn<T> | undefined {
  if (equal === true) return (a, b) => a === b
  if (typeof equal === 'function') return equal
  return undefined
}

function withObserverTracking<T>(fn: () => T, observer: Observer<unknown>): T {
  const prevObserver = activeObserver
  activeObserver = observer
  const result = fn()
  activeObserver = prevObserver
  return result
}

// Type-erased update function to handle mixed observer types
function updateAnyObserver(observer: Observer<unknown>): void {
  observer.value = withObserverTracking(() => observer.updateFn(observer.value), observer)
  
  // If this observer is also a subject (computed cell), notify its observers
  if ('observers' in observer && 'hasChanged' in observer) {
    const subject = observer as ComputedCell<unknown>
    const shouldNotify = subject.hasChanged !== false
    if (shouldNotify) {
      subject.observers.forEach((obs) => {
        if (isUpdating) {
          pendingObservers.add(obs)
        } else {
          updateAnyObserver(obs)
        }
      })
    }
  }
}

// Type-safe wrapper for external use
function updateObserver<T>(observer: Observer<T>): void {
  updateAnyObserver(observer as Observer<unknown>)
}

function batchUpdate(updateFn: () => void): void {
  if (isUpdating) {
    updateFn()
    return
  }
  
  isUpdating = true
  pendingObservers.clear()
  
  try {
    updateFn()
    
    // Process all pending observers until no more are added
    while (pendingObservers.size > 0) {
      const currentBatch = Array.from(pendingObservers)
      pendingObservers.clear()
      
      // Separate computed cells from callbacks
      const computedCells = currentBatch.filter(obs => 'observers' in obs)
      const callbacks = currentBatch.filter(obs => !('observers' in obs))
      
      // Update computed cells first
      computedCells.forEach(updateAnyObserver)
      
      // Then update callbacks (after all computed cells have stabilized in this batch)
      if (pendingObservers.size === 0) {
        callbacks.forEach(updateAnyObserver)
      } else {
        // If more computed cells were added, add callbacks back to pending
        callbacks.forEach(obs => pendingObservers.add(obs))
      }
    }
  } finally {
    isUpdating = false
    pendingObservers.clear()
  }
}

/**
 * Creates an input closure. The value is accessed
 * via the accessor and changed via the
 * mutator returned as part an `InputPair<T>`.
 *
 * @typeParam T   - Type of the closure's value.
 *                By extension the type of the return
 *                value of the accessor and the type
 *                of the mutator's single argument.
 *
 * @param value   - Input closure's initial value.
 * @param equal   - By default the current and previous
 *                values are not compared so invoking
 *                the mutator with identical values
 *                will trigger updates on any
 *                subscribers. When `true` is
 *                specified the
 *                {@link https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Strict_equality | strict equality operator}
 *                is used to compare values and
 *                mutations with unchanging values
 *                **are** suppressed.
 *                When `T` is a structural type
 *                it is necessary to provide a
 *                `(a: T, b: T) => boolean` comparison
 *                predicate instead.
 * @param options - Holder object for relevant options.
 *                Assigning a `name` to a subject can
 *                be useful during debugging.
 * @returns       - An `InputPair<T>`. The 1st
 *                element is the accessor (getter
 *                function), the 2nd element is
 *                the mutator (setter function).
 */
function createInput<T>(
  value: T,
  equal?: boolean | EqualFn<T>,
  options?: Options
): InputPair<T> {
  const subject: Subject<T> = {
    name: options?.name,
    observers: new Set(),
    value,
    equalFn: createEqualityFn(equal),
  }

  const read: GetterFn<T> = () => {
    if (activeObserver) subject.observers.add(activeObserver)
    return subject.value
  }

  const write: SetterFn<T> = (nextValue) => {
    if (subject.equalFn && subject.equalFn(subject.value, nextValue)) {
      return subject.value
    }
    
    batchUpdate(() => {
      subject.value = nextValue
      subject.observers.forEach(updateAnyObserver)
    })
    
    return subject.value
  }

  return [read, write]
}

/**
 * Creates a computed (derived) closure with the
 * supplied function which computes the current value
 * of the closure.
 *
 * @privateRemarks
 * `Observer<T>` may be good enough to get through
 * the enabled test case but more is needed to
 * get further ...
 *
 * @typeParam T   - Type of the closure's value.
 *                By extension the type of the value
 *                returned by the update function and
 *                of the value
 *                accepted by the function.
 *
 * @param updateFn - Update function. This function
 *                 references one or more accessors of
 *                 other subjects. It **should not**
 *                 perform side effects. It is expected
 *                 to return a value which will be the
 *                 value of the closure until the next
 *                 update. The closure's value is
 *                 supplied to this update function
 *                 on the next update.
 * @param value    - Initial value that is passed to
 *                 `updateFn` when it executes for the
 *                 first time.
 * @param equal    - By default the current and previous
 *                 values are not compared so updates
 *                 will be triggered even if the value
 *                 doesn't _change_. When `true` is
 *                 specified the
 *                 {@link https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Strict_equality | strict equality operator}
 *                 is used to compare values and updates
 *                 with identical values **are**
 *                 suppressed. When `T` is a structural
 *                 type it is necessary to provide a
 *                 `(a: T, b: T) => boolean` comparison
 *                 predicate instead.
 * @param options  - Holder object for relevant options.
 *                 Assigning a `name` to a subject can
 *                 be useful during debugging.
 * @returns        - The accessor to the closure's
 *                 value (getter function). Retrieves
 *                 the closure's current value. Used by
 *                 observers (or more accurately their
 *                 update function) to obtain the
 *                 value (and to subscribe for
 *                 updates).
 */
function createComputed<T>(
  updateFn: UpdateFn<T>,
  value?: T,
  equal?: boolean | EqualFn<T>,
  options?: { name?: string }
): GetterFn<T> {
  const equalFn = createEqualityFn(equal)

  const computed: ComputedCell<T> = {
    name: options?.name,
    value: value!,
    observers: new Set(),
    equalFn,
    hasChanged: true,
    updateFn: (prevValue?: T) => {
      const newValue = withObserverTracking(() => updateFn(prevValue), computed as Observer<unknown>)
      
      if (equalFn && prevValue !== undefined && equalFn(prevValue, newValue)) {
        computed.hasChanged = false
        return prevValue
      }
      
      computed.hasChanged = true
      return newValue
    },
  }

  // Initial computation to establish dependencies
  updateObserver(computed)

  return () => {
    if (activeObserver) computed.observers.add(activeObserver)
    return computed.value!
  }
}

/**
 * Creates a callback closure with the supplied
 * function which is expected to perform side effects.
 *
 * @privateRemarks
 * `observer` isn't mean't to be an empty object literal.
 * Replace it with something more appropriate to its
 * purpose.
 *
 * @typeParam T    - Type of the closure's value.
 *                 By extension the type of the value
 *                 returned by the callback function
 *                 and of the value accepted by the
 *                 function.
 *
 * @param updateFn - Callback function. This function
 *                 references one or more accessors of
 *                 subjects. It may perform side effects.
 *                 It will also be passed the
 *                 value that it returned the last time it
 *                 was invoked.
 * @param value    - Initial value that is passed to
 *                 `updateFn` when it executes for
 *                  the first time.
 * @returns        - The `unsubscribe` function. Once
 *                 invoked the callback closure will
 *                 stop receiving updates from the
 *                 subjects it subscribed to.
 */
function createCallback<T>(updateFn: UpdateFn<T>, value?: T): UnsubscribeFn {
  const observer: Observer<T> = {
    value,
    updateFn,
  }

  // Execute callback initially to establish dependencies
  updateObserver(observer)

  return (): void => {
    // Find and remove this observer from all subjects it's subscribed to
    // This is a simple approach - in production you'd want to track dependencies
    // more efficiently
    observer.updateFn = () => observer.value as T // Make it a no-op
  }
}

export { createInput, createComputed, createCallback }