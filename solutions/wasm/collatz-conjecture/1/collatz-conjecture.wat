(module
  (func (export "steps") (param $number i32) (result i32)
    (local $steps i32)
    (if
      (i32.le_s
        (local.get $number)
        (i32.const 0))
      (then
        (return
          (i32.const -1))))
    (block $block
      (loop $loop
        (br_if $block
          (i32.eq
            (local.get $number)
            (i32.const 1)))
        (if
          (i32.eq
            (i32.rem_s
              (local.get $number)
              (i32.const 2)) (i32.const 0))
          (then
            (local.set $number
              (i32.div_s
                (local.get $number)
                (i32.const 2))))
          (else
            (local.set $number
              (i32.add
                (i32.mul
                  (local.get $number)
                  (i32.const 3))
                (i32.const 1)))))
        (local.set $steps
          (i32.add
            (local.get $steps)
            (i32.const 1)))
        (br $loop)))
    (return
      (local.get $steps))
    (return (i32.const 0))
  )
)