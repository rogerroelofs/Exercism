(module
  (func (export "score") (param $x f32) (param $y f32) (result i32)
    (local $radius f32)
    (local $x2 f32)
    (local $y2 f32)

    (f32.mul (local.get $x) (local.get $x))
    (local.set $x2)

    (f32.mul (local.get $y) (local.get $y))
    (local.set $y2)
    
    (local.set $radius (f32.sqrt (f32.add (local.get $x2) (local.get $y2))))

    (f32.le (local.get $radius) (f32.const 1))
    (if (then
        (return (i32.const 10))
    ))
    (f32.le (local.get $radius) (f32.const 5))
    (if (then
        (return (i32.const 5))
    ))
    (f32.le (local.get $radius) (f32.const 10))
    (if (then
        (return (i32.const 1))
    ))
    (i32.const 0)
    return
  )
)
