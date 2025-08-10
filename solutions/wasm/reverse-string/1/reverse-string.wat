(module
  (memory (export "mem") 1)
 
  (func (export "reverseString") (param $offset i32) (param $length i32) (result i32 i32)
    (local $start i32)
    (local $end i32)
    (local $tmp i32)
    (local.set $start (local.get $offset))
    (local.set $end (i32.sub (i32.add (local.get $offset) (local.get $length)) (i32.const 1)))

    (loop $loop
      (local.set $tmp (i32.load8_s (local.get $start)))
      (i32.store8 (local.get $start) (i32.load8_s (local.get $end)))
      (i32.store8 (local.get $end) (local.get $tmp))
      (local.set $start (i32.add (local.get $start) (i32.const 1))) 
      (local.set $end (i32.sub (local.get $end) (i32.const 1))) 
      (br_if $loop (i32.lt_s (local.get $start) (local.get $end))))
    (return (local.get $offset) (local.get $length))
  )
)
