(module
  (memory (export "mem") 1)
  (data (i32.const 40) "you")
  (data (i32.const 43) ", one for me.")
  (data (i32.const 56) "One for ")

  (func (export "twoFer") (param $offset i32) (param $length i32) (result i32 i32)
    (if (i32.eqz (local.get $length)) (then
      (memory.copy (i32.const 64) (i32.const 40) (i32.const 3))
      (local.set $length (i32.const 3))))
    (memory.copy
      (i32.add (i32.const 64) (local.get $length))
      (i32.const 43)
      (i32.const 13))
    (return
      (i32.const 56)
      (i32.add (i32.const 21) (local.get $length))) 
  )
)
