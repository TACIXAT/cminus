; REMOVING: 
;   br label %if.end
; REMOVING: 
;   br label %if.end

; REMOVING: 
;   ret i32 57005

; ModuleID = 'out.ll'
source_filename = "cminus"

declare i32 @input()

declare i32 @output(i32)

define i32 @gcd(i32, i32) {
entry:
  %v = alloca i32
  %u = alloca i32
  store i32 %0, i32* %u
  store i32 %1, i32* %v
  %v1 = load i32, i32* %v
  %eqtmp = icmp eq i32 %v1, 0
  br i1 %eqtmp, label %if.true, label %if.false

if.true:                                          ; preds = %entry
  %u2 = load i32, i32* %u
  ret i32 %u2

if.false:                                         ; preds = %entry
  %v3 = load i32, i32* %v
  %u4 = load i32, i32* %u
  %u5 = load i32, i32* %u
  %v6 = load i32, i32* %v
  %divtmp = sdiv i32 %u5, %v6
  %v7 = load i32, i32* %v
  %multmp = mul i32 %divtmp, %v7
  %subtmp = sub i32 %u4, %multmp
  %calltmp = call i32 @gcd(i32 %v3, i32 %subtmp)
  ret i32 %calltmp
}

define i32 @main() {
entry:
  %y = alloca i32
  %x = alloca i32
  %calltmp = call i32 @input()
  store i32 %calltmp, i32* %x
  %calltmp1 = call i32 @input()
  store i32 %calltmp1, i32* %y
  %x2 = load i32, i32* %x
  %y3 = load i32, i32* %y
  %calltmp4 = call i32 @gcd(i32 %x2, i32 %y3)
  %calltmp5 = call i32 @output(i32 %calltmp4)
  ret i32 0
}
