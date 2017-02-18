; ModuleID = 'tiny'
source_filename = "tiny"

declare i32 @read()

declare void @write(i32)

define i32 @main() {
entry:
  %fact = alloca i32
  %x = alloca i32
  %readtmp = call i32 @read()
  store i32 %readtmp, i32* %x
  %x1 = load i32, i32* %x
  %slttmp = icmp slt i32 0, %x1
  br i1 %slttmp, label %if.true, label %if.end

if.true:                                          ; preds = %entry
  store i32 1, i32* %fact
  br label %repeat.true

repeat.true:                                      ; preds = %repeat.true, %if.true
  %fact2 = load i32, i32* %fact
  %x3 = load i32, i32* %x
  %multmp = mul i32 %fact2, %x3
  store i32 %multmp, i32* %fact
  %x4 = load i32, i32* %x
  %subtmp = sub i32 %x4, 1
  store i32 %subtmp, i32* %x
  %x5 = load i32, i32* %x
  %eqtmp = icmp eq i32 %x5, 0
  br i1 %eqtmp, label %repeat.end, label %repeat.true

repeat.end:                                       ; preds = %repeat.true
  %fact6 = load i32, i32* %fact
  call void @write(i32 %fact6)
  br label %if.end

if.end:                                           ; preds = %entry, %repeat.end
  ret i32 0
}
