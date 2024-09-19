object App {
    private val r = Runtime.getRuntime()
    private val command = getUnsafeCommand()

    // ruleid: tainted-obj-fields
    val process = r.exec(command)
}

val r = Runtime.getRuntime()

val command1 = getUnsafeCommand()
// ruleid: tainted-obj-fields
val process1 = r.exec(command1)

val command2 = getSafeCommand()
// OK:
val process2 = r.exec(command2)
