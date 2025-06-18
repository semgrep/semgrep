let src = Logs.Src.create "parallelism"

module Log = (val Logs.src_log src : Logs.LOG)
