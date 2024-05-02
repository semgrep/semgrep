let src = Logs.Src.create "semgrep.targeting"

module Log = (val Logs.src_log src : Logs.LOG)
