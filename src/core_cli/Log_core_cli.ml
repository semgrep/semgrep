let src = Logs.Src.create "semgrep.core_cli"

module Log = (val Logs.src_log src : Logs.LOG)
