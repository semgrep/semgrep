let with_open_in path func =
  File.with_open_infile path (fun chan -> func Chan.{ ic = chan; p = path })
