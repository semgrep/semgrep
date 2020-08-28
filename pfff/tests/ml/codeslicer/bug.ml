let backpatch (pos, orig) =
  let displ = (!out_position - orig) asr 2 in
  !out_buffer.[pos] <- Char.unsafe_chr displ;
  !out_buffer.[pos+1] <- Char.unsafe_chr (displ asr 8);
  !out_buffer.[pos+2] <- Char.unsafe_chr (displ asr 16);
  !out_buffer.[pos+3] <- Char.unsafe_chr (displ asr 24)
