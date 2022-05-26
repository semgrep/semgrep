def cp(src_path, dst_path):
    #ERROR: match
    with (open(src_path) as src, open(dst_path, mode="w") as dst, ):
        contents = src.read()
        dst.write(contents)
