//fbcode/external/sigar/cpp/sigar_fileinfo.cpp

void main() {

#ifdef HAVE_READDIR_R
    while (readdir_r(dirp, &dbuf, &ent) == 0) {
        if (ent == NULL) {
            break;
        }
#else
    while ((ent = readdir(dirp))) {
#endif

   }

}
