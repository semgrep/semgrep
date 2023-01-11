static void
addrecover(Table t, ulong lba)
{
    if((nrtab%8) == 0) {
        rtab = realloc(rtab, (nrtab+8)*sizeof(rtab[0]));
        if(rtab == nil)
            sysfatal("out of memory");
    }
    rtab[nrtab] = (Recover){t, lba};
    nrtab++;
}


static int
rootgen(Chan *c, char *name, Dirtab*, int, int s, DirEntry *dp)
{
    int t;
    Dirtab *d;
    Dirlist *l;

    switch((int)c->qid.path){
    case Qdir:
        if(s == DEVDOTDOT){
            devdir(c, (Qid){Qdir, 0, QTDIR}, "#/", 0, eve, 0555, dp);
            return 1;
        }
    }

    r->ofcall.qid = (Qid){0, 0, QTDIR};
}
