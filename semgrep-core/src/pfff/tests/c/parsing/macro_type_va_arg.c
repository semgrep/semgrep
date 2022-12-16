static int
tbdffmt(Fmt* fmt)
{
    char *p;
    int l, r;
    uint type, tbdf;

    if((p = malloc(READSTR)) == nil)
        return fmtstrcpy(fmt, "(tbdfconv)");

    switch(fmt->r){
    case 'T':
         tbdf = va_arg(fmt->args, int);
    }

    p = va_arg(f->args, uchar*);
}
