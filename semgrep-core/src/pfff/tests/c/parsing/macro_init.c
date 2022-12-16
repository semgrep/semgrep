
#define DATASEGM(p) { 0xFFFF, SEGG|SEGB|(0xF<<16)|SEGP|SEGPL(p)|SEGDATA|SEGW }
#define EXECSEGM(p) { 0xFFFF, SEGG|SEGD|(0xF<<16)|SEGP|SEGPL(p)|SEGEXEC|SEGR }
#define TSSSEGM(b,p) { ((b)<<16)|sizeof(Tss),((b)&0xFF000000)|(((b)>>16)&0xFF)|SEGTSS|SEGPL(p)|SEGP }


#define SEG_NULL        (struct segdesc){ 0,0,0,0,0,0,0,0,0,0,0,0,0 }
