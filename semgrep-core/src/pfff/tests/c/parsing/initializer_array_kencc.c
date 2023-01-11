static  struct
{
    char*   name;
    int flag;
    int ilval;      /* initial values */
    int irval;
} volumes[] =
{
[Vaudio]        "audio",    Fout,       50, 50,
[Vsynth]        "synth",    Fin|Fout,   0,  0,
[Vcd]       "cd",       Fin|Fout,   0,  0,
[Vline]     "line", Fin|Fout,   0,  0,
[Vmic]      "mic",  Fin|Fout|Fmono, 0,  0,
[Vspeaker]  "speaker",  Fout|Fmono, 0,  0,

[Vtreb]     "treb",     Fout,       50, 50,
[Vbass]     "bass",     Fout,       50, 50,

[Vspeed]    "speed",    Fin|Fout|Fmono, Speed,  Speed,
        0
};


char*
chanstr[32+1] = {
[1] "k1",
[2] "k2",
[4] "k4",
[8] "m8",
[16]    "r5g6b5",
[24]    "r8g8b8",
[32]    "x8r8g8b8",
};


static Type types[256] = {
    [TypeEMPTY]     { "EMPTY", "" },
    [TypeFAT12]     { "FAT12", "dos" },
};


static char isfrog[256]={
    /*NUL*/ 1, 1, 1, 1, 1, 1, 1, 1,
    /*BKS*/ 1, 1, 1, 1, 1, 1, 1, 1,
    /*DLE*/ 1, 1, 1, 1, 1, 1, 1, 1,
    /*CAN*/ 1, 1, 1, 1, 1, 1, 1, 1,
    [' ']   1,
    ['/']   1,
    [0x7f]  1,
};


Segdesc gdt[NGDT] =
{
[NULLSEG]   { 0, 0},        /* null descriptor */
[KDSEG]     DATASEGM(0),        /* kernel data/stack */
[KESEG]     EXECSEGM(0),        /* kernel code */
[UDSEG]     DATASEGM(3),        /* user data/stack */
[UESEG]     EXECSEGM(3),        /* user code */
[TSSSEG]    TSSSEGM(0,0),       /* tss segment */
/*s: [[gdt]] other elements */
[KESEG16]       EXEC16SEGM(0),  /* kernel code 16-bit */
/*e: [[gdt]] other elements */
};
