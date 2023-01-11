
int (*_pcmspecial)(char*, char*);

int
pcmspecial(char *idstr, char *isa)
{
    return (_pcmspecial != nil)? _pcmspecial(idstr, isa): -1;
}
