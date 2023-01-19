int bad_code3(){
    struct NAME *var;
    var = malloc(sizeof(auth));
    free(var);
    // ruleid: use-after-free-taint
    if(var->auth){
        printf("you have logged in already");
    }
    return 0;
}
