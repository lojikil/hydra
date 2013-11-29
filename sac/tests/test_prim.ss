(define (md5sum s)
    (%prim "SExp *ret = nil;
    char result[16] = {0};
    MD5_CTX hash;
    MD5_Init(&hash);
    MD5_Update(h, (void *)s->object.str, sizeof(char) * s->object.length);
    MD5_Final(&result, hash);
    return makestring(result);"))
