#include "nauty25r9/nausparse.h"

/**
 * Check whether graphs sg1 and sg2 are isomorphic.
 */
int isomorphic(sparsegraph* sg1, sparsegraph* sg2) {
    if (sg1->nv != sg2->nv) return FALSE;

    int n = sg1->nv;

    // Some required magic
    DYNALLSTAT(int, orbits, orbits_sz); DYNALLOC1(int, orbits, orbits_sz, n, "malloc");
    DYNALLSTAT(int, lab1, lab1_sz);     DYNALLOC1(int, lab1, lab1_sz, n, "malloc");
    DYNALLSTAT(int, lab2, lab2_sz);     DYNALLOC1(int, lab2, lab2_sz, n, "malloc");
    DYNALLSTAT(int, ptn, ptn_sz);       DYNALLOC1(int, ptn, ptn_sz, n, "malloc");
    static DEFAULTOPTIONS_SPARSEGRAPH(options);
    statsblk stats;

    // Canonical labeling of given graphs will be here
    SG_DECL(cg1); SG_DECL(cg2);

    // Build canonical labeling
    options.getcanon = TRUE;
    sparsenauty(sg1, lab1, ptn, orbits, &options, &stats, &cg1);
    sparsenauty(sg2, lab2, ptn, orbits, &options, &stats, &cg2);

    return aresame_sg(&cg1, &cg2);
}
