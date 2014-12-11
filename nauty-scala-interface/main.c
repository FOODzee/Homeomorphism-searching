#include "nauty25r9/nausparse.h"

/**
 * Check whether graphs sg1 and sg2 are isomorphic.
 *
 * Should be built into .so
 * (for example by
 *
gcc -Wall -fPIC -O2  -c main.c -o obj/main.o
gcc -Wall -fPIC -O2  -c nauty25r9/nausparse.c -o obj/nauty25r9/nausparse.o
gcc -Wall -fPIC -O2  -c nauty25r9/nautil.c -o obj/nauty25r9/nautil.o
gcc -Wall -fPIC -O2  -c nauty25r9/nauty.c -o obj/nauty25r9/nauty.o
g++ -shared  obj/main.o obj/nauty25r9/nausparse.o obj/nauty25r9/nautil.o obj/nauty25r9/nauty.o \
    -o /home/foodzee/lib/libnauty-scala-interface.so -s
 *
 * )
 *
 * @author foodzee.
 */
int isomorphic(sparsegraph* sg1, sparsegraph* sg2) {
    if (sg1->nv != sg2->nv) return FALSE;

    int n = sg1->nv;

    // Some required magic.
    DYNALLSTAT(int, orbits, orbits_sz); DYNALLOC1(int, orbits, orbits_sz, n, "malloc");
    DYNALLSTAT(int, lab1, lab1_sz);     DYNALLOC1(int, lab1, lab1_sz, n, "malloc");
    DYNALLSTAT(int, lab2, lab2_sz);     DYNALLOC1(int, lab2, lab2_sz, n, "malloc");
    DYNALLSTAT(int, ptn, ptn_sz);       DYNALLOC1(int, ptn, ptn_sz, n, "malloc");
    static DEFAULTOPTIONS_SPARSEGRAPH(options);
    statsblk stats;

    // Canonical labeling of given graphs will be here.
    SG_DECL(cg1); SG_DECL(cg2);

    // Build canonical labeling.
    options.getcanon = TRUE;
    sparsenauty(sg1, lab1, ptn, orbits, &options, &stats, &cg1);
    sparsenauty(sg2, lab2, ptn, orbits, &options, &stats, &cg2);

    return aresame_sg(&cg1, &cg2);
}








/**
 * nauty is copyright (1984-2014) Brendan McKay. All rights reserved.
 * Traces is copyright (2008-2014) Adolfo Piperno. All rights reserved.
 * Permission is hereby given for use and/or distribution with the exception of sale for profit
 * or application with nontrivial military significance. You must not remove this copyright notice,
 * and you must document any changes that you make to this program.
 * This software is subject to this copyright only,
 * irrespective of any copyright attached to any package of which this is a part.
 * Absolutely no guarantees or warranties are made concerning the suitability,
 * correctness, or any other aspect of this program. Any use is at your own risk.
 */
