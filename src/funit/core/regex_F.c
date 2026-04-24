#ifndef _WIN32
/*
 * C wrapper for POSIX regex functions
 * Adapted from MAPL3 utilities/regex/regex_F.c
 * Provides interface between Fortran and system regex library
 */

#include <sys/types.h>
#include <regex.h>
#include <string.h>
#include <stdlib.h>

void C_regalloc(regex_t **preg_return) {
  *preg_return = malloc(sizeof(**preg_return));
}

/* pattern must be NUL terminated. */
void C_regcomp(regex_t *preg, const char *pattern,
               const char *flags, int *status_return) {
  int i, cflags=0;
  for (i=0;flags[i];i++) {
    switch (flags[i]) {
      case 'i': cflags |= REG_ICASE; break;
      case 'm': cflags |= REG_NEWLINE; break;
      case 'x': cflags |= REG_EXTENDED; break;
      case 'n': cflags |= REG_NOSUB; break;
      case ' ': break;
      default: *status_return=-2; return;
    }
  }
  *status_return = regcomp(preg,pattern,cflags);
}

void C_regexec(const regex_t *preg, const char *string, int nmatch,
               int matches[nmatch][2], const char *flags,
               int *status_return) {
  int i, eflags=0;
  regmatch_t *pmatch;
  for (i=0;flags[i];i++) {
    switch (flags[i]) {
      case 'b': eflags |= REG_NOTBOL; break;
      case 'e': eflags |= REG_NOTEOL; break;
      case ' ': break;
      default: *status_return=-2; return;
    }
  }
  if (nmatch>0 && sizeof(pmatch->rm_so)!=sizeof(matches[0][0])) {
    pmatch = malloc(sizeof(regmatch_t)*nmatch);
    *status_return = regexec(preg,string,nmatch,pmatch,eflags);
    for (i=0;i<nmatch;i++) {
      matches[i][0]=pmatch[i].rm_so;
      matches[i][1]=pmatch[i].rm_eo;
    }
    free(pmatch);
  } else {
    *status_return = regexec(preg,string,nmatch,(regmatch_t*)&(matches[0][0]),eflags);
  }
}

#endif /* _WIN32 */
