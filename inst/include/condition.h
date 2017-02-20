#ifndef CONDITION_H_
#define CONDITION_H_

#ifdef __cplusplus
extern "C"
{
#endif

#include <Rinternals.h>

static SEXP signal_condition(const char * msg, const char * class_, SEXP env = R_GlobalEnv) {
  SEXP condition, c, signalConditionFun, out;

  const char *nms[] = {"message", ""};
  PROTECT(condition = Rf_mkNamed(VECSXP, nms));

  PROTECT(c = Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(c, 0, Rf_mkChar(class_));
  SET_STRING_ELT(c, 1, Rf_mkChar("condition"));

  SET_VECTOR_ELT(condition, 0, Rf_mkString(msg));
  Rf_setAttrib(condition, R_ClassSymbol, c);
  signalConditionFun = Rf_findFun(Rf_install("signalCondition"), R_BaseEnv);

  SEXP call = PROTECT(Rf_lang2(signalConditionFun, condition));
  PROTECT(out = Rf_eval(call, env));

  UNPROTECT(4);

  return out;
}

#ifdef __cplusplus
}
#endif

#endif /* CONDITION_H_ */
