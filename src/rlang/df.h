#ifndef RLANG_DF_H
#define RLANG_DF_H

extern sexp* r_classes_data_frame;
extern sexp* r_classes_tibble;

sexp* r_alloc_data_frame(r_ssize n_rows,
                         sexp* names,
                         enum r_type* v_types, r_ssize n_types);
void r_init_data_frame(sexp* x, r_ssize n_nows);
void r_init_tibble(sexp* x, r_ssize n_rows);


#endif
