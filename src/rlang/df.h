#ifndef RLANG_DF_H
#define RLANG_DF_H


sexp* r_alloc_df_list(r_ssize n_rows,
                      sexp* names,
                      const enum r_type* v_types,
                      r_ssize types_size);

void r_init_data_frame(sexp* x, r_ssize n_nows);
void r_init_tibble(sexp* x, r_ssize n_rows);


#endif
