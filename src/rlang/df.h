#ifndef RLANG_DF_H
#define RLANG_DF_H


r_obj* r_alloc_df_list(r_ssize n_rows,
                       r_obj* names,
                       const enum r_type* v_types,
                       r_ssize types_size);

void r_init_data_frame(r_obj* x, r_ssize n_nows);
void r_init_tibble(r_obj* x, r_ssize n_rows);


#endif
