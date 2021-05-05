static
r_obj* dict_find_node_info(struct r_dict* dict,
                           r_obj* key,
                           r_ssize* hash,
                           r_obj** parent);

static
r_obj* dict_find_node(struct r_dict* dict, r_obj* key);

static
void dict_push(struct r_dict* p_dict,
               r_ssize hash,
               r_obj* parent,
               r_obj* key,
               r_obj* value);
