#include <stdio.h>
#include <string.h>

#include "erl_nif.h"

#include "betree.h"
#include "config.h"
#include "tree.h"
#include "value.h"

// return values
static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_error;
static ERL_NIF_TERM atom_bad_expr;
static ERL_NIF_TERM atom_bad_event;

// domain types
static ERL_NIF_TERM atom_int;
static ERL_NIF_TERM atom_int_list;
static ERL_NIF_TERM atom_bin;
static ERL_NIF_TERM atom_bin_list;
static ERL_NIF_TERM atom_bool;
static ERL_NIF_TERM atom_float;
static ERL_NIF_TERM atom_frequency_caps;
static ERL_NIF_TERM atom_segments;
static ERL_NIF_TERM atom_int64;

// domain properties
static ERL_NIF_TERM atom_allow_undefined;
static ERL_NIF_TERM atom_disallow_undefined;

// values
static ERL_NIF_TERM atom_true;
static ERL_NIF_TERM atom_false;
static ERL_NIF_TERM atom_undefined;

static ErlNifResourceType* MEM_BETREE;

static ERL_NIF_TERM make_atom(ErlNifEnv* env, const char* name)
{
    ERL_NIF_TERM ret;

    if(enif_make_existing_atom(env, name, &ret, ERL_NIF_LATIN1))
        return ret;

    return enif_make_atom(env, name);
}

static void cleanup(ErlNifEnv* env, void* res)
{
    (void)env;
    struct betree** betree_res = res;
    struct betree* betree = *betree_res;
    betree_free(betree);
}

static int load(ErlNifEnv* env, void **priv_data, ERL_NIF_TERM load_info)
{
    (void)priv_data;
    (void)load_info;

    atom_ok = make_atom(env, "ok");
    atom_error = make_atom(env, "error");
    atom_bad_expr = make_atom(env, "bad_expr");
    atom_bad_event = make_atom(env, "bad_event");

    atom_int = make_atom(env, "int");;
    atom_int_list = make_atom(env, "int_list");
    atom_bin = make_atom(env, "bin");
    atom_bin_list = make_atom(env, "bin_list");
    atom_bool = make_atom(env, "bool");;
    atom_float = make_atom(env, "float");
    atom_frequency_caps = make_atom(env, "frequency_caps");
    atom_segments = make_atom(env, "segments");
    atom_int64 = make_atom(env, "int64");

    atom_allow_undefined = make_atom(env, "allow_undefined");
    atom_disallow_undefined = make_atom(env, "disallow_undefined");

    atom_true = make_atom(env, "true");
    atom_false = make_atom(env, "false");
    atom_undefined = make_atom(env, "undefined");

    int flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    MEM_BETREE = enif_open_resource_type(env, NULL, "betree", cleanup, flags, NULL);
    if (MEM_BETREE == NULL) {
        return -1;
    }

    return 0;
}

static struct betree* get_betree(ErlNifEnv* env, const ERL_NIF_TERM term)
{
    struct betree** betree_res = NULL;
    enif_get_resource(env, term, MEM_BETREE, (void**)&betree_res);
    struct betree* betree = *betree_res;
    return betree;
}

static ERL_NIF_TERM nif_betree_free(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM retval;

    if(argc != 1) {
        retval = enif_make_badarg(env);
        goto cleanup;
    }

    enif_release_resource(MEM_BETREE);

    retval = atom_ok;
cleanup:
    return retval;
}

static ERL_NIF_TERM nif_betree_make(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)env;(void)argc;(void)argv;
    ERL_NIF_TERM retval;

    struct betree** betree_res = enif_alloc_resource(MEM_BETREE, sizeof(*betree_res));

    struct betree* betree = betree_make();
    memcpy(betree_res, &betree, sizeof(*betree_res));

    ERL_NIF_TERM term = enif_make_resource(env, betree_res);

    enif_release_resource(betree_res);

    retval = enif_make_tuple(env, 2, atom_ok, term);
    return retval;
}

static char *alloc_string(ErlNifBinary bin)
{
    size_t key_len = bin.size;
    char *key = calloc(key_len + 1, sizeof(*key));
    if (!key) {
        return NULL;
    }

    memcpy(key, bin.data, key_len);

    key[key_len] = 0;

    return key;
}

static ERL_NIF_TERM nif_betree_add_domain(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM retval;
    ErlNifBinary bin;
    char* domain = NULL;
    if(argc != 2) {
        retval = enif_make_badarg(env);
        goto cleanup;
    }

    struct betree* betree = get_betree(env, argv[0]);

    if (!enif_inspect_binary(env, argv[1], &bin)) {
        retval = enif_make_badarg(env);
        goto cleanup;
    }
    domain = alloc_string(bin);
    if (domain == NULL) {
        retval = enif_make_badarg(env);
        goto cleanup;
    }

    betree_add_domain(betree, domain);
    retval = atom_ok;
cleanup:
    if(domain != NULL) {
        free(domain);
    }

    return retval;
}

#define DOMAIN_NAME_LEN 256

static bool add_domains(ErlNifEnv* env, struct betree* betree, ERL_NIF_TERM list, unsigned int list_len)
{
    ERL_NIF_TERM head;
    ERL_NIF_TERM tail = list;
    const ERL_NIF_TERM* tuple;
    int tuple_len;
    char domain_name[DOMAIN_NAME_LEN];
    bool allow_undefined;
    for(unsigned int i = 0; i < list_len; i++) {
        if(!enif_get_list_cell(env, tail, &head, &tail)) {
            return false;
        }

        if(!enif_get_tuple(env, head, &tuple_len, &tuple)) {
            return false;
        }
        if(tuple_len < 3) {
            return false;
        }

        if(!enif_get_atom(env, tuple[0], domain_name, DOMAIN_NAME_LEN, ERL_NIF_LATIN1)) {
            return false;
        }

        if(enif_is_identical(atom_allow_undefined, tuple[2])) {
            allow_undefined = true;
        }
        else if(enif_is_identical(atom_disallow_undefined, tuple[2])) {
            allow_undefined = false;
        }
        else {
            return false;
        }

        if(enif_is_identical(atom_int, tuple[1]) || enif_is_identical(atom_int64, tuple[1])) {
            if(tuple_len == 5) {
                int64_t min, max;
                if(!enif_get_int64(env, tuple[3], &min) || !enif_get_int64(env, tuple[4], &max)) {
                    return false;
                }
                add_attr_domain_bounded_i(betree->config, domain_name, allow_undefined, min, max);
            }
            else {
                add_attr_domain_i(betree->config, domain_name, allow_undefined);
            }
        }
        else if(enif_is_identical(atom_int_list, tuple[1])) {
            if(tuple_len == 5) {
                int64_t min, max;
                if(!enif_get_int64(env, tuple[3], &min) || !enif_get_int64(env, tuple[4], &max)) {
                    return false;
                }
                add_attr_domain_bounded_il(betree->config, domain_name, allow_undefined, min, max);
            }
            else {
                add_attr_domain_il(betree->config, domain_name, allow_undefined);
            }
        }
        else if(enif_is_identical(atom_bin, tuple[1])) {
            if(tuple_len == 4) {
                uint64_t max;
                if(!enif_get_uint64(env, tuple[3], &max)) {
                    return false;
                }
                add_attr_domain_bounded_s(betree->config, domain_name, allow_undefined, max);
            }
            else {
                add_attr_domain_s(betree->config, domain_name, allow_undefined);
            }
        }
        else if(enif_is_identical(atom_bin_list, tuple[1])) {
            if(tuple_len == 4) {
                uint64_t max;
                if(!enif_get_uint64(env, tuple[3], &max)) {
                    return false;
                }
                add_attr_domain_bounded_sl(betree->config, domain_name, allow_undefined, max);
            }
            else {
                add_attr_domain_sl(betree->config, domain_name, allow_undefined);
            }
        }
        else if(enif_is_identical(atom_bool, tuple[1])) {
            add_attr_domain_b(betree->config, domain_name, allow_undefined);
        }
        else if(enif_is_identical(atom_float, tuple[1])) {
            if(tuple_len == 5) {
                double min, max;
                if(!enif_get_double(env, tuple[3], &min) || !enif_get_double(env, tuple[4], &max)) {
                    return false;
                }
                add_attr_domain_bounded_f(betree->config, domain_name, allow_undefined, min, max);
            }
            else {
                add_attr_domain_f(betree->config, domain_name, allow_undefined);
            }
        }
        else if(enif_is_identical(atom_frequency_caps, tuple[1])) {
            add_attr_domain_frequency(betree->config, domain_name, allow_undefined);
        }
        else if(enif_is_identical(atom_segments, tuple[1])) {
            add_attr_domain_segments(betree->config, domain_name, allow_undefined);
        }
        else {
            return false;
        }
    }
    return true;
}

static ERL_NIF_TERM nif_betree_add_domains(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM retval;
    if(argc != 2) {
        retval = enif_make_badarg(env);
        goto cleanup;
    }

    struct betree* betree = get_betree(env, argv[0]);

    unsigned int list_len;
    if(!enif_get_list_length(env, argv[1], &list_len)) {
        retval = enif_make_badarg(env);
        goto cleanup;
    }

    ERL_NIF_TERM head;
    ERL_NIF_TERM tail = argv[1];

    for(unsigned int i = 0; i < list_len; i++) {
        if(!enif_get_list_cell(env, tail, &head, &tail)) {
            return false;
        }

        unsigned int inner_list_len;
        if(!enif_get_list_length(env, head, &inner_list_len)) {
            retval = enif_make_badarg(env);
            goto cleanup;
        }

        if(!add_domains(env, betree, head, inner_list_len)) {
            retval = enif_make_badarg(env);
            goto cleanup;
        }
    }

    retval = atom_ok;
cleanup:

    return retval;
}

static ERL_NIF_TERM nif_betree_insert(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM retval;
    ErlNifBinary bin;
    char* expr = NULL;
    if(argc != 3) {
        retval = enif_make_badarg(env);
        goto cleanup;
    }

    struct betree* betree = get_betree(env, argv[0]);

    betree_sub_t sub_id;
    if(!enif_get_uint64(env, argv[1], &sub_id)) {
        retval = enif_make_badarg(env);
        goto cleanup;
    }

    if (!enif_inspect_binary(env, argv[2], &bin)) {
        retval = enif_make_badarg(env);
        goto cleanup;
    }
    expr = alloc_string(bin);
    if (expr == NULL) {
        retval = enif_make_badarg(env);
        goto cleanup;
    }

    bool result = betree_insert(betree, sub_id, expr);
    if(result) {
        retval = atom_ok;
    }
    else {
        retval = atom_error;
    }
cleanup:
    if(expr != NULL) {
        free(expr);
    }

    return retval;
}

static bool get_binary(ErlNifEnv* env, ERL_NIF_TERM term, struct string_value* ptr)
{
    ErlNifBinary bin;

    if (!enif_inspect_binary(env, term, &bin)) {
        return false;
    }

    ptr->string = alloc_string(bin);

    return true;
}

static bool get_boolean(ErlNifEnv* env, ERL_NIF_TERM term, bool *ptr) 
{
    if(enif_is_identical(atom_true, term)) {
        *ptr = true;
        return true;
    }
    else if(enif_is_identical(atom_false, term)) {
        *ptr = false;
        return true;
    }
    return false;
}

static bool get_int(ErlNifEnv* env, ERL_NIF_TERM term, int64_t *ptr) 
{
    if(!enif_get_int64(env, term, ptr)) {
        return false;
    }
    return true;
}

static bool get_float(ErlNifEnv* env, ERL_NIF_TERM term, double *ptr) 
{
    if(!enif_get_double(env, term, ptr)) {
        return false;
    }
    return true;
}

static bool get_bin_list(ErlNifEnv* env, ERL_NIF_TERM term, struct string_list_value *ptr) 
{
    ERL_NIF_TERM head;
    ERL_NIF_TERM tail = term;
    ErlNifBinary bin;
    unsigned int length;

    if (!enif_get_list_length(env, term, &length)) {
        return false;
    }

    ptr->count = length;
    ptr->strings = calloc(length, sizeof(*ptr->strings));

    for (unsigned int i = 0; i < length; i++) {
        if(!enif_get_list_cell(env, tail, &head, &tail)) {
            return false;
        }

        if(!enif_inspect_binary(env, head, &bin)) {
            return false;
        }

        ptr->strings[i].string = alloc_string(bin);
    }

    return true;
}

static bool get_int_list(ErlNifEnv* env, ERL_NIF_TERM term, struct integer_list_value *ptr)
{
    ERL_NIF_TERM head;
    ERL_NIF_TERM tail = term;
    int64_t value;
    unsigned int length;

    if (!enif_get_list_length(env, term, &length)) {
        return false;
    }

    ptr->count = length;
    ptr->integers = calloc(length, sizeof(*ptr->integers));

    for (unsigned int i = 0; i < length; i++) {
        if(!enif_get_list_cell(env, tail, &head, &tail)) {
            return false;
        }
        if (!enif_get_int64(env, head, &value)) {
            return false;
        }

        ptr->integers[i] = value;
    }

    return true;
}

static bool get_frequency_cap(ErlNifEnv* env, ERL_NIF_TERM term, struct frequency_cap *ptr) 
{
    int cap_arity;
    int key_arity;
    ErlNifBinary bin;

    const ERL_NIF_TERM *cap_content;
    const ERL_NIF_TERM *key_content;

    if (!enif_get_tuple(env, term, &cap_arity, &cap_content) || cap_arity != 3) {
        return false;
    }

    if (!enif_get_tuple(env, cap_content[0], &key_arity, &key_content) || key_arity != 3) {
        return false;
    }

    if (!enif_inspect_binary(env, key_content[0], &bin)) {
        return false;
    }

    char* type_str = alloc_string(bin);
    enum frequency_type_e type = get_type_from_string(type_str);
    ptr->type = type;
    free(type_str);

    if (!enif_get_uint(env, key_content[1], &(ptr->id))) {
        return false;
    }

    if (!enif_inspect_binary(env, key_content[2], &bin)) {
        return false;
    }

    ptr->namespace.string = alloc_string(bin);

    if (!enif_get_uint(env, cap_content[1], &(ptr->value))) {
        return false;
    }

    if(enif_is_identical(atom_undefined, cap_content[2])) {
        ptr->timestamp_defined = false;
    }
    else if (enif_get_int64(env, cap_content[2], &(ptr->timestamp))) {
        ptr->timestamp_defined = true;
    }
    else {
        return false;
    }

    return true;
}

static bool get_frequency_caps_list(ErlNifEnv* env, ERL_NIF_TERM term, struct frequency_caps_list *ptr) 
{
    ERL_NIF_TERM head;
    ERL_NIF_TERM tail = term;
    unsigned int length;

    if (!enif_get_list_length(env, term, &length)) {
        return false;
    }

    ptr->size = length;
    ptr->content = calloc(length, sizeof(*ptr->content));

    for (unsigned int i = 0; i < ptr->size; i++) {
        if(!enif_get_list_cell(env, tail, &head, &tail)) {
            return false;
        }
        if(!get_frequency_cap(env, head, &(ptr->content[i]))) {
            return false;
        }
    }

    return true;
}

static bool get_segment(ErlNifEnv* env, ERL_NIF_TERM term, struct segment *ptr)
{
    int segment_arity;
    const ERL_NIF_TERM *segment_content;

    if (!enif_get_tuple(env, term, &segment_arity, &segment_content) || segment_arity != 2) {
        return false;
    }

    if (!enif_get_int64(env, segment_content[0], &(ptr->id))) {
        return false;
    }

    if (!enif_get_int64(env, segment_content[1], &(ptr->timestamp))) {
        return false;
    }

    return true;
}

static bool get_segments_list(ErlNifEnv* env, ERL_NIF_TERM term, struct segments_list *ptr) 
{
    ERL_NIF_TERM head;
    ERL_NIF_TERM tail = term;
    unsigned int length;

    if (!enif_get_list_length(env, term, &length)) {
        return false;
    }

    ptr->size = length;
    ptr->content = calloc(length, sizeof(*ptr->content));

    for (unsigned int i = 0; i < ptr->size; i++) {
        if(!enif_get_list_cell(env, tail, &head, &tail)) {
            return false;
        }
        if(!get_segment(env, head, &(ptr->content[i]))) {
            return false;
        }
    }

    return true;
}

static bool add_preds(ErlNifEnv* env, struct betree* betree, struct event* event, const ERL_NIF_TERM* tuple, int tuple_len, size_t initial_domain_index)
{
    // Start at 1 to not use the record name
    for(int i = 1; i < tuple_len; i++) {
        ERL_NIF_TERM element = tuple[i];
        size_t domain_index = initial_domain_index + i - 1;
        const struct attr_domain* attr_domain = betree->config->attr_domains[domain_index];
        if(attr_domain->allow_undefined) {
            if(enif_is_identical(atom_undefined, element)) {
                continue;
            }
        }
        bool result;
        struct value value;
        value.value_type = attr_domain->bound.value_type;
        switch(attr_domain->bound.value_type) {
            case VALUE_B: 
                result = get_boolean(env, element, &value.bvalue); 
                break;
            case VALUE_I:
                result = get_int(env, element, &value.ivalue);
                break;
            case VALUE_F:
                result = get_float(env, element, &value.fvalue);
                break;
            case VALUE_S:
                result = get_binary(env, element, &value.svalue);
                break;
            case VALUE_IL:
                result = get_int_list(env, element, &value.ilvalue);
                break;
            case VALUE_SL:
                result = get_bin_list(env, element, &value.slvalue);
                break;
            case VALUE_SEGMENTS:
                result = get_segments_list(env, element, &value.segments_value);
                break;
            case VALUE_FREQUENCY:
                result = get_frequency_caps_list(env, element, &value.frequency_value);
                break;
            default: 
                result = false; 
                break;
        }
        if(result == false) {
            return false;
        }
        struct pred* pred = make_pred(attr_domain->attr_var.attr, attr_domain->attr_var.var, value);
        event->preds[domain_index] = pred;
    }
    return true;
}

static ERL_NIF_TERM nif_betree_search_with_term(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM retval;
    struct report* report = NULL;
    ERL_NIF_TERM* subs = NULL;
    struct event* event = NULL;

    if(argc != 2) {
        retval = enif_make_badarg(env);
        goto cleanup;
    }

    struct betree* betree = get_betree(env, argv[0]);
    if(betree == NULL) {
        retval = enif_make_badarg(env);
        goto cleanup;
    }

    unsigned int list_len;
    if(!enif_get_list_length(env, argv[1], &list_len)) {
        retval = enif_make_badarg(env);
        goto cleanup;
    }

    event = make_event();
    event->pred_count = betree->config->attr_domain_count;
    event->preds = calloc(event->pred_count, sizeof(*event->preds));

    ERL_NIF_TERM head;
    ERL_NIF_TERM tail = argv[1];

    const ERL_NIF_TERM* tuple;
    int tuple_len;

    size_t pred_index = 0;

    for(unsigned int i = 0; i < list_len; i++) {
        if(!enif_get_list_cell(env, tail, &head, &tail)) {
            retval = enif_make_badarg(env);
            goto cleanup;
        }

        if(!enif_get_tuple(env, head, &tuple_len, &tuple)) {
            retval = enif_make_badarg(env);
            goto cleanup;
        }

        add_preds(env, betree, event, tuple, tuple_len, pred_index);
        pred_index += (tuple_len - 1);
    }

    fill_event(betree->config, event);

    report = make_report();
    betree_search_with_event(betree, event, report);

    subs = calloc(report->matched, sizeof(*subs));
    for(size_t i = 0; i < report->matched; i++) {
        subs[i] = enif_make_uint64(env, report->subs[i]);
    }
    ERL_NIF_TERM res = enif_make_list_from_array(env, subs, report->matched);
    retval = enif_make_tuple2(env, atom_ok, res);
cleanup:
    if(event != NULL) {
        free_event(event);
    }
    if(report != NULL) {
        free_report(report);
    }
    if(subs != NULL) {
        free(subs);
    }
    return retval;
}

static ERL_NIF_TERM nif_betree_search(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM retval;
    ErlNifBinary bin;
    char* event = NULL;
    struct report* report = NULL;
    ERL_NIF_TERM* subs = NULL;

    if(argc != 2) {
        retval = enif_make_badarg(env);
        goto cleanup;
    }

    struct betree* betree = get_betree(env, argv[0]);
    if(betree == NULL) {
        retval = enif_make_badarg(env);
        goto cleanup;
    }

    if (!enif_inspect_binary(env, argv[1], &bin)) {
        retval = enif_make_badarg(env);
        goto cleanup;
    }
    event = alloc_string(bin);
    if (event == NULL) {
        retval = enif_make_badarg(env);
        goto cleanup;
    }

    report = make_report();
    betree_search(betree, event, report);
    subs = calloc(report->matched, sizeof(*subs));
    for(size_t i = 0; i < report->matched; i++) {
        subs[i] = enif_make_uint64(env, report->subs[i]);
    }
    ERL_NIF_TERM res = enif_make_list_from_array(env, subs, report->matched);
    retval = enif_make_tuple2(env, atom_ok, res);
cleanup:
    if(event != NULL) {
        free(event);
    }
    if(report != NULL) {
        free_report(report);
    }
    if(subs != NULL) {
        free(subs);
    }
    return retval;
}

static ERL_NIF_TERM nif_betree_delete(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM retval;
    if(argc != 2) {
        retval = enif_make_badarg(env);
        goto cleanup;
    }

    struct betree* betree = get_betree(env, argv[0]);

    betree_sub_t sub_id;
    if(!enif_get_uint64(env, argv[1], &sub_id)) {
        retval = enif_make_badarg(env);
        goto cleanup;
    }

    betree_delete(betree, sub_id);
    retval = atom_ok;
cleanup:

    return retval;
}

static ErlNifFunc nif_functions[] = {
    {"betree_make", 0, nif_betree_make, 0},
    {"betree_free", 1, nif_betree_free, 0},
    {"betree_add_domain", 2, nif_betree_add_domain, 0},
    {"betree_add_domains", 2, nif_betree_add_domains, 0},
    {"betree_insert", 3, nif_betree_insert, 0},
    {"betree_search", 2, nif_betree_search, 0},
    {"betree_search_with_term", 2, nif_betree_search_with_term, 0},
    {"betree_delete", 2, nif_betree_delete, 0},
};

ERL_NIF_INIT(erl_betree_nif, nif_functions, &load, NULL, NULL, NULL);

