// vim: set filetype=cpp shiftwidth=4 tabstop=4 expandtab tw=80: 

#include "erl_nif.h"


static int load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
    return 0;
}


static int reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    return load(env, priv, load_info);
}


static int upgrade(ErlNifEnv* env, void** priv, void** old_priv,
          ERL_NIF_TERM load_info)
{
    return 0;
}


static void unload(ErlNifEnv* env, void* priv)
{
    return;
}









/**
 * Define the private API.
 */

static ErlNifFunc nif_funcs[] =
{
};

/* Pass information to VM */
ERL_NIF_INIT(i18n_nif,nif_funcs,load,reload,upgrade,unload)


