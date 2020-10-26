#include "clib.h"
#include <alpm.h>
#include <alpm_list.h>
#include <stdio.h>
#include <string.h>

int query_community(callback_t callback) {
  alpm_errno_t err;
  alpm_handle_t *handle;
  handle = alpm_initialize("/", "/var/lib/pacman", &err);
  alpm_db_t *db =
      alpm_register_syncdb(handle, "community", ALPM_SIG_USE_DEFAULT);
  alpm_list_t *i, *pkgs = NULL;

  pkgs = alpm_db_get_pkgcache(db);

  for (i = pkgs; i; i = alpm_list_next(i)) {
    const char *name = alpm_pkg_get_name(i->data);
    const char *ver = alpm_pkg_get_version(i->data);
    if (strcmp(name, "ghc") == 0 || strcmp(name, "ghc-libs") == 0) {
      alpm_list_t *v, *provides;
      provides = alpm_pkg_get_provides(i->data);
      for (v = provides; v; v = alpm_list_next(v)) {
        alpm_depend_t *d = v->data;
        const char *d_name = d->name;
        const char *d_ver = d->version;
        callback(d_name, d_ver);
      }
    } else
      callback(name, ver);
  }
  alpm_release(handle);
  return err;
}
