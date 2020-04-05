#include <caml/mlvalues.h>
#include <sys/time.h>
#include <stddef.h>

CAMLprim value useconds() {
  struct timeval t;
  gettimeofday(&t, NULL);
  return Val_int(t.tv_sec*1000000+t.tv_usec);
}
