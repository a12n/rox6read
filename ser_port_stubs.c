#include <termios.h>
#include <unistd.h>

#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

void
ser_port_setrawattr(value fd)
{
    struct termios tio;

    CAMLparam1(fd);

    if (tcgetattr(Int_val(fd), &tio) != 0) {
        caml_failwith("tcgetattr");
    }
    cfmakeraw(&tio);
    if (tcsetattr(Int_val(fd), TCSANOW, &tio) != 0) {
        caml_failwith("tcsetattr");
    }

    CAMLreturn0;
}
