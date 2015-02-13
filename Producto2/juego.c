/* Hello World in C, Ansi-style */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

int main(void)
{
  puts("Hola! Tratare de adivinar un numero.");
  puts("Piensa en un numero del 1 al 10.");
  sleep( 5 );
  puts("Ahora multiplicalo por 9.");
  sleep( 5 );
  puts("Si el numero tiene dos digitos, sumalos entre si. Ej. 36 -> 3+6=9. SI tu numero tiene un solo digito, sumale 0.");
  sleep( 5 );
  puts("Al numero restante sumale 4.");
  sleep( 10 );
  puts("Muy bien. EL resultado es 13 :)");
  return EXIT_SUCCESS;
}
