
/* Minimal main() to run a standalone Pure module compiled with -c. */

extern void __pure_main__(int argc, char** argv);

int main(int argc, char** argv)
{
  __pure_main__(argc, argv);
  return 0;
}
