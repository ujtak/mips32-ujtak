#include <iostream>
#include <string>
#include <cstdlib>
#include <unistd.h>
#include "mips.hpp"

using std::string;

struct config {
  string inst_file;
  string data_file;
} conf;

void usage(int exitcode)
{
  exitcode ? std::cerr : std::cout
    <<  "Usage: mipser [ -h ] <file>\n"
        "\n"
        "<file>     binary source to execute\n"
        "  -h       print this help\n"
    <<  std::endl;

  exit(exitcode);
}

void parseopt(int argc, char **argv)
{
  while (1) {
    int opt = getopt(argc, argv, "ho:");
    if (opt == -1)
      break;

    switch (opt) {
      case 'o':
        conf.data_file = optarg;
        break;
      case 'h':
        usage(0);
      default:
        usage(1);
    }
  }

  if (optind != argc - 1)
    usage(1);

  conf.inst_file = argv[argc-1];
  conf.data_file = conf.data_file.size() != 0
                 ? conf.data_file
                 : "mem_data_true.dat";
}

int main(int argc, char **argv)
{
  parseopt(argc, argv);

  MIPS cpu;

  cpu.read_inst(conf.inst_file);

  int exec_code;
  do {
    exec_code = cpu.exec_cycle();
  } while (exec_code == 0);

  cpu.write_data(conf.data_file);

  return 0;
}
